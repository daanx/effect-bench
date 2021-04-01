/* ----------------------------------------------------------------------------
 
-----------------------------------------------------------------------------*/
#include <mpeff.h>
#include <stdio.h>  
#include <stdint.h>

#define unreferenced(x) ((void)x)

/*-----------------------------------------------------------------
  Define operations
-----------------------------------------------------------------*/
MPE_DEFINE_EFFECT2(state, get, put)
MPE_DEFINE_OP0(state, get, long)
MPE_DEFINE_VOIDOP1(state, put, long)


MPE_DEFINE_EFFECT1(reader, ask)
MPE_DEFINE_OP0(reader, ask, long)

/*-----------------------------------------------------------------
  Benchmark
-----------------------------------------------------------------*/

void* bench_counter(void* arg) {
  unreferenced(arg);
  long count = 0;
  long i;
  while ((i = state_get()) > 0) {
    //trace_printf("counter: %i\n", i);
    state_put(i-1);
    count++;
  }
  return mpe_voidp_long(count);
}

/*-----------------------------------------------------------------
state handler
-----------------------------------------------------------------*/

static void* _state_get(mpe_resume_t* rc, void* local, void* arg) {
  unreferenced(arg);
  //trace_printf("state get: %i\n", *((long*)local));
  return mpe_resume_tail(rc, local, local);
}

static void* _state_put(mpe_resume_t* rc, void* local, void* arg) {
  //trace_printf("state put: %i, %li\n", *((long*)local), (long)(arg));
  return mpe_resume_tail(rc, arg, NULL);
}

static const mpe_handlerdef_t state_def = { MPE_EFFECT(state), NULL, NULL, NULL, {
  { MPE_OP_TAIL_NOOP, MPE_OPTAG(state,get), &_state_get },
  { MPE_OP_TAIL_NOOP, MPE_OPTAG(state,put), &_state_put },
  { MPE_OP_NULL, mpe_op_null, NULL } 
}};

void* state_handle(mpe_actionfun_t* action, long state0, void* arg) {
  return mpe_handle(&state_def, mpe_voidp_long(state0), action, arg);
}


/*-----------------------------------------------------------------
  reader
-----------------------------------------------------------------*/

static void* _reader_ask(mpe_resume_t* rc, void* local, void* arg) {
  unreferenced(arg);
  //trace_printf("reader ask: %i\n", *((long*)local));
  return mpe_resume_tail(rc, local, local);
}

static const mpe_handlerdef_t reader_def = { MPE_EFFECT(reader), NULL, NULL, NULL, {
  { MPE_OP_TAIL_NOOP, MPE_OPTAG(reader,ask), &_reader_ask },
  { MPE_OP_NULL, mpe_op_null, NULL }
}};

void* reader_handle(mpe_actionfun_t action, long val, void* arg) {
  return mpe_handle(&reader_def, mpe_voidp_long(val), action, arg);
}


/*-----------------------------------------------------------------
  Run
-----------------------------------------------------------------*/
static void* bench_reader1(void* arg) {
  return reader_handle(&bench_counter,1,arg);
}

static void test(long count) {
  void* res1 = state_handle(&bench_reader1,count,NULL);
  printf("%ld\n", mpe_long_voidp(res1));
}

int main(int argc, char** argv) {
  test(100100100L);
}

