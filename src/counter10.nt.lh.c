/* ----------------------------------------------------------------------------

-----------------------------------------------------------------------------*/
#include "libhandler.h"
#include <stdio.h>  

#define unreferenced(x) ((void)x)

/*-----------------------------------------------------------------
  Define operations
-----------------------------------------------------------------*/
LH_DEFINE_EFFECT2(state, get, put)
LH_DEFINE_OP0(state, get, long)
LH_DEFINE_VOIDOP1(state, put, long)


LH_DEFINE_EFFECT1(reader, ask)
LH_DEFINE_OP0(reader, ask, long)


/*-----------------------------------------------------------------
  Benchmark
-----------------------------------------------------------------*/

lh_value bench_counter(lh_value arg) {
  unreferenced(arg);
  long count = 0;
  long i;
  while ((i = state_get()) > 0) {
    //trace_printf("counter: %i\n", i);
    state_put(i-1);
    count++;
  }
  return lh_value_long(count);
}

/*-----------------------------------------------------------------
state handler
-----------------------------------------------------------------*/

static lh_value _state_result(lh_value local, lh_value arg) {
  unreferenced(local);
  //trace_printf("state result: %i, %li\n", *((long*)local), (long)(x));
  return arg;
} 

static lh_value _state_get(lh_resume rc, lh_value local, lh_value arg) {
  unreferenced(arg);
  //trace_printf("state get: %i\n", *((long*)local));
  return lh_tail_resume(rc, local, local);
}

static lh_value _state_put(lh_resume rc, lh_value local, lh_value arg) {
  //trace_printf("state put: %i, %li\n", *((long*)local), (long)(arg));
  return lh_tail_resume(rc, arg, lh_value_null);
}

static const lh_operation _state_ops[] = {
  { LH_OP_SCOPED, LH_OPTAG(state,get), &_state_get },
  { LH_OP_SCOPED, LH_OPTAG(state,put), &_state_put },
  { LH_OP_NULL, lh_op_null, NULL }
};
static const lh_handlerdef state_def = {
  LH_EFFECT(state), NULL, NULL, &_state_result, _state_ops };

lh_value state_handle(lh_value(*action)(lh_value), long state0, lh_value arg) {
  return lh_handle(&state_def, lh_value_long(state0), action, arg);
}

/*-----------------------------------------------------------------
  reader
-----------------------------------------------------------------*/

static lh_value _reader_result(lh_value local, lh_value arg) {
  unreferenced(local);
  //trace_printf("reader result: %i, %li\n", *((long*)local), (long)(x));
  return arg;
} 

static lh_value _reader_ask(lh_resume rc, lh_value local, lh_value arg) {
  unreferenced(arg);
  //trace_printf("reader ask: %i\n", *((long*)local));
  return lh_tail_resume(rc, local, local);
}

static const lh_operation _reader_ops[] = {
  { LH_OP_TAIL_NOOP, LH_OPTAG(reader,ask), &_reader_ask },
  { LH_OP_NULL, lh_op_null, NULL }
};
static const lh_handlerdef reader_def = {
  LH_EFFECT(reader), NULL, NULL, &_reader_result, _reader_ops };

lh_value reader_handle(lh_value(*action)(lh_value), long val, lh_value arg) {
  return lh_handle(&reader_def, lh_value_long(val), action, arg);
}

/*-----------------------------------------------------------------
  Run
-----------------------------------------------------------------*/
static lh_value bench_reader10(lh_value arg) {
  return reader_handle(&bench_counter,10,arg);
}
static lh_value bench_reader9(lh_value arg) {
  return reader_handle(&bench_reader10,1,arg);
}
static lh_value bench_reader8(lh_value arg) {
  return reader_handle(&bench_reader9,1,arg);
}
static lh_value bench_reader7(lh_value arg) {
  return reader_handle(&bench_reader8,1,arg);
}
static lh_value bench_reader6(lh_value arg) {
  return reader_handle(&bench_reader7,1,arg);
}
static lh_value bench_reader5(lh_value arg) {
  return reader_handle(&bench_reader6,1,arg);
}
static lh_value bench_reader4(lh_value arg) {
  return reader_handle(&bench_reader5,1,arg);
}
static lh_value bench_reader3(lh_value arg) {
  return reader_handle(&bench_reader4,1,arg);
}
static lh_value bench_reader2(lh_value arg) {
  return reader_handle(&bench_reader3,1,arg);
}
static lh_value bench_reader1(lh_value arg) {
  return reader_handle(&bench_reader2,1,arg);
}


static void test(long count) {
  long total = 0;
  for(long i = 0; i < 100000; i++) {
    lh_value res = state_handle(&bench_reader1,count/100000,lh_value_null);
    total += lh_long_value(res);
  }
  printf("%ld\n", total);  
}


int main(int argc, char** argv) {
  test(10100100L);
}



