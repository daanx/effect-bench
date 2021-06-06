/* ----------------------------------------------------------------------------

-----------------------------------------------------------------------------*/
#include <stdio.h>  
#include <stdlib.h>
#include <mpeff.h>

#define unreferenced(x) ((void)x)

/*-----------------------------------------------------------------
  Define operations
-----------------------------------------------------------------*/
MPE_DEFINE_EFFECT2(choice, choose, fail)
MPE_DEFINE_OP1(choice, choose, int, int)
MPE_DEFINE_VOIDOP0(choice, fail)

/*-----------------------------------------------------------------
  Define operations
-----------------------------------------------------------------*/
MPE_DEFINE_EFFECT1(yield, yield)
MPE_DEFINE_VOIDOP1(yield, yield, long)
 

/*-----------------------------------------------------------------
  Benchmark
-----------------------------------------------------------------*/

static void triples( long n, long s ) {
  long x = choice_choose(n);
  long y = choice_choose(x-1);
  long z = choice_choose(y-1);
  if (x+y+z == s) yield_yield(x);
             else choice_fail();
}

static void* do_triples(void* arg) {
  long l = mpe_long_voidp(arg);
  long n = (l >> 16);
  long s = (l & 0xFFFF);
  triples(n,s);
  return mpe_voidp_int(0);
}

/*-----------------------------------------------------------------
 choice handler
-----------------------------------------------------------------*/

static void* _choice_result(void* local, void* arg) {
  unreferenced(local);  
  return arg;
}

static void* _choice_fail(mpe_resume_t* rc, void* local, void* arg) {
  unreferenced(local);
  unreferenced(arg);  
  mpe_resume_release(rc);
  return mpe_voidp_int(0);
}

static void* _choice_choose(mpe_resume_t* rc, void* local, void* arg) {
  unreferenced(local);
  int n = mpe_int_voidp(arg);  
  for( int i = 1; i <= n; i++) {
    if (i!=n) mpe_resume(rc,local,mpe_voidp_int(i));
         else mpe_resume_final(rc,local,mpe_voidp_int(i));
  }
  //mpe_release(rc);
  return mpe_voidp_int(0);
}
  


static const mpe_handlerdef_t choice_def = { MPE_EFFECT(choice), &_choice_result, {
  { MPE_OP_SCOPED, MPE_OPTAG(choice,choose), &_choice_choose },
  { MPE_OP_ABORT, MPE_OPTAG(choice,fail), &_choice_fail },
  { MPE_OP_NULL, mpe_op_null, NULL }
} };

void* choice_handle(void*(*action)(void*), void* arg) {
  return mpe_handle(&choice_def, mpe_voidp_null, action, arg);
}



static void* _yield_result(void* local, void* arg) {
  unreferenced(arg);
  //trace_printf("reader result: %i, %li\n", *((long*)local), (long)(x));
  return local;
} 

static void* _yield_yield(mpe_resume_t* rc, void* local, void* arg) {
  unreferenced(arg);
  //trace_printf("reader ask: %i\n", *((long*)local));
  return mpe_resume_tail(rc, mpe_voidp_long( mpe_long_voidp(local)+1 ), local);
}

static const mpe_handlerdef_t yield_def = { MPE_EFFECT(yield),  &_yield_result, {
  { MPE_OP_TAIL_NOOP, MPE_OPTAG(yield,yield), &_yield_yield },
  { MPE_OP_NULL, mpe_op_null, NULL }
} };

void* yield_handle(void*(*action)(void*), long val, void* arg) {
  return mpe_handle(&yield_def, mpe_voidp_long(val), action, arg);
}

/*-----------------------------------------------------------------
 bench
-----------------------------------------------------------------*/

static void* do_choose_triples( void* arg ) {
  return choice_handle( &do_triples, arg );
}

static void test(int n, int s) {
  long count = mpe_long_voidp( yield_handle( &do_choose_triples, 0, mpe_voidp_long(n << 16 | s) ) );
  printf("%ld\n", count);
}


int main(int argc, char** argv) {
  test(500,127);
}

