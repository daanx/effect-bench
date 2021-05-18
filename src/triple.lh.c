/* ----------------------------------------------------------------------------

-----------------------------------------------------------------------------*/
#include "libhandler.h"
#include <stdio.h>  
#include <stdlib.h>

#define unreferenced(x) ((void)x)

/*-----------------------------------------------------------------
  Define operations
-----------------------------------------------------------------*/
LH_DEFINE_EFFECT2(choice, choose, fail)
LH_DEFINE_OP1(choice, choose, int, int)
LH_DEFINE_VOIDOP0(choice, fail)

/*-----------------------------------------------------------------
  Define operations
-----------------------------------------------------------------*/
LH_DEFINE_EFFECT1(yield, yield)
LH_DEFINE_VOIDOP1(yield, yield, long)


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

static lh_value do_triples(lh_value arg) {
  long l = lh_long_value(arg);
  long n = (l >> 16);
  long s = (l & 0xFFFF);
  triples(n,s);
  return lh_value_int(0);
}

/*-----------------------------------------------------------------
 choice handler
-----------------------------------------------------------------*/

static lh_value _choice_result(lh_value local, lh_value arg) {
  unreferenced(local);  
  return arg;
}

static lh_value _choice_fail(lh_resume rc, lh_value local, lh_value arg) {
  unreferenced(rc);
  unreferenced(local);
  unreferenced(arg);
  return lh_value_int(0);
}

static lh_value _choice_choose(lh_resume rc, lh_value local, lh_value arg) {
  unreferenced(rc);
  unreferenced(local);
  int n = lh_int_value(arg);  
  for( int i = 1; i <= n; i++) {
    lh_scoped_resume(rc,local,lh_value_int(i));
  }
  //lh_release(rc);
  return lh_value_int(0);
}
  

static const lh_operation _choice_ops[] = {
  { LH_OP_SCOPED, LH_OPTAG(choice,choose), &_choice_choose },
  { LH_OP_NORESUME, LH_OPTAG(choice,fail), &_choice_fail },
  { LH_OP_NULL, lh_op_null, NULL }
};
static const lh_handlerdef choice_def = { LH_EFFECT(choice), NULL, NULL, &_choice_result, _choice_ops };

lh_value choice_handle(lh_value(*action)(lh_value), lh_value arg) {
  return lh_handle(&choice_def, lh_value_null, action, arg);
}



static lh_value _yield_result(lh_value local, lh_value arg) {
  unreferenced(arg);
  //trace_printf("reader result: %i, %li\n", *((long*)local), (long)(x));
  return local;
} 

static lh_value _yield_yield(lh_resume rc, lh_value local, lh_value arg) {
  unreferenced(arg);
  //trace_printf("reader ask: %i\n", *((long*)local));
  return lh_tail_resume(rc, lh_value_long( lh_long_value(local)+1 ), local);
}

static const lh_operation _yield_ops[] = {
  { LH_OP_TAIL_NOOP, LH_OPTAG(yield,yield), &_yield_yield },
  { LH_OP_NULL, lh_op_null, NULL }
};
static const lh_handlerdef yield_def = {
  LH_EFFECT(yield), NULL, NULL, &_yield_result, _yield_ops };

lh_value yield_handle(lh_value(*action)(lh_value), long val, lh_value arg) {
  return lh_handle(&yield_def, lh_value_long(val), action, arg);
}

/*-----------------------------------------------------------------
 bench
-----------------------------------------------------------------*/

static lh_value do_choose_triples( lh_value arg ) {
  return choice_handle( &do_triples, arg );
}

static void test(int n, int s) {
  long count = lh_long_value( yield_handle( &do_choose_triples, 0, lh_value_long(n << 16 | s) ) );
  printf("%ld\n", count);
}


int main(int argc, char** argv) {
  test(500,127);
}

