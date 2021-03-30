/* ----------------------------------------------------------------------------

-----------------------------------------------------------------------------*/
#include "libhandler.h"
#include <stdio.h>  
#include <stdlib.h>

#define unreferenced(x) ((void)x)

/*-----------------------------------------------------------------
  Define operations
-----------------------------------------------------------------*/
LH_DEFINE_EFFECT2(state, get, put)

LH_DEFINE_OP0(state, get, long)
LH_DEFINE_VOIDOP1(state, put, long)



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
  functions from int -> int
-----------------------------------------------------------------*/

typedef struct function_s {
  void* env;
  int (*fun)(void*,int);
} function_t;

static int function_apply( function_t f, int arg ) {
  return (f.fun)(f.env,arg);
}

static function_t function_create( void* env, int (*f)(void*,int) ) {
  function_t fun = { env, f };
  return fun;
}

static function_t lh_fun_value( lh_value v ) {
  function_t* p  = (function_t*)(lh_ptr_value(v));
  function_t fun = *p;
  free(p);
  return fun;
}

static lh_value lh_value_fun( function_t fun ) {
  function_t* p = (function_t*)malloc( sizeof(function_t) );
  *p = fun;
  return lh_value_ptr(p);
}

/*-----------------------------------------------------------------
state handler
-----------------------------------------------------------------*/

static int fun_result( void* env, int st ) {
  unreferenced(env);
  return (int)((intptr_t)(env));
}

static lh_value _state_result(lh_value local, lh_value arg) {
  unreferenced(local);
  //trace_printf("state result: %i, %li\n", *((long*)local), (long)(x));
  return lh_value_fun( function_create((void*)((intptr_t)(lh_int_value(arg))),&fun_result) ); 
} 


static int fun_get( void* venv, int st ) {
  lh_resume rc = (lh_resume)(venv);
  function_t f = lh_fun_value( lh_release_resume( rc, lh_value_null, lh_value_int(st) ) );
  return function_apply( f, st );
}

static lh_value _state_get(lh_resume rc, lh_value local, lh_value arg) {
  unreferenced(arg);
  unreferenced(local);
  //trace_printf("state get: %i\n", *((long*)local));
  return lh_value_fun( function_create( rc, &fun_get ) );
}


struct env_put_t {
  int newst;
  lh_resume rc;
};

static int fun_put( void* venv, int st ) {
  struct env_put_t env = *((struct env_put_t*)(venv));
  free(venv);
  function_t f = lh_fun_value( lh_release_resume( env.rc, lh_value_null, lh_value_null ) );
  return function_apply( f, env.newst );
}

static lh_value _state_put(lh_resume rc, lh_value local, lh_value st) {
  unreferenced(local);
  struct env_put_t* env = (struct env_put_t*)malloc( sizeof(struct env_put_t) );
  env->newst = lh_int_value(st);
  env->rc = rc;  
  return lh_value_fun( function_create( env, &fun_put ) );
}

static const lh_operation _state_ops[] = {
  { LH_OP_GENERAL, LH_OPTAG(state,get), &_state_get },
  { LH_OP_GENERAL, LH_OPTAG(state,put), &_state_put },
  { LH_OP_NULL, lh_op_null, NULL }
};
static const lh_handlerdef state_def = {
  LH_EFFECT(state), NULL, NULL, &_state_result, _state_ops };

lh_value state_handle(lh_value(*action)(lh_value), int st, lh_value arg) {
  function_t f = lh_fun_value( lh_handle(&state_def, lh_value_null, action, arg) );
  return lh_value_int( function_apply( f, st ) );
}

/*-----------------------------------------------------------------
  Run
-----------------------------------------------------------------*/
static void test(long count) {
  lh_value res1 = state_handle(&bench_counter,count,lh_value_null);
  printf("%d\n", lh_int_value(res1));
}


int main(int argc, char** argv) {
  test(10100100L);
}

