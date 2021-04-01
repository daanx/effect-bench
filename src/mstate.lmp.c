/* ----------------------------------------------------------------------------

-----------------------------------------------------------------------------*/
#include <stdio.h>  
#include <stdlib.h>
#include <mpeff.h>

#define unreferenced(x) ((void)x)

/*-----------------------------------------------------------------
  Define operations
-----------------------------------------------------------------*/
MPE_DEFINE_EFFECT2(mstate, get, put)

MPE_DEFINE_OP0(mstate, get, long)
MPE_DEFINE_VOIDOP1(mstate, put, long)



/*-----------------------------------------------------------------
  Benchmark
-----------------------------------------------------------------*/

static void* bench_counter(void* arg) {
  unreferenced(arg);
  long count = 0;
  long i;
  while ((i = mstate_get()) > 0) {
    //trace_prlongf("counter: %i\n", i);
    mstate_put(i-1);
    count++;
  }
  return mpe_voidp_long(count);
}

/*-----------------------------------------------------------------
  functions from long -> long
-----------------------------------------------------------------*/

typedef struct function_s {
  void* env;
  long (*fun)(void*,long);
} function_t;

static inline long function_apply( function_t f, long arg ) {
  return (f.fun)(f.env,arg);
}

static function_t function_create( void* env, long (*f)(void*,long) ) {
  function_t fun = { env, f };
  return fun;
}

static function_t mpe_fun_voidp( void* v ) {
  function_t* p  = (function_t*)(mpe_ptr_voidp(v));
  function_t fun = *p;
  free(p);
  return fun;
}

static void* mpe_voidp_fun( function_t fun ) {
  function_t* p = (function_t*)malloc( sizeof(function_t) );
  if (p!=NULL) *p = fun;
  return mpe_voidp_ptr(p);
}

/*-----------------------------------------------------------------
state handler
-----------------------------------------------------------------*/

// fn(x:long){ fn(s){ x } }
static long fun_result( void* env, long st ) {
  unreferenced(env);
  return mpe_long_voidp(env);
}

static void* _state_result(void* local, void* arg) {
  unreferenced(local);
  //trace_prlongf("state result: %i, %li\n", *((long*)local), (long)(x));
  return mpe_voidp_fun( function_create(arg,&fun_result) ); 
} 

// control get(){ fn(s){ resume(s)(s) } }
static long fun_get( void* venv, long st ) {
  mpe_resume_t* rc = (mpe_resume_t*)(venv);
  function_t f = mpe_fun_voidp( mpe_resume_final( rc, NULL, mpe_voidp_long(st) ) );
  return function_apply( f, st );
}

static void* _state_get(mpe_resume_t* rc, void* local, void* arg) {
  unreferenced(arg);
  unreferenced(local);
  //trace_prlongf("state get: %i\n", *((long*)local));
  return mpe_voidp_fun( function_create( rc, &fun_get ) );
}


struct env_put_t {
  long newst;
  mpe_resume_t* rc;
};

// control put(s'){ fn(s){ resume(())(s') } }
static long fun_put( void* venv, long st ) {
  struct env_put_t env = *((struct env_put_t*)(venv));
  free(venv);
  function_t f = mpe_fun_voidp( mpe_resume_final( env.rc, NULL, NULL ) );
  return function_apply( f, env.newst );
}

static void* _state_put(mpe_resume_t* rc, void* local, void* st) {
  unreferenced(local);
  struct env_put_t* env = (struct env_put_t*)malloc( sizeof(struct env_put_t) );
  env->newst = mpe_long_voidp(st);
  env->rc = rc;
  return mpe_voidp_fun( function_create( env, &fun_put ) );
}

static const mpe_handlerdef_t state_def = { MPE_EFFECT(mstate), NULL, NULL, &_state_result, {
  { MPE_OP_ONCE, MPE_OPTAG(mstate,get), &_state_get },
  { MPE_OP_ONCE, MPE_OPTAG(mstate,put), &_state_put },
  { MPE_OP_NULL, mpe_op_null, NULL }
}};

static void* state_handle(mpe_actionfun_t action, long st, void* arg) {
  function_t f = mpe_fun_voidp( mpe_handle(&state_def, NULL, action, arg) );
  return mpe_voidp_long( (f.fun)(f.env,st) );
             //function_apply( f, st ) 
             
}

/*-----------------------------------------------------------------
  Run
-----------------------------------------------------------------*/
static void test(long count) {
  void* res1 = state_handle(&bench_counter,count,NULL);
  printf("%ld\n", mpe_long_voidp(res1));
}

int main(int argc, char** argv) {
  test(10100100L);
}

