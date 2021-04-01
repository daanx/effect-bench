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
  List of void*'s
-----------------------------------------------------------------*/

struct _bnode {
  struct _bnode* next;
  void* value;
};

typedef struct _bnode* blist;

#define mpe_voidp_blist(l)  mpe_voidp_ptr(l)
#define mpe_blist_voidp(v)  ((blist)(mpe_ptr_voidp(v)))


static blist blist_nil = NULL;

static blist blist_cons(void* val, blist tail) {
  blist res = (blist)malloc(sizeof(struct _bnode));
  res->next = tail;
  res->value = val;
  return res;
}

static blist blist_single(void* val) {
  return blist_cons(val, NULL);
}

static blist blist_copy(blist xs) {
  if (xs == NULL) return xs;
  return blist_cons(xs->value, blist_copy(xs->next));
}

static blist blist_appendto(blist xs, blist ys)
{
  if (xs == NULL) return ys;
  blist tl = xs;
  while (tl->next != NULL) { tl = tl->next; }
  tl->next = ys;
  return xs;
}


static void blist_free(blist xs) {
  while (xs != NULL) {
    blist next = xs->next;
    free(xs);
    xs = next;
  }
}

static long blist_length(blist xs) {
  long count = 0;
  while (xs != NULL) {
    count++;
    xs = xs->next;
  }
  return count;
}

static void blist_print(blist xs, void (*print_elem)(void*)) {
  printf("[");
  while (xs != NULL) {
    print_elem(xs->value);
    xs = xs->next;
    if (xs != NULL) printf(",");
  }
  printf("]");
}

/*-----------------------------------------------------------------
  Benchmark
-----------------------------------------------------------------*/

/*
safe :: Int -> Int -> [Int] -> Bool
safe queen diag xs
  = case xs of
      [] -> True
      (q:qs) -> queen /= q &&
                queen /= q + diag &&
                queen /= q - diag &&
                safe queen (diag + 1) qs

findSolution :: Int -> Int -> Eff (Choose :* e) [Int]
findSolution n col 
  = if (col == 0) 
     then return [] 
     else do sol <- findSolution n (col - 1)
             queen <- perform choose n
             if (safe queen 1 sol)
               then seq sol $ return $! (queen : sol)
               else perform none ()
*/

static bool safe( int queen, blist xs ) {
  int diag = 1;
  for( int diag = 1; xs != NULL; diag++, xs = xs->next) {
    int q = mpe_int_voidp(xs->value);
    if (queen == q || queen == q + diag || queen == q - diag) return false;    
  }
  return true;
}

static blist find_solution( int n, int col ) {
  if (col == 0) return NULL;
  blist sol = find_solution( n, col - 1);
  int queen = choice_choose( n );
  if (safe(queen,sol)) {
    return blist_cons(mpe_voidp_int(queen),sol);
  }
  else {
    choice_fail();
    return NULL; 
  }
}

static void* bench_nqueens(void* arg) {
  int n = mpe_int_voidp(arg);
  return mpe_voidp_blist( find_solution(n,n) );
}

/*-----------------------------------------------------------------
 choice handler
-----------------------------------------------------------------*/

static void* _choice_result(void* local, void* arg) {
  unreferenced(local);  
  return mpe_voidp_blist( blist_single(arg) );
}

static void* _choice_fail(mpe_resume_t* rc, void* local, void* arg) {
  unreferenced(rc);
  unreferenced(local);
  unreferenced(arg);
  mpe_resume_release(rc);
  return mpe_voidp_blist( NULL );
}

static void* _choice_choose(mpe_resume_t* rc, void* local, void* arg) {
  unreferenced(rc);
  unreferenced(local);
  int max = mpe_int_voidp(arg);
  blist xss = blist_nil;
  for( int i = 1; i <= max; i++) {
    blist yss = mpe_blist_voidp( (i < max ? mpe_resume(rc,local,mpe_voidp_int(i)) : mpe_resume_final(rc,local,mpe_voidp_int(i))) );
    xss = blist_appendto(yss,xss); // hmm, reversed order
  }
  return mpe_voidp_blist(xss);
}
  

static const mpe_handlerdef_t choice_def = { MPE_EFFECT(choice), NULL, NULL, &_choice_result, {
  { MPE_OP_SCOPED, MPE_OPTAG(choice,choose), &_choice_choose },
  { MPE_OP_NEVER,  MPE_OPTAG(choice,fail), &_choice_fail },
  { MPE_OP_NULL, mpe_op_null, NULL }
}};

void* choice_handle(mpe_actionfun_t* action, void* arg) {
  return mpe_handle(&choice_def, mpe_voidp_null, action, arg);
}

/*-----------------------------------------------------------------
 bench
-----------------------------------------------------------------*/

static void test(int n) {
  blist xss = mpe_blist_voidp( choice_handle(&bench_nqueens,mpe_voidp_int(n)) );
  printf("%ld\n", blist_length(xss));
}

int main(int argc, char** argv) {
  test(12);
}

