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
  List of lh_value's
-----------------------------------------------------------------*/

struct _bnode {
  struct _bnode* next;
  lh_value value;
};

typedef struct _bnode* blist;

#define lh_value_blist(l)  lh_value_ptr(l)
#define lh_blist_value(v)  ((blist)(lh_ptr_value(v)))


blist blist_nil = NULL;

blist blist_cons(lh_value val, blist tail) {
  blist res = (blist)malloc(sizeof(struct _bnode));
  res->next = tail;
  res->value = val;
  return res;
}

blist blist_single(lh_value val) {
  return blist_cons(val, NULL);
}

blist blist_copy(blist xs) {
  if (xs == NULL) return xs;
  return blist_cons(xs->value, blist_copy(xs->next));
}

blist blist_appendto(blist xs, blist ys)
{
  if (xs==NULL) return ys;
  blist tl = xs;
  while (tl->next != NULL) { tl = tl->next; }
  tl->next = ys; 
  return xs;
}

void blist_free(blist xs) {
  while (xs != NULL) {
    blist next = xs->next;
    free(xs);
    xs = next;
  }
}

long blist_length(blist xs) {
  long count = 0;
  while (xs != NULL) {
    count++;
    xs = xs->next;
  }
  return count;
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
    int q = lh_int_value(xs->value);
    if (queen == q || queen == q + diag || queen == q - diag) return false;    
  }
  return true;
}

static blist find_solution( int n, int col ) {
  if (col == 0) return NULL;
  blist sol = find_solution( n, col - 1);
  int queen = choice_choose( n );
  if (safe(queen,sol)) {
    return blist_cons(lh_value_int(queen),sol);
  }
  else {
    choice_fail();
    return NULL; 
  }
}

static lh_value bench_nqueens(lh_value arg) {
  int n = lh_int_value(arg);
  return lh_value_blist( find_solution(n,n) );
}

/*-----------------------------------------------------------------
 choice handler
-----------------------------------------------------------------*/

static lh_value _choice_result(lh_value local, lh_value arg) {
  unreferenced(local);  
  return lh_value_blist( blist_single(arg) );
}

static lh_value _choice_fail(lh_resume rc, lh_value local, lh_value arg) {
  unreferenced(rc);
  unreferenced(local);
  unreferenced(arg);
  return lh_value_blist( NULL );
}

static lh_value _choice_choose(lh_resume rc, lh_value local, lh_value arg) {
  unreferenced(rc);
  unreferenced(local);
  int max = lh_int_value(arg);
  blist xss = blist_nil;
  for( int i = 1; i <= max; i++) {
    blist yss = lh_blist_value( lh_scoped_resume(rc,local,lh_value_int(i)) );
    xss = blist_appendto(yss,xss); // hmm, reversed order
  }
  //lh_release(rc);
  return lh_value_blist(xss);
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

/*-----------------------------------------------------------------
 bench
-----------------------------------------------------------------*/

static void test(int n) {
  blist xss = lh_blist_value( choice_handle(&bench_nqueens,lh_value_int(n)) );
  printf("%ld\n", blist_length(xss));
}


int main(int argc, char** argv) {
  test(12);
}

