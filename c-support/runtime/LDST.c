//
// LDST.c
//
// Supporting functions.
//

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "LDST.h"

struct RunInfo {
  bool applied0;
  int n;
  union LDST_t *args;
  union LDST_t *result;
};

static enum LDST_res_t run_impl(struct LDST_cont_t *k_then, void *run_info, union LDST_t value) {
  struct RunInfo *info = run_info;
  if (info->applied0 && info->n == 0) {
    // Store the result and continue with the given continuation.
    *info->result = value;
    return ldst_invoke(k_then, value);
  }

  struct RunInfo *new_info = malloc(sizeof(struct RunInfo));
  if (!new_info) {
    return LDST__no_mem;
  }

  struct LDST_cont_t *k_now = malloc(sizeof(struct LDST_cont_t));
  if (!k_now) {
    free(new_info);
    return LDST__no_mem;
  }

  k_now->k_lam.lam_fp = run_impl;
  k_now->k_lam.lam_closure = new_info;
  k_now->k_next = k_then;

  new_info->n        = info->n;
  new_info->args     = info->args;
  new_info->result   = info->result;
  new_info->applied0 = true;

  if (!info->applied0) {
    LDST_fp0_t fp0 = (LDST_fp0_t)value.val_lam.lam_fp;
    return fp0(k_now);
  }

  new_info->n -= 1;
  new_info->args += 1;
  struct LDST_lam_t lam = value.val_lam;
  return lam.lam_fp(k_now, lam.lam_closure, info->args[0]);
}

enum LDST_res_t ldst_run(union LDST_t *result, LDST_fp0_t f, int n, union LDST_t *args) {
  struct RunInfo *info = malloc(sizeof(struct RunInfo));
  if (!info)
    return LDST__no_mem;

  info->applied0 = false;
  info->n = n;
  info->args = args;
  info->result = result;
  struct LDST_lam_t lambda = { run_impl, info };
  union LDST_t arg0 = { .val_lam = { (LDST_fp_t)f, 0 } };
  return ldst_fork(lambda, arg0);
}

union LDST_t ldst_main(LDST_fp0_t f) {
  union LDST_t result;
  enum LDST_res_t err = ldst_run(&result, f, 0, 0);
  switch (err) {
    case LDST__ok:
      return result;
    case LDST__no_mem:
      fputs("ldst: out of memory", stderr);
      break;
    case LDST__deadlock:
      fputs("ldst: deadlocked", stderr);
      break;
    case LDST__unmatched_label:
      fputs("ldst: unmatched label", stderr);
      break;
    default:
      fputs("ldst: unknown error", stderr);
      break;
  }
  exit(err);
}

static enum LDST_res_t nat_fold_k(struct LDST_cont_t *k, void *void_closure, union LDST_t v) {
  // closure[0] = f
  // closure[1] = n
  // closure[2] = i
  // closure[3] = a
  union LDST_t *closure = void_closure;

  struct LDST_cont_t *new_k = malloc(sizeof(struct LDST_cont_t));
  if (!new_k)
    return LDST__no_mem;

  new_k->k_lam.lam_fp = ldst_nat_fold;
  new_k->k_lam.lam_closure = closure;
  new_k->k_next = k;

  union LDST_t a = closure[3];
  struct LDST_lam_t f = v.val_lam;
  return f.lam_fp(new_k, f.lam_closure, a);
}

/*
 * natrec n { z, n'. T. (a : T). e }  ~  foldr (\n' a -> e) z n
 *
 * ==> Pseudo Haskell (f == \n' a. e)
 *
 *   nat-fold f n z = go 0 z
 *     where go i a = if n == i then a else go (i+1) (f i a)
 *
 * ==> Eta reduction, explicit currying, capture lists
 *
 *   nat-fold f n = go [f n i=0]
 *   go [f n i] a = if n == i then a else go [f n i=i-1] (f i a)
 *
 * This function (`ldst_nat_fold`) directly corresponds to `go` (first
 * argument already applied) and therefore has to be called with the correct
 * closure: A list of three values in the order of `f`, `n`, `i`.
 */
enum LDST_res_t ldst_nat_fold(struct LDST_cont_t *k, void *void_closure, union LDST_t a) {
  // closure[0] = f
  // closure[1] = n
  // closure[2] = i
  union LDST_t *closure = void_closure;
  int n = closure[1].val_int;
  int i = closure[2].val_int;

  if (n == i)
    return ldst_invoke(k, a);

  // The closure is reused both for the continuation and in there (see
  // `nat_fold_k`) it is also used as the closure to the recusive call to
  // `ldst_nat_fold` (this function).
  union LDST_t *new_closure = malloc(4 * sizeof(union LDST_t));
  if (!new_closure)
    return LDST__no_mem;

  struct LDST_cont_t *new_k = malloc(sizeof(struct LDST_cont_t));
  if (!new_k) {
    free(new_closure);
    return LDST__no_mem;
  }

  struct LDST_lam_t f = closure[0].val_lam;
  new_closure[0].val_lam = f;
  new_closure[1].val_int = n;
  new_closure[2].val_int = i + 1;
  new_closure[3] = a;

  new_k->k_lam.lam_fp = nat_fold_k;
  new_k->k_lam.lam_closure = new_closure;
  new_k->k_next = k;

  union LDST_t idx = { .val_int = i };
  return f.lam_fp(new_k, f.lam_closure, idx);
}

enum LDST_res_t ldst_make_recv_result(struct LDST_chan_t *chan, union LDST_t value, union LDST_t *result) {
  union LDST_t *received_pair = malloc(2 * sizeof(union LDST_t));
  if (!received_pair)
    return LDST__no_mem;

  received_pair[0] = value;
  received_pair[1].val_chan = chan;
  result->val_pair = received_pair;
  return LDST__ok;
}
