//
// LDST.c
//
// Supporting functions.
//

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "LDST.h"

struct SyncInfo {
  LDST_t *result;
  union {
    LDST_lam_t op;
    bool has_result;
  };
};

static LDST_res_t assign_k(LDST_cont_t *k, LDST_ctxt_t *ctxt, void *vinfo, LDST_t val) {
  struct SyncInfo *info = vinfo;
  *info->result = val;
  info->has_result = true;
  return LDST_invoke(k, ctxt, val);
}

static LDST_res_t assign(LDST_cont_t *then_k, LDST_ctxt_t *ctxt, void *vinfo, LDST_t arg) {
  LDST_cont_t *k = malloc(sizeof(LDST_cont_t));
  if (!k)
    return LDST_NO_MEM;

  struct SyncInfo *info = vinfo;
  LDST_lam_t op = info->op;
  info->has_result = false;
  k->k_lam.lam_fp = assign_k;
  k->k_lam.lam_closure = info->result;
  k->k_next = then_k;
  return op.lam_fp(k, ctxt, op.lam_closure, arg);
}

LDST_res_t LDST_sync(LDST_ctxt_t *ctxt, LDST_t *result, LDST_lam_t op, LDST_t arg) {
  LDST_res_t res;
  struct SyncInfo info = { result, { .op = op } };
  LDST_lam_t assign_op = { assign, &info };

  res = LDST_fork(ctxt, assign_op, arg);
  if (res != LDST_OK)
    return res;

  res = LDST_context_wait(ctxt);
  if (res != LDST_OK)
    return res;

  return info.has_result ? LDST_OK : LDST_NO_RESULT;
}

struct RunInfo {
  int n;
  LDST_t *args;
};

static LDST_res_t run_impl(LDST_cont_t *k_then, LDST_ctxt_t *ctxt, void *run_info, LDST_t value) {
  struct RunInfo *info = run_info;
  LDST_cont_t *k_now = k_then;

  if (info->n > 1) {
    // Create a new continuation to apply the remaining arguments is required.
    struct RunInfo *new_info = malloc(sizeof *new_info);
    if (!new_info) {
      return LDST_NO_MEM;
    }

    k_now = malloc(sizeof(LDST_cont_t));
    if (!k_now) {
      free(new_info);
      return LDST_NO_MEM;
    }

    new_info->n = info->n - 1;
    new_info->args = info->args + 1;
    k_now->k_lam.lam_fp = run_impl;
    k_now->k_lam.lam_closure = new_info;
    k_now->k_next = k_then;
  }

  LDST_lam_t lam = value.val_lam;
  return lam.lam_fp(k_now, ctxt, lam.lam_closure, info->args[0]);
}

static LDST_res_t run_impl0(LDST_cont_t *k, LDST_ctxt_t *ctxt, void *run_info, LDST_t value) {
  LDST_fp0_t fp = (LDST_fp0_t)value.val_lam.lam_fp;

  if (run_info) {
    LDST_cont_t *k_new = malloc(sizeof *k_new);
    k_new->k_next = k;
    k_new->k_lam.lam_fp = run_impl;
    k_new->k_lam.lam_closure = run_info;
    k = k_new;
  }

  return fp(k, ctxt);
}

LDST_res_t LDST_run(LDST_ctxt_t *ctxt, LDST_t *result, LDST_fp0_t f, int n, LDST_t *args) {
  struct RunInfo *info = 0;
  if (n > 0) {
    info = malloc(sizeof *info);

    if (!info)
      return LDST_NO_MEM;

    info->n = n;
    info->args = args;
  }

  LDST_lam_t lambda = { run_impl0, info };
  LDST_t arg0 = { .val_lam = { (LDST_fp_t)f, 0 } };
  return LDST_sync(ctxt, result, lambda, arg0);
}

LDST_t LDST_main(LDST_fp0_t f) {
  LDST_t result;
  LDST_ctxt_t *ctxt = LDST_context_create();
  LDST_res_t err = ctxt ? LDST_run(ctxt, &result, f, 0, 0) : LDST_NO_MEM;

  switch (err) {
    case LDST_OK:
      return result;
    case LDST_NO_MEM:
      fputs("ldst: out of memory", stderr);
      break;
    case LDST_DEADLOCK:
      fputs("ldst: deadlocked", stderr);
      break;
    case LDST_UNMATCHED_LABEL:
      fputs("ldst: unmatched label", stderr);
      break;
    case LDST_NO_RESULT:
      fputs("ldst: result not available", stderr);
      break;
    default:
      fputs("ldst: unknown error", stderr);
      break;
  }
  exit(err);
}

static LDST_res_t nat_fold_k(LDST_cont_t *k, LDST_ctxt_t *ctxt, void *void_closure, LDST_t v) {
  // closure[0] = f
  // closure[1] = n
  // closure[2] = i
  // closure[3] = a
  LDST_t *closure = void_closure;

  LDST_cont_t *new_k = malloc(sizeof(LDST_cont_t));
  if (!new_k)
    return LDST_NO_MEM;

  new_k->k_lam.lam_fp = LDST_nat_fold;
  new_k->k_lam.lam_closure = closure;
  new_k->k_next = k;

  LDST_t a = closure[3];
  LDST_lam_t f = v.val_lam;
  return f.lam_fp(new_k, ctxt, f.lam_closure, a);
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
LDST_res_t LDST_nat_fold(LDST_cont_t *k, LDST_ctxt_t *ctxt, void *void_closure, LDST_t a) {
  // closure[0] = f
  // closure[1] = n
  // closure[2] = i
  LDST_t *closure = void_closure;
  int n = closure[1].val_int;
  int i = closure[2].val_int;

  if (n == i)
    return LDST_invoke(k, ctxt, a);

  // The closure is reused both for the continuation and in there (see
  // `nat_fold_k`) it is also used as the closure to the recusive call to
  // `ldst_nat_fold` (this function).
  LDST_t *new_closure = malloc(4 * sizeof(LDST_t));
  if (!new_closure)
    return LDST_NO_MEM;

  LDST_cont_t *new_k = malloc(sizeof(LDST_cont_t));
  if (!new_k) {
    free(new_closure);
    return LDST_NO_MEM;
  }

  LDST_lam_t f = closure[0].val_lam;
  new_closure[0].val_lam = f;
  new_closure[1].val_int = n;
  new_closure[2].val_int = i + 1;
  new_closure[3] = a;

  new_k->k_lam.lam_fp = nat_fold_k;
  new_k->k_lam.lam_closure = new_closure;
  new_k->k_next = k;

  LDST_t idx = { .val_int = i };
  return f.lam_fp(new_k, ctxt, f.lam_closure, idx);
}

// The recv operator returns a pair where the first element is the received
// value and the second element is the "new" channel.
LDST_res_t LDST_make_recv_result(LDST_chan_t *chan, LDST_t value, LDST_t *result) {
  LDST_t *received_pair = malloc(2 * sizeof(LDST_t));
  if (!received_pair)
    return LDST_NO_MEM;

  received_pair[0] = value;
  received_pair[1].val_chan = chan;
  result->val_pair = received_pair;
  return LDST_OK;
}
