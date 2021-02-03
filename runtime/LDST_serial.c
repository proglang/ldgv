//
// LDST_serial.c
//
// Serial LDST backend.

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "LDST.h"

struct LDST_queue_t {
  union  LDST_t           q_val;
  struct LDST_cont_t     *q_cont;
  struct LDST_queue_t    *q_next;
};

struct LDST_chan_t {
  union LDST_t            chan_value;
  struct LDST_cont_t     *chan_cont;
};

static bool Executing = false;
static int BlockedCount = 0;
static struct LDST_queue_t *Runnables = 0;

static enum LDST_res_t enqueue(struct LDST_cont_t *cont, union LDST_t value) {
  struct LDST_queue_t *runnable = malloc(sizeof(struct LDST_queue_t));
  if (!runnable) {
    return LDST__no_mem;
  }

  runnable->q_val  = value;
  runnable->q_cont = cont;
  runnable->q_next = Runnables;
  Runnables = runnable;
  return LDST__ok;
}

static void reset_channel(struct LDST_chan_t *chan) {
  memset(chan, 0, sizeof(struct LDST_chan_t));
}

enum LDST_res_t ldst__chan_new(struct LDST_chan_t **chan) {
  struct LDST_chan_t *new_chan = malloc(sizeof(struct LDST_chan_t));
  if (!new_chan)
    return LDST__no_mem;

  reset_channel(new_chan);
  *chan = new_chan;
  return LDST__ok;
}

static bool should_suspend(struct LDST_cont_t *k, struct LDST_chan_t *chan) {
  if (chan->chan_cont) {
    BlockedCount--;
    return false;
  }

  chan->chan_cont = k;
  BlockedCount++;
  return true;
}

static enum LDST_res_t make_recv_result(struct LDST_chan_t *chan, union LDST_t value, union LDST_t *result) {
  union LDST_t *received_pair = malloc(2 * sizeof(union LDST_t));
  if (!received_pair)
    return LDST__no_mem;

  received_pair[0] = value;
  received_pair[1].val_chan = chan;
  result->val_pair = received_pair;
  return LDST__ok;
}

enum LDST_res_t ldst__chan_send(struct LDST_cont_t *k, void *channel, union LDST_t value) {
  struct LDST_chan_t *chan = channel;
  if (should_suspend(k, chan)) {
    chan->chan_value = value;
    return LDST__ok;
  }

  // Enqueue the receiving side.
  enum LDST_res_t res;
  res = make_recv_result(chan, value, &value);
  if (res != LDST__ok) {
    return res;
  }
  res = enqueue(chan->chan_cont, value);
  if (res != LDST__ok) {
    free(value.val_pair);
    return res;
  }

  // Continue the current thread.
  reset_channel(chan);
  value.val_chan = chan;
  return ldst__invoke(k, value);
}

enum LDST_res_t ldst__chan_recv(struct LDST_cont_t *k, struct LDST_chan_t *chan) {
  if (should_suspend(k, chan)) {
    return LDST__ok;
  }

  // Enqueue the sending side.
  enum LDST_res_t res;
  union LDST_t value = { .val_chan = chan };
  res = enqueue(chan->chan_cont, value);
  if (res != LDST__ok)
    return res;

  // Create the result value for the calling thread.
  res = make_recv_result(chan, chan->chan_value, &value);
  if (res != LDST__ok)
    return res;

  // Continue the current thread.
  reset_channel(chan);
  return ldst__invoke(k, value);
}

static enum LDST_res_t run_runnables() {
  if (Executing) {
    return LDST__ok;
  }

  Executing = true;

  enum LDST_res_t res = LDST__ok;
  struct LDST_queue_t *runnable;
  while (res == LDST__ok && (runnable = Runnables)) {
    Runnables = runnable->q_next;
    res = ldst__invoke(runnable->q_cont, runnable->q_val);
    free(runnable);
  }

  Executing = false;
  if (res != LDST__ok)
    return res;

  if (BlockedCount > 0)
    return LDST__deadlock;

  return LDST__ok;
}

enum LDST_res_t ldst__fork(struct LDST_lam_t op, union LDST_t value) {
  struct LDST_cont_t *k = malloc(sizeof(struct LDST_cont_t));
  if (!k) {
    return LDST__no_mem;
  }

  k->k_lam = op;
  k->k_next = 0;
  enum LDST_res_t res = enqueue(k, value);
  if (res != LDST__ok) {
    free(k);
    return res;
  }

  return run_runnables();
}

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
    return ldst__invoke(k_then, value);
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

enum LDST_res_t ldst__run( union LDST_t *result, LDST_fp0_t f, int n, union LDST_t *args) {
  struct RunInfo *info = malloc(sizeof(struct RunInfo));
  if (!info)
    return LDST__no_mem;

  info->applied0 = false;
  info->n = n;
  info->args = args;
  info->result = result;
  struct LDST_lam_t lambda = { run_impl, info };
  union LDST_t arg0 = { .val_lam = { (LDST_fp_t)f, 0 } };
  return ldst__fork(lambda, arg0);
}
