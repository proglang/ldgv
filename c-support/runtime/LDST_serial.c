//
// LDST_serial.c
//
// Serial LDST backend.

#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include "LDST.h"

typedef struct cont_stack {
  union  LDST_t           q_val;
  struct LDST_cont_t     *q_cont;
  struct cont_stack      *q_next;
} cont_stack_t;

struct LDST_chan_t {
  union LDST_t            chan_value;
  struct LDST_cont_t     *chan_cont;
};

static bool Executing = false;
static int BlockedCount = 0;
static cont_stack_t *Runnables = 0;

static enum LDST_res_t enqueue(struct LDST_cont_t *cont, union LDST_t value) {
  cont_stack_t *runnable = malloc(sizeof(cont_stack_t));
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

enum LDST_res_t ldst_chan_new(struct LDST_chan_t **chan) {
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

enum LDST_res_t ldst_chan_send(struct LDST_cont_t *k, void *channel, union LDST_t value) {
  struct LDST_chan_t *chan = channel;
  if (should_suspend(k, chan)) {
    chan->chan_value = value;
    return LDST__ok;
  }

  // Enqueue the receiving side.
  enum LDST_res_t res;
  res = ldst_make_recv_result(chan, value, &value);
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
  return ldst_invoke(k, value);
}

enum LDST_res_t ldst_chan_recv(struct LDST_cont_t *k, struct LDST_chan_t *chan) {
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
  res = ldst_make_recv_result(chan, chan->chan_value, &value);
  if (res != LDST__ok)
    return res;

  // Continue the current thread.
  reset_channel(chan);
  return ldst_invoke(k, value);
}

static enum LDST_res_t run_runnables() {
  if (Executing) {
    return LDST__ok;
  }

  Executing = true;

  enum LDST_res_t res = LDST__ok;
  cont_stack_t *runnable;
  while (res == LDST__ok && (runnable = Runnables)) {
    Runnables = runnable->q_next;
    res = ldst_invoke(runnable->q_cont, runnable->q_val);
    free(runnable);
  }

  Executing = false;
  if (res != LDST__ok)
    return res;

  if (BlockedCount > 0)
    return LDST__deadlock;

  return LDST__ok;
}

enum LDST_res_t ldst_fork(struct LDST_lam_t op, union LDST_t value) {
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