//
// LDST_serial.c
//
// Serial LDST backend.

#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include "LDST.h"

typedef struct cont_stack {
  LDST_t             q_val;
  LDST_cont_t       *q_cont;
  struct cont_stack *q_next;
} cont_stack_t;

struct LDST_chan {
  LDST_t            chan_value;
  LDST_cont_t     *chan_cont;
};

static bool Executing = false;
static int BlockedCount = 0;
static cont_stack_t *Runnables = 0;

static LDST_res_t enqueue(LDST_cont_t *cont, LDST_t value) {
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

static void reset_channel(LDST_chan_t *chan) {
  memset(chan, 0, sizeof(LDST_chan_t));
}

LDST_res_t ldst_chan_new(LDST_chan_t **chan) {
  LDST_chan_t *new_chan = malloc(sizeof(LDST_chan_t));
  if (!new_chan)
    return LDST__no_mem;

  reset_channel(new_chan);
  *chan = new_chan;
  return LDST__ok;
}

static bool should_suspend(LDST_cont_t *k, LDST_chan_t *chan) {
  if (chan->chan_cont) {
    BlockedCount--;
    return false;
  }

  chan->chan_cont = k;
  BlockedCount++;
  return true;
}

LDST_res_t ldst_chan_send(LDST_cont_t *k, void *channel, LDST_t value) {
  LDST_chan_t *chan = channel;
  if (should_suspend(k, chan)) {
    chan->chan_value = value;
    return LDST__ok;
  }

  // Enqueue the receiving side.
  LDST_res_t res;
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

LDST_res_t ldst_chan_recv(LDST_cont_t *k, LDST_chan_t *chan) {
  if (should_suspend(k, chan)) {
    return LDST__ok;
  }

  // Enqueue the sending side.
  LDST_res_t res;
  LDST_t value = { .val_chan = chan };
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

static LDST_res_t run_runnables() {
  if (Executing) {
    return LDST__ok;
  }

  Executing = true;

  LDST_res_t res = LDST__ok;
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

LDST_res_t ldst_fork(LDST_lam_t op, LDST_t value) {
  LDST_cont_t *k = malloc(sizeof(LDST_cont_t));
  if (!k) {
    return LDST__no_mem;
  }

  k->k_lam = op;
  k->k_next = 0;
  LDST_res_t res = enqueue(k, value);
  if (res != LDST__ok) {
    free(k);
    return res;
  }

  return run_runnables();
}
