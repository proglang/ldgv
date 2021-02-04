//
// LDST_serial.c
//
// Serial LDST backend.

#include <stdbool.h>
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
