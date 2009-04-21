#ifndef SMOP_LOST_H
#define SMOP_LOST_H

#include <smop/base.h>
#include <smop/nagc.h>

typedef struct SMOP__LOST__Frame {
  SMOP__NAGC__Object__BASE;
  SMOP__Object* back;
  SMOP__Object* lastr;
  void* user;
  int pc;
  int (*step)(SMOP__Object* interpreter,
              SMOP__Object* frame);
  void (*destr)(SMOP__Object* interpreter,
              SMOP__Object* frame);
} SMOP__LOST__Frame;

SMOP__Object* SMOP__LOST__Frame_create(SMOP__Object* interpreter,
                                       SMOP__Object* back,
                                       void* user,
                                       int (*step)(SMOP__Object* interpreter,
                                                   SMOP__Object* frame),
                                       void (*destr)(SMOP__Object* interpreter,
                                                    SMOP__Object* frame));

void smop_lost_init();
void smop_lost_destr();

#endif
