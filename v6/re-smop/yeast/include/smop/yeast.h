#ifndef SMOP_YEAST_H
#define SMOP_YEAST_H

#include <smop/base.h>
#include <smop/nagc.h>

typedef struct SMOP__Yeast__Frame {
  SMOP__NAGC__Object__BASE;
  SMOP__Object* back;
  SMOP__Object** ret;
  SMOP__Object** reg;
  SMOP__Object* yeast;
  int pc;
  void (*step)(SMOP__Object* interpreter,
              SMOP__Object* frame);
} SMOP__Yeast__Frame;

typedef struct SMOP__Yeast {
  SMOP__NAGC__Object__BASE
  int registers;
  SMOP__Object** constants;
  int constants_len;
  void (*step)(SMOP__Object* interpreter,
              SMOP__Object* frame);
} SMOP__Yeast;

SMOP__Object* SMOP__Yeast__Frame_create(SMOP__Object* interpreter,SMOP__Object* yeast);

void smop_yeast_init();
void smop_yeast_destr();

void smop_yeast_frame_init();
void smop_yeast_frame_destr();

void yeast_reg_set(SMOP__Object* interpreter,SMOP__Object* moldframe, int regnum, SMOP__Object* value);

#endif
