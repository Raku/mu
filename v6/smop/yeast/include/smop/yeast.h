#ifndef SMOP_YEAST_H
#define SMOP_YEAST_H

#include <smop/base.h>
#include <smop/nagc.h>

typedef struct SMOP__Yeast__Frame {
  SMOP__NAGC__Object__BASE;
  SMOP__Object* back;
  SMOP__Object* lexical;
  SMOP__Object* catch;
  SMOP__Object* control;
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

SMOP__Object* SMOP__Yeast_create(int registers,SMOP__Object** constants,void (*step)(SMOP__Object* interpreter, SMOP__Yeast__Frame* frame));
SMOP__Object* SMOP__Yeast__Frame_create(SMOP__Object* interpreter,SMOP__Object* yeast);
SMOP__Object* SMOP__Frame_create(SMOP__Object* interpreter,SMOP__Object* yeast_or_mold);
extern SMOP__Object* SMOP__Yeast__RI;

void smop_yeast_init();
void smop_yeast_destr();

void smop_yeast_frame_init();
void smop_yeast_frame_destr();

void yeast_reg_set(SMOP__Object* interpreter,SMOP__Object* frame, int regnum, SMOP__Object* value);
void smop_reg_set(SMOP__Object* interpreter,SMOP__Object* frame, int regnum, SMOP__Object* value);
void smop_back_set(SMOP__Object* interpreter,SMOP__Object* moldframe, SMOP__Object* value);

#endif
