#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>
#include <smop/yeast.h>
#include <stdlib.h>

static SMOP__NAGC__ResponderInterface* RI;

SMOP__Object* SMOP__Yeast_create(int registers,SMOP__Object** constants,void (*step)(SMOP__Object* interpreter, SMOP__Object* frame)) {
    SMOP__Yeast* ret = (SMOP__Yeast*) smop_nagc_alloc(sizeof(SMOP__Yeast));
    ret->RI = (SMOP__ResponderInterface*)RI;

    int i;
    for (i = 0;constants[i];i++);
    i++;

    ret->constants = malloc(sizeof(SMOP__Object*) * i);
    memcpy(ret->constants,constants,sizeof(SMOP__Object*) * i);

    ret->constants_len = i-1;

    ret->registers = registers+i;

    ret->step = step;

    return (SMOP__Object*) ret;
}

static void DESTROYALL(SMOP__Object* interpreter,
                              SMOP__Object* value) {
  SMOP__Yeast* yeast = (SMOP__Yeast*)value;
  int i;
  for (i=0;i <= yeast->constants_len;i++) {
    if (yeast->constants[i])
      SMOP_RELEASE(interpreter,yeast->constants[i]);
  }
  free(yeast->constants);
}

void smop_yeast_init() {
  RI = calloc(1,sizeof(SMOP__NAGC__ResponderInterface));
  RI->RI = (SMOP__ResponderInterface*) SMOP__metaRI;
  ((SMOP__NAGC__ResponderInterface*)RI)->MESSAGE = smop_placeholder_message;
  ((SMOP__NAGC__ResponderInterface*)RI)->DESTROYALL = DESTROYALL;
  ((SMOP__NAGC__ResponderInterface*)RI)->REFERENCE = smop_nagc_reference;
  ((SMOP__NAGC__ResponderInterface*)RI)->RELEASE = smop_nagc_release;
  ((SMOP__NAGC__ResponderInterface*)RI)->id = "yeast";
}

void smop_yeast_destr() {
  free(RI);
}
