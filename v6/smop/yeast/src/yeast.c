#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>
#include <smop/yeast.h>
#include <stdlib.h>
#include <string.h>

static SMOP__NAGC__ResponderInterface* RI;
SMOP__Object* SMOP__Yeast__RI;
static SMOP__Object* SMOP__ID__create;

SMOP__Object* SMOP__Yeast_create(int registers,SMOP__Object** constants,void (*step)(SMOP__Object* interpreter, SMOP__Yeast__Frame* frame)) {
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

static SMOP__Object* DUMP(SMOP__Object* interpreter,
                                SMOP__ResponderInterface* responder,
                                SMOP__Object* obj) {

  SMOP__Yeast* yeast = (SMOP__Yeast*) obj;

  return smop_dump_create((SMOP__Object*[]) {

      smop_dump_attr_create("RI"),
      smop_dump_obj_create(yeast->RI),
      smop_dump_attr_create("ref_cnt"),
      smop_dump_int_create(yeast->ref_cnt),

      smop_dump_attr_create("registers"),
      smop_dump_int_create(yeast->registers),
      smop_dump_attr_create("constants"),
      smop_dump_obj_array_create(yeast->constants,yeast->constants_len),
      NULL
  });
}

static SMOP__Object* MESSAGE(SMOP__Object* interpreter,
                            SMOP__ResponderInterface* self,
                            SMOP__Object* identifier,
                            SMOP__Object* capture) {
  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  SMOP__Object* invocant = SMOP__NATIVE__capture_positional(interpreter,capture,0);
  if (SMOP__ID__create == identifier) {
    ret = SMOP__Yeast__Frame_create(interpreter,SMOP_REFERENCE(interpreter,invocant));
  }
  SMOP_RELEASE(interpreter,invocant);
  SMOP_RELEASE(interpreter,capture);
  return ret;
}

void smop_yeast_init() {
  SMOP__ID__create = SMOP__NATIVE__idconst_create("create");
  RI = calloc(1,sizeof(SMOP__NAGC__ResponderInterface));
  SMOP__Yeast__RI = RI;
  RI->RI = (SMOP__ResponderInterface*) SMOP__metaRI;
  ((SMOP__NAGC__ResponderInterface*)RI)->MESSAGE = MESSAGE;
  ((SMOP__NAGC__ResponderInterface*)RI)->DESTROYALL = DESTROYALL;
  ((SMOP__NAGC__ResponderInterface*)RI)->DUMP = DUMP;
  ((SMOP__NAGC__ResponderInterface*)RI)->REFERENCE = smop_nagc_reference;
  ((SMOP__NAGC__ResponderInterface*)RI)->RELEASE = smop_nagc_release;
  ((SMOP__NAGC__ResponderInterface*)RI)->id = "yeast";
  smop_yeast_frame_init();
}

void smop_yeast_destr() {
  smop_yeast_frame_destr();
  free(RI);
}
