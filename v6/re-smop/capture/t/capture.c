#include <stdlib.h>
#include <stdio.h>
#include <smop/capture.h>
#include <smop/s0native.h>
int main() {
  printf("1..5\n");
  smop_s0native_init();
  smop_nagc_init();
  smop_capture_init();

  SMOP__Object* key1 = SMOP__NATIVE__idconst_create("key1");
  SMOP__Object* key2 = SMOP__NATIVE__idconst_create("key2");
  SMOP__Object* value1 = SMOP__NATIVE__idconst_create("value1");
  SMOP__Object* value2 = SMOP__NATIVE__idconst_create("value2");
  SMOP__Object* capture = SMOP__NATIVE__capture_create(SMOP__EmptyInterpreter,(SMOP__Object*[]) {SMOP__NATIVE__bool_false,SMOP__NATIVE__bool_true,NULL},(SMOP__Object*[]) {
      key1,value1,
      key2,value2,
      NULL
  });
  printf("ok 1\n");
  if (SMOP__NATIVE__capture_positional(SMOP__EmptyInterpreter,capture,0) == SMOP__NATIVE__bool_false) {
    printf("ok 2\n");
  } else {
    printf("not ok 2\n");
  }
  if (SMOP__NATIVE__capture_positional(SMOP__EmptyInterpreter,capture,1) == SMOP__NATIVE__bool_true) {
    printf("ok 3\n");
  } else {
    printf("not ok 3\n");
  }
  if (SMOP__NATIVE__capture_named(SMOP__EmptyInterpreter,capture,key1) == value1) {
    printf("ok 4\n");
  } else {
    printf("not ok 4 \n");
    int len;
    printf("%s\n",SMOP__NATIVE__idconst_fetch_with_null(SMOP__NATIVE__capture_named(SMOP__EmptyInterpreter,capture,key1),&len));
  }
  if (SMOP__NATIVE__capture_named(SMOP__EmptyInterpreter,capture,key2) == value2) {
    printf("ok 5\n");
  } else {
    printf("not ok 5\n");
  }
  SMOP_RELEASE(SMOP__EmptyInterpreter,capture);

  smop_capture_destr();
  smop_nagc_destr();
  smop_s0native_destr();
  return 0;
}

