#include "vroom.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..8\n");
  vroom_init();

  VROOM__CORE__Value* key, val, plc;

  VROOM__CORE__Hash* hash = vroom_hash_create();

  key = (VROOM__CORE__Value*)vroom_bytes_create("efgh", 4);
  VROOM__CORE__Scalar* looked = VROOM_HASH_LOOKP(key);

  if (looked == NULL) {
    printf("not ok");
  } else {
    printf("ok");
  }
  printf(" 1 - LOOKP returns an object.\n");

  if (VROOM_HASH_EXISTS(key) == NULL) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 2 - LOOKP doesn't autovivify.\n");

  val = (VROOM__CORE__Value*)vroom_int_create(1234);
  VROOM__CORE__Value* old = VROOM_SCALAR_STORE(looked, val);
  vroom_value_refcnt_dec(old); old=NULL;
  vroom_value_refcnt_dec(val); val=NULL;

  val = VROOM_HASH_LOOKP(key);
  if (val == NULL) {
    printf("not ok");
  } else {
    printf("ok");
  }
  printf(" 3 - STORE autovivify.\n");
  vroom_value_refcnt_dec(key); key=NULL;
  
  if (vroom_int_lowlevel((VROOM__CORE__int*)val) == 1234) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 4 - LOOKP gets the right value.");
  vroom_value_refcnt_dec(val); val=NULL;

  key = (VROOM__CORE__Value*)vroom_int_create(1234);
  val = (VROOM__CORE__Value*)vroom_int_create(1234);
  plc = VROOM_HASH_LOOKP(key);
  old = VROOM_SCALAR_STORE(plc,val);
  vroom_value_refcnt_dec(plc); plc=NULL;
  vroom_value_refcnt_dec(old); old=NULL;
  vroom_value_refcnt_dec(key); key=NULL;
  vroom_value_refcnt_dec(val); val=NULL;

  printf("ok 5 - STORING and replacing keys in the HASH.");

  key = (VROOM__CORE__Value*)vroom_bytes_create("abcd", 4);
  val = (VROOM__CORE__Value*)vroom_bytes_create("abcd", 4);
  plc = VROOM_HASH_LOOKP(key);
  old = VROOM_SCALAR_STORE(plc,val);
  vroom_value_refcnt_dec(plc); plc=NULL;
  vroom_value_refcnt_dec(old); old=NULL;
  vroom_value_refcnt_dec(key); key=NULL;
  vroom_value_refcnt_dec(val); val=NULL;

  printf("ok 6 - STORING new keys to the HASH.");

  key = (VROOM__CORE__Value*)vroom_bytes_create("abcd", 4);
  val = VROOM_HASH_LOOKP(key);
  if (val == NULL) {
    printf("not ok");
  } else {
    printf("ok");
  }
  printf(" 7 - STORE autovivify.\n");
  vroom_value_refcnt_dec(key); key=NULL;

  char* result; int size;
  result = vroom_bytes_lowlevel((VROOM__CORE__bytes*)val, &size);
  if (strncmp(result,"abcd",4)==0) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 8 - LOOKP gets the right value.");
  vroom_value_refcnt_dec(val); val=NULL;


  vroom_destr();
  return 0;
}
