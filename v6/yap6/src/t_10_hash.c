#include "yap6.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..8\n");
  yap6_init();

  YAP6__CORE__Value* key, val, plc;

  YAP6__CORE__Hash* hash = yap6_hash_create();

  key = (YAP6__CORE__Value*)yap6_bytes_create("efgh", 4);
  YAP6__CORE__Scalar* looked = YAP6_HASH_LOOKP(key);

  if (looked == NULL) {
    printf("not ok");
  } else {
    printf("ok");
  }
  printf(" 1 - LOOKP returns an object.\n");

  if (YAP6_HASH_EXISTS(key) == NULL) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 2 - LOOKP doesn't autovivify.\n");

  val = (YAP6__CORE__Value*)yap6_int_create(1234);
  YAP6__CORE__Value* old = YAP6_SCALAR_STORE(looked, val);
  yap6_value_refcnt_dec(old); old=NULL;
  yap6_value_refcnt_dec(val); val=NULL;

  val = YAP6_HASH_LOOKP(key);
  if (val == NULL) {
    printf("not ok");
  } else {
    printf("ok");
  }
  printf(" 3 - STORE autovivify.\n");
  yap6_value_refcnt_dec(key); key=NULL;
  
  if (yap6_int_lowlevel((YAP6__CORE__int*)val) == 1234) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 4 - LOOKP gets the right value.");
  yap6_value_refcnt_dec(val); val=NULL;

  key = (YAP6__CORE__Value*)yap6_int_create(1234);
  val = (YAP6__CORE__Value*)yap6_int_create(1234);
  plc = YAP6_HASH_LOOKP(key);
  old = YAP6_SCALAR_STORE(plc,val);
  yap6_value_refcnt_dec(plc); plc=NULL;
  yap6_value_refcnt_dec(old); old=NULL;
  yap6_value_refcnt_dec(key); key=NULL;
  yap6_value_refcnt_dec(val); val=NULL;

  printf("ok 5 - STORING and replacing keys in the HASH.");

  key = (YAP6__CORE__Value*)yap6_bytes_create("abcd", 4);
  val = (YAP6__CORE__Value*)yap6_bytes_create("abcd", 4);
  plc = YAP6_HASH_LOOKP(key);
  old = YAP6_SCALAR_STORE(plc,val);
  yap6_value_refcnt_dec(plc); plc=NULL;
  yap6_value_refcnt_dec(old); old=NULL;
  yap6_value_refcnt_dec(key); key=NULL;
  yap6_value_refcnt_dec(val); val=NULL;

  printf("ok 6 - STORING new keys to the HASH.");

  key = (YAP6__CORE__Value*)yap6_bytes_create("abcd", 4);
  val = YAP6_HASH_LOOKP(key);
  if (val == NULL) {
    printf("not ok");
  } else {
    printf("ok");
  }
  printf(" 7 - STORE autovivify.\n");
  yap6_value_refcnt_dec(key); key=NULL;

  char* result; int size;
  result = yap6_bytes_lowlevel((YAP6__CORE__bytes*)val, &size);
  if (strncmp(result,"abcd",4)==0) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 8 - LOOKP gets the right value.");
  yap6_value_refcnt_dec(val); val=NULL;


  yap6_destr();
  return 0;
}
