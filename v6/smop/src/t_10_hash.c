#include "smop.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..8\n");
  smop_init();

  SMOP__CORE__Value* key, val, plc;

  SMOP__CORE__Hash* hash = smop_hash_create();

  key = (SMOP__CORE__Value*)smop_bytes_create("efgh", 4);
  SMOP__CORE__Scalar* looked = SMOP_HASH_LOOKP(key);

  if (looked == NULL) {
    printf("not ok");
  } else {
    printf("ok");
  }
  printf(" 1 - LOOKP returns an object.\n");

  if (SMOP_HASH_EXISTS(key) == NULL) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 2 - LOOKP doesn't autovivify.\n");

  val = (SMOP__CORE__Value*)smop_int_create(1234);
  SMOP__CORE__Value* old = SMOP_SCALAR_STORE(looked, val);
  smop_value_refcnt_dec(old); old=NULL;
  smop_value_refcnt_dec(val); val=NULL;

  val = SMOP_HASH_LOOKP(key);
  if (val == NULL) {
    printf("not ok");
  } else {
    printf("ok");
  }
  printf(" 3 - STORE autovivify.\n");
  smop_value_refcnt_dec(key); key=NULL;
  
  if (smop_int_lowlevel((SMOP__CORE__int*)val) == 1234) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 4 - LOOKP gets the right value.");
  smop_value_refcnt_dec(val); val=NULL;

  key = (SMOP__CORE__Value*)smop_int_create(1234);
  val = (SMOP__CORE__Value*)smop_int_create(1234);
  plc = SMOP_HASH_LOOKP(key);
  old = SMOP_SCALAR_STORE(plc,val);
  smop_value_refcnt_dec(plc); plc=NULL;
  smop_value_refcnt_dec(old); old=NULL;
  smop_value_refcnt_dec(key); key=NULL;
  smop_value_refcnt_dec(val); val=NULL;

  printf("ok 5 - STORING and replacing keys in the HASH.");

  key = (SMOP__CORE__Value*)smop_bytes_create("abcd", 4);
  val = (SMOP__CORE__Value*)smop_bytes_create("abcd", 4);
  plc = SMOP_HASH_LOOKP(key);
  old = SMOP_SCALAR_STORE(plc,val);
  smop_value_refcnt_dec(plc); plc=NULL;
  smop_value_refcnt_dec(old); old=NULL;
  smop_value_refcnt_dec(key); key=NULL;
  smop_value_refcnt_dec(val); val=NULL;

  printf("ok 6 - STORING new keys to the HASH.");

  key = (SMOP__CORE__Value*)smop_bytes_create("abcd", 4);
  val = SMOP_HASH_LOOKP(key);
  if (val == NULL) {
    printf("not ok");
  } else {
    printf("ok");
  }
  printf(" 7 - STORE autovivify.\n");
  smop_value_refcnt_dec(key); key=NULL;

  char* result; int size;
  result = smop_bytes_lowlevel((SMOP__CORE__bytes*)val, &size);
  if (strncmp(result,"abcd",4)==0) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 8 - LOOKP gets the right value.");
  smop_value_refcnt_dec(val); val=NULL;


  smop_destr();
  return 0;
}
