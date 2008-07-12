#include <stdlib.h>
#include <stdio.h>
#include <smop.h>
#include <string.h>
#include <smop_lowlevel.h>
#include <smop_s1p.h>

SMOP__Object* SMOP__S1P__Hash;

typedef struct hash_bucket {
    struct hash_bucket* next;
    SMOP__Object* key;
    SMOP__Object* cell;
} hash_bucket;
typedef struct smop_s1p_hash_struct {
  SMOP__Object__BASE
  int size;
  hash_bucket** buckets;
} smop_s1p_hash_struct;

/*static void hash_grow(smop_s1p_hash_struct* hash,int new_size) {
}*/
SMOP__Object* SMOP__S1P__Hash_create(void) {
    smop_s1p_hash_struct* ret = (smop_s1p_hash_struct*) smop_lowlevel_alloc(sizeof(smop_s1p_hash_struct));
    ret->RI = (SMOP__ResponderInterface*)SMOP__S1P__Hash;
    ret->buckets = (hash_bucket**) calloc(1,sizeof(hash_bucket*));
    ret->size = 1;
    return (SMOP__Object*) ret;
}
static SMOP__Object* smop_s1p_hash_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {

  smop_s1p_hash_struct* invocant = (smop_s1p_hash_struct*)(SMOP__NATIVE__capture_invocant(interpreter, capture));

  if (invocant) SMOP_RELEASE(interpreter,invocant);

  if (identifier == SMOP__ID__postcircumfix_curly) {
    if (SMOP__NATIVE__capture_positional_count(interpreter,capture) == 1) {
      SMOP__Object* key = SMOP__NATIVE__capture_positional(interpreter,capture,0);
      int hashing_result = 0;
      /*if (SMOP_RI(key) == (SMOP__ResponderInterface*)SMOP__S1P__Str) {
        //fprintf(stderr,"key hashed to %d\n",hashing_result);
      } else {
        fprintf(stderr,"only SMOP__S1P__Str keys are supported for now\n");
      }*/

      hash_bucket* bucket = invocant->buckets[hashing_result];
      //fprintf(stderr,"bucket %p\n",bucket);
      if (bucket) while (1) {
        if (bucket->key == key) {
          fprintf(stderr,"# found key in hash\n");
          SMOP_REFERENCE(interpreter,bucket->cell);
          SMOP_RELEASE(interpreter,capture);
          return bucket->cell;
        } else if (bucket->next) {
          bucket = bucket->next;
        } else {
          break;
        }
      }

      //fprintf(stderr,"creating new cell\n");
      SMOP__Object* cell = SMOP__S1P__Scalar_create(SMOP__NATIVE__bool_false);
      hash_bucket* new_bucket = (hash_bucket*) calloc(1,sizeof(hash_bucket));

      new_bucket->key  = key;
      //SMOP_REFERENCE(interpreter,key);

      new_bucket->cell = cell;
      new_bucket->next = NULL;
      if (bucket) bucket->next = new_bucket;
      else invocant->buckets[hashing_result] = new_bucket;

      SMOP_REFERENCE(interpreter,cell);
      SMOP_RELEASE(interpreter,capture);
      return cell;
    } else {
      fprintf(stderr,"wrong number of arguments to postrcicumfix:<{ }>\n");
    }
  } else if (identifier == SMOP__ID__new) {
    return SMOP__S1P__Hash_create();

  } else if (identifier == SMOP__ID__DESTROYALL) {
      int i;
      for (i=0;i<invocant->size;i++) {
          hash_bucket* current = invocant->buckets[i];
          while (current) {
              SMOP_RELEASE(interpreter,current->key);
              SMOP_RELEASE(interpreter,current->cell);
              hash_bucket* prev = current;
              current = current->next;
              free(prev);
          }
      }
      free(invocant->buckets);
  }

  SMOP_RELEASE(interpreter,capture);
  return SMOP__NATIVE__bool_false;
}


void smop_s1p_hash_init() {
  SMOP__S1P__Hash = calloc(1,sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__S1P__Hash)->MESSAGE = smop_s1p_hash_message;
  ((SMOP__ResponderInterface*)SMOP__S1P__Hash)->REFERENCE = smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__S1P__Hash)->RELEASE = smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__S1P__Hash)->id = "s1p Hash";
}

void smop_s1p_hash_destr() {
  free(SMOP__S1P__Hash);
}


