#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <smop.h>
#include <smop_lowlevel.h>

/* The native capture prototype is at the same time a responder
 * interface. And the prototype is not subject to garbage
 * collection. Each capture instance, however uses the smop_lowlevel,
 * and, therefore, is subject to gc.
 */
SMOP__Object* SMOP__NATIVE__capture;

typedef struct named_argument {
  SMOP__Object* key;
  SMOP__Object* value;
} named_argument;

typedef struct native_capture_struct {
  SMOP__Object__BASE
  SMOP__Object* invocant;
  SMOP__Object** positional;
  int count_positional;
  /* The keys that are constant identifiers can have an optimized
   * lookup process.
   */
  named_argument* o_named;
  int count_o_named;
  /* Any other object will go to the normal lookup.
   */
  named_argument* named;
  int count_named;
} native_capture_struct;

/* A constant empty capture will be created. Understanding that a
 * capture is readonly, everytime someone tryies to create one using
 * "new", the constant empty capture will be returned.
 */
static SMOP__Object* smop_native_empty_capture;

static SMOP__Object* capture_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  SMOP__Object* ret = NULL;

  if (identifier == SMOP__ID__new) {
    ret = smop_native_empty_capture;
  } else if (identifier == SMOP__ID__DESTROYALL) {
    if (capture && capture != (SMOP__Object*)self && capture != smop_native_empty_capture) {
      native_capture_struct* self = (native_capture_struct*)capture;
      smop_lowlevel_wrlock(capture);
      SMOP__Object* invocant = self->invocant; self->invocant = NULL;
      SMOP__Object** positional = self->positional; self->positional = NULL; self->count_positional = 0;
      named_argument* o_named = self->o_named; self->o_named = NULL; self->count_o_named = 0;
      named_argument* named = self->named; self->named = NULL; self->count_named = 0;
      smop_lowlevel_unlock(capture);

      if (invocant) SMOP_RELEASE(interpreter, invocant);
      int count = 0;
      while (positional[count]) {
        SMOP_RELEASE(interpreter, positional[count]);
        count++;
      }
      int i = 0;
      while (i < count) {
        SMOP_RELEASE(interpreter, o_named[i].key);
        SMOP_RELEASE(interpreter, o_named[i].value);
        i++;
      }
      i = 0;
      while (i < count) {
        SMOP_RELEASE(interpreter, named[i].key);
        SMOP_RELEASE(interpreter, named[i].value);
        i++;
      }
      free(positional);
      free(o_named);
      free(named);
    }
    ret = NULL;
  }
  return ret;
}

static SMOP__Object* capture_reference(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if ((SMOP__Object*)responder != obj && smop_native_empty_capture != obj) {
    smop_lowlevel_refcnt_inc(interpreter, responder, obj);
  }
  return obj;
}

static SMOP__Object* capture_release(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if ((SMOP__Object*)responder != obj && smop_native_empty_capture != obj) {
    smop_lowlevel_refcnt_dec(interpreter, responder, obj);
  }
  return obj;
}



void smop_native_capture_init() {

  // initialize the capture prototype
  SMOP__NATIVE__capture = calloc(1, sizeof(SMOP__ResponderInterface));
  assert(SMOP__NATIVE__capture);
  ((SMOP__ResponderInterface*)SMOP__NATIVE__capture)->MESSAGE = capture_message;
  ((SMOP__ResponderInterface*)SMOP__NATIVE__capture)->REFERENCE = capture_reference;
  ((SMOP__ResponderInterface*)SMOP__NATIVE__capture)->RELEASE = capture_release;

  // initialize the constant empty capture
  smop_native_empty_capture = calloc(1, sizeof(native_capture_struct));
  assert(smop_native_empty_capture);
  smop_native_empty_capture->RI = (SMOP__ResponderInterface*)SMOP__NATIVE__capture;

}

void smop_native_capture_destr() {

  // destroy the constant empty capture
  free(smop_native_empty_capture);
  // destroy the capture prototype
  free(SMOP__NATIVE__capture);
 
}

static int cmp_opt_named(const void* p1, const void* p2) {
  if (!p1 && !p2) {
    return 0;
  } else if (p1 && !p2) {
    return -1;
  } else if (!p1 && p2) {
    return 1;
  } else {
    named_argument* n1 = (named_argument*)p1;
    named_argument* n2 = (named_argument*)p2;
    if (n1->key > n2->key) {
      return -1;
    } else if (n2->key > n1->key) {
      return 1;
    } else {
      return 0;
    }
  }
}

SMOP__Object*   SMOP__NATIVE__capture_create(SMOP__Object* interpreter,
                                             SMOP__Object* invocant,
                                             SMOP__Object** positional,
                                             SMOP__Object** named) {

  if (invocant == NULL && positional == NULL && named == NULL)
    return smop_native_empty_capture;

  native_capture_struct* ret = (native_capture_struct*)smop_lowlevel_alloc(sizeof(native_capture_struct));
  
  ret->invocant = invocant;

  if (positional) {
    int length = -1;
    while (positional[++length]);
    int size = sizeof(SMOP__Object*) * length;
    ret->positional = malloc(size);
    memcpy(ret->positional, positional, size);
    ret->count_positional = length - 1;
  }

  if (named) {
    int length = 0;
    int l_opt = 0;
    int l_nor = 0;
    while (named[length]) {
      if (named[length]->RI == SMOP__ID__new->RI) {
        l_opt++;
      } else {
        l_nor++;
      }
      length += 2;
    }

    int s_opt = sizeof(named_argument) * (l_opt + 1);
    ret->o_named = malloc(s_opt);
    ret->o_named[l_opt].key = NULL;
    ret->o_named[l_opt].value = NULL;
    ret->count_o_named = l_opt;
    int s_nor = sizeof(named_argument) * (l_nor + 1);
    ret->named = malloc(s_nor);
    ret->named[l_nor].key = NULL;
    ret->named[l_nor].value = NULL;
    ret->count_named = l_nor;

    length = 0;
    l_opt = 0;
    l_nor = 0;
    while (named[length]) {
      if (named[length]->RI == SMOP__ID__new->RI) {
        ret->o_named[l_opt].key = named[length];
        ret->o_named[l_opt].value = named[length + 1];
        l_opt++;
      } else {
        ret->named[l_nor].key = named[length];
        ret->named[l_nor].value = named[length + 1];
        l_nor++;
      }
      length += 2;
    }

    /* To optimize lookup, let's sort the named arguments in order to
     * be able to do binary searchs later.
     */
    if (l_opt) {
      qsort(ret->o_named, l_opt, sizeof(named_argument), cmp_opt_named);
    }

    /* The same would apply here, but we still don't have the string
     * code to call WHICH.
     */
    if (l_nor) {
      fprintf(stderr, "Native capture still don't support non-constant-identifiers as key for named arguments.\n");
    }
    
  }
  
  return (SMOP__Object*)ret;
}

SMOP__Object*   SMOP__NATIVE__capture_invocant(SMOP__Object* interpreter,
                                               SMOP__Object* capture) {
  if (capture) {
    smop_lowlevel_rdlock(capture);
    SMOP__Object* invocant = ((native_capture_struct*)capture)->invocant;
    smop_lowlevel_unlock(capture);
    return SMOP_REFERENCE(interpreter, invocant);
  } else {
    return NULL;
  }
}



SMOP__Object*   SMOP__NATIVE__capture_positional(SMOP__Object* interpreter,
                                                 SMOP__Object* capture, int p) {
  if (capture) {
    SMOP__Object* arg;
    native_capture_struct* self = ((native_capture_struct*)capture);
    smop_lowlevel_rdlock(capture);
    if (p < self->count_positional) {
      arg = self->positional[p];
      smop_lowlevel_unlock(capture);
      return SMOP_REFERENCE(interpreter, self->positional[p]);
    } else {
      smop_lowlevel_unlock(capture);
      return NULL;
    }
  } else {
    return NULL;
  }
}

SMOP__Object*   SMOP__NATIVE__capture_named(SMOP__Object* interpreter,
                                            SMOP__Object* capture,
                                            SMOP__Object* identifier) {
  if (capture) {
    if (identifier->RI == SMOP__ID__new->RI) {
      native_capture_struct* self = (native_capture_struct*)capture;
      smop_lowlevel_rdlock(capture);
      SMOP__Object* ret = bsearch(identifier, self->o_named, self->count_o_named, sizeof(named_argument), cmp_opt_named);
      if (ret) {
        smop_lowlevel_unlock(capture);
        return SMOP_REFERENCE(interpreter,ret);
      } else {
        if (self->count_named) {
          fprintf(stderr, "Native capture still don't support non-constant-identifiers as key for named arguments.\n");
        }
        smop_lowlevel_unlock(capture);
        return NULL;
      }
    } else {
      fprintf(stderr, "Native capture still don't support non-constant-identifiers as key for named arguments.\n");
      return NULL;
    }
  } else {
    return NULL;
  }
}

int SMOP__NATIVE__capture_may_recurse(SMOP__Object* interpreter,
                                      SMOP__Object* capture) {
  if (capture) {
    if (capture == smop_native_empty_capture) {
      return 0;
    } else {
      native_capture_struct* self = (native_capture_struct*)capture;
      smop_lowlevel_rdlock(capture);
      int c = self->count_named;
      smop_lowlevel_unlock(capture);
      if (c) {
        return 1;
      } else {
        return 0;
      }
    }
  } else {
    return 0;
  }
}

int SMOP__NATIVE__capture_positional_count(SMOP__Object* interpreter,
                                           SMOP__Object* capture) {
  if (capture) {
    if (capture == smop_native_empty_capture) {
      return 0;
    } else {
      native_capture_struct* self = (native_capture_struct*)capture;
      smop_lowlevel_rdlock(capture);
      int c = self->count_positional;
      smop_lowlevel_unlock(capture);
      return c;
    }
  } else {
    return 0;
  }

}

SMOP__Object*   SMOP__NATIVE__capture_delegate(SMOP__Object* interpreter,
                                               SMOP__Object* invocant,
                                               SMOP__Object* original_capture) {
  smop_lowlevel_rdlock(original_capture);
  SMOP__Object** positional = ((native_capture_struct*)original_capture)->positional;
  SMOP__Object** named = ((native_capture_struct*)original_capture)->named;
  smop_lowlevel_unlock(original_capture);
  return SMOP__NATIVE__capture_create(interpreter,invocant,positional,named);
}
