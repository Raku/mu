#include <stdio.h>
#ifndef SMOP_BASE_H
#define SMOP_BASE_H

// forward declarations
struct SMOP__Object; typedef struct SMOP__Object SMOP__Object;
struct SMOP__ResponderInterface; typedef struct SMOP__ResponderInterface SMOP__ResponderInterface;

/*
 * The SMOP__Object struct represents any object in the SMOP
 * runtime.  The data of this object should be opaque for the users,
 * the only one that should know about it is the responder
 * interface. Every object must have a responder interface. If it
 * doesn't, it is considered itself as one.
 */
#define SMOP__Object__BASE        \
  SMOP__ResponderInterface* RI;   \
  void* data;

struct SMOP__Object {
  SMOP__Object__BASE
};

/*
 * The SMOP__ResponderInterface represents the RI of any object. It
 * understands the object layout to know how to access the members
 * defined in the object, dispatching the methods.  This finish the
 * basic set, as the interpreter delegates to the responder interface the
 * message.
 *
 * MESSAGE is the only low-level definition on a responder interface,
 * everything else is handled the same way in the low-level as it is
 * in the high-level.
 *
 * REFERENCE is called every time an object is referenced in some
 * other place.
 *
 * RELEASE is called every time an reference is released.
 *
 * This doesn't mean that every object need to be refcounted, but
 * without it it would be impossible to implement a refcount gc. Both
 * methods return the input pointer.
 */
#define SMOP__ResponderInterface__BASE                            \
  SMOP__Object* (*MESSAGE)  (SMOP__Object* interpreter,           \
                             SMOP__ResponderInterface* self,      \
                             SMOP__Object* identifier,            \
                             SMOP__Object* capture);              \
  SMOP__Object* (*REFERENCE)(SMOP__Object* interpreter,           \
                             SMOP__ResponderInterface* self,      \
                             SMOP__Object* object);               \
  SMOP__Object* (*RELEASE)  (SMOP__Object* interpreter,           \
                             SMOP__ResponderInterface* self,      \
                             SMOP__Object* object);               \
  char* id;


struct SMOP__ResponderInterface {
  SMOP__Object__BASE
  SMOP__ResponderInterface__BASE
};

/* Every object in SMOP must be binary compatible with one of these
 * two structures. Given that, not necessarly needs to be created by
 * smop itself, they don't even need to be managed by smop. Each
 * ResponderInterface implementation can decide how to deal with issues like
 * garbage collection and so on, as every object interaction is
 * intermediated by a ResponderInterface MESSAGE. But to support basic
 * refcounting, two other metaclass methods must exist. They can be
 * no-ops for some metaclasses, but the interpreter will always call
 * them.
 */

/* 
 * Here follows the basic macros for that triade.
 */
#define SMOP_RI(object) ((SMOP__ResponderInterface*)((((SMOP__Object*)object)->RI)?(((SMOP__Object*)object)->RI):((SMOP__ResponderInterface*)object)))

#ifdef SMOP_HUNT_NULLS
#include <assert.h>
#define SMOP_DISPATCH(interpreter, object, identifier, capture) ({\
      SMOP__Object* ret = (((SMOP__ResponderInterface*)object)->MESSAGE(  \
          (SMOP__Object*)interpreter, ((SMOP__ResponderInterface*)object), \
          identifier, capture \
      ));\
      assert(ret);\
      ret;\
      })
#else
#define SMOP_DISPATCH(interpreter, object, identifier, capture) \
      (((SMOP__ResponderInterface*)object)->MESSAGE(  \
          (SMOP__Object*)interpreter, ((SMOP__ResponderInterface*)object), \
          identifier, capture \
      ))
#endif

#ifdef SMOP_LOWLEVEL_MEM_DEBUG
#define SMOP_REFERENCE(interpreter, object) \
      (fprintf(stderr,"[SMOP_LOWLEVEL_MEM_DEBUG] ++ (%p) %s:%d (%s)\n",object,__FILE__,__LINE__,__func__),\
      (((SMOP__ResponderInterface*)(((SMOP__Object*)object)->RI)?(((SMOP__Object*)object)->RI):((SMOP__ResponderInterface*)object))->REFERENCE( (SMOP__Object*)interpreter, \
          ((SMOP__ResponderInterface*)(((SMOP__Object*)object)->RI)?(((SMOP__Object*)object)->RI):((SMOP__ResponderInterface*)object)), \
          (SMOP__Object*)object \
      )))

#define SMOP_RELEASE(interpreter, object) \
      (fprintf(stderr,"[SMOP_LOWLEVEL_MEM_DEBUG] -- (%p) %s:%d (%s)\n",object,__FILE__,__LINE__,__func__),\
      (((SMOP__ResponderInterface*)(((SMOP__Object*)object)->RI)?(((SMOP__Object*)object)->RI):((SMOP__ResponderInterface*)object))->RELEASE( (SMOP__Object*)interpreter, \
          ((SMOP__ResponderInterface*)(((SMOP__Object*)object)->RI)?(((SMOP__Object*)object)->RI):((SMOP__ResponderInterface*)object)), \
          (SMOP__Object*)object \
      )))
#else
#define SMOP_REFERENCE(interpreter, object) \
      (((SMOP__ResponderInterface*)(((SMOP__Object*)object)->RI)?(((SMOP__Object*)object)->RI):((SMOP__ResponderInterface*)object))->REFERENCE( (SMOP__Object*)interpreter, \
          ((SMOP__ResponderInterface*)(((SMOP__Object*)object)->RI)?(((SMOP__Object*)object)->RI):((SMOP__ResponderInterface*)object)), \
          (SMOP__Object*)object \
      ))
#define SMOP_RELEASE(interpreter, object) \
      (((SMOP__ResponderInterface*)(((SMOP__Object*)object)->RI)?(((SMOP__Object*)object)->RI):((SMOP__ResponderInterface*)object))->RELEASE( (SMOP__Object*)interpreter, \
          ((SMOP__ResponderInterface*)(((SMOP__Object*)object)->RI)?(((SMOP__Object*)object)->RI):((SMOP__ResponderInterface*)object)), \
          (SMOP__Object*)object \
      ))
#endif


#endif
