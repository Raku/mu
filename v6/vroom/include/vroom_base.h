
#ifndef VROOM_BASE_H
#define VROOM_BASE_H

// forward declarations
struct VROOM__Object; typedef struct VROOM__Object VROOM__Object;
struct VROOM__ResponderInterface; typedef struct VROOM__ResponderInterface VROOM__ResponderInterface;

/*
 * The VROOM__Object struct represents any object in the VROOM
 * runtime.  The data of this object should be opaque for the users,
 * the only one that should know about it is the responder
 * interface. Every object must have a responder interface. If it
 * doesn't, it is considered itself as one.
 */
struct VROOM__Object {
  VROOM__ResponderInterface* WHO;
};

/*
 * The VROOM__ResponderInterface represents the WHO of any object. It
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
struct VROOM__ResponderInterface {
  VROOM__ResponderInterface* WHO;
  VROOM__Object* (*MESSAGE)  (VROOM__Object* stack,
                              VROOM__ResponderInterface* self,
                              VROOM__Object* identifier,
                              VROOM__Object* capture);
  VROOM__Object* (*REFERENCE)(VROOM__ResponderInterface* self,
                              VROOM__Object* object);
  VROOM__Object* (*RELEASE)  (VROOM__ResponderInterface* self,
                              VROOM__Object* object);
}

/* Every object in YAP6 must be binary compatible with one of these
 * two structures. Given that, not necessarly needs to be created by
 * vroom itself, they don't even need to be managed by vroom. Each
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
#define VROOM_WHO(object) ((VROOM__ResponderInterface*)(((VROOM__Object*)object)->WHO)?(((VROOM__Object*)object)->WHO):(object))

#define VROOM_DISPATCH(stack, object, identifier, capture) \
      (((VROOM__ResponderInterface*)object)->MESSAGE(  \
          stack, ((VROOM__ResponderInterface*)object), \
          identifier, capture \
      ))

#define VROOM_REFERENCE(object) \
      (((VROOM__ResponderInterface*)(((VROOM__Object*)object)->WHO)?(((VROOM__Object*)object)->WHO):(object))->REFERENCE(  \
          ((VROOM__ResponderInterface*)(((VROOM__Object*)object)->WHO)?(((VROOM__Object*)object)->WHO):(object)), \
          object \
      ))

#define VROOM_RELEASE(object) \
      (((VROOM__ResponderInterface*)(((VROOM__Object*)object)->WHO)?(((VROOM__Object*)object)->WHO):(object))->RELEASE(  \
          ((VROOM__ResponderInterface*)(((VROOM__Object*)object)->WHO)?(((VROOM__Object*)object)->WHO):(object)), \
          object \
      ))



#endif
