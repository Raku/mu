
#ifndef YAP6_BASE_H
#define YAP6_BASE_H

// forward declarations
struct YAP6__Object; typedef struct YAP6__Object YAP6__Object;
struct YAP6__Prototype; typedef struct YAP6__Prototype YAP6__Prototype;
struct YAP6__MetaClass; typedef struct YAP6__MetaClass YAP6__MetaClass;

/*
 * The YAP6__Object struct represents any object in the YAP6 runtime.
 * The data of this object should be opaque for the users, the only
 * ones that should know about it are the prototype and the
 * metaclass. Every object must have a prototype. If it doesn't, it is
 * considered itself as one. The 'repr' member is where the metaclass
 * handles the object layout in interaction with the prototype to
 * implement the object logic.
 */
struct YAP6__Object {
  void* repr;
  YAP6__Prototype* WHAT;
};

/*
 * The YAP6__Prototype struct represents a prototype object. Every
 * object have a prototype unless it is itself one.  The prototype
 * have the object implementation, but only the metaclass knows HOW it
 * is laid out. And the metaclass is the way to fetch it.  A prototype
 * without a metaclass is a metaclass.
 */
struct YAP6__Prototype {
  void* repr;
  YAP6__Prototype* WHAT;
  YAP6__MetaClass* HOW;
};

/*
 * The YAP6__MetaClass represents the HOW of any object. It
 * understands the object layout to know how to access the methods
 * defined in the prototype, dispatching the methods.  This finish the
 * basic triade, as the interpreter delegates to the metaclass the
 * message.  REFERENCE is called every time an object is referenced in
 * some other place. RELEASE is called every time an reference is
 * released. This doesn't mean that every object need to be
 * refcounted, but without it it would be impossible to implement a
 * refcount gc. Both methods return the input pointer.
 */
struct YAP6__MetaClass {
  void* repr;
  YAP6__Prototype* WHAT;
  YAP6__MetaClass* HOW;
  YAP6__Object* (*MESSAGE)   (YAP6__Object* stack,
                              YAP6__MetaClass* self,
                              YAP6__Object* identifier,
                              YAP6__Object* capture);
  YAP6__Object* (*REFERENCE) (YAP6__MetaClass* self,
                              YAP6__Object* object);
  YAP6__Object* (*RELEASE)   (YAP6__MetaClass* self,
                              YAP6__Object* object);
}

/* Every object in YAP6 must be binary compatible with one of these
 * three. Given that, not necessarly needs to be created by yap6
 * itself, they don't even need to be managed by YAP6. Each MetaClass
 * implementation can decide how to deal with issues like garbage
 * collection and so on, as every object interaction is intermediated
 * by a MetaClass MESSAGE. But to support basic refcounting, two other
 * metaclass methods must exist. They can be no-ops for some
 * metaclasses, but the interpreter will always call them.
 */

/* 
 * Here follows the basic macros for that triade.
 */
#define YAP6_WHAT(object) ((YAP6__Prototype*)((((YAP6__Object*)object)->WHAT)?(((YAP6__Object*)object)->WHAT):(object)))

#define YAP6_HOW(object) ((YAP6__MetaClass*)( \
                          (((YAP6__Prototype*)((((YAP6__Object*)object)->WHAT)?(((YAP6__Object*)object)->WHAT):(object)))->HOW) ? \
                          (((YAP6__Prototype*)((((YAP6__Object*)object)->WHAT)?(((YAP6__Object*)object)->WHAT):(object)))->HOW) : \
                          (object)\
                         ))

#define YAP6_DISPATCH(stack, metaclass, identifier, capture) ((YAP6__MetaClass*)metaclass)->MESSAGE(stack,(YAP6__MetaClass*)metaclass,\
                                                                                             (YAP6__Object*)identifier,\
                                                                                             (YAP6__Object*)capture)

#define YAP6_REFERENCE(object) (((YAP6__MetaClass*)( \
                          (((YAP6__Prototype*)((((YAP6__Object*)object)->WHAT)?(((YAP6__Object*)object)->WHAT):(object)))->HOW) ? \
                          (((YAP6__Prototype*)((((YAP6__Object*)object)->WHAT)?(((YAP6__Object*)object)->WHAT):(object)))->HOW) : \
                          (object)\
                         ))->REFERENCE(((YAP6__MetaClass*)( \
                          (((YAP6__Prototype*)((((YAP6__Object*)object)->WHAT)?(((YAP6__Object*)object)->WHAT):(object)))->HOW) ? \
                          (((YAP6__Prototype*)((((YAP6__Object*)object)->WHAT)?(((YAP6__Object*)object)->WHAT):(object)))->HOW) : \
                          (object)\
                         )),object))

#define YAP6_RELEASE(object) (((YAP6__MetaClass*)( \
                          (((YAP6__Prototype*)((((YAP6__Object*)object)->WHAT)?(((YAP6__Object*)object)->WHAT):(object)))->HOW) ? \
                          (((YAP6__Prototype*)((((YAP6__Object*)object)->WHAT)?(((YAP6__Object*)object)->WHAT):(object)))->HOW) : \
                          (object)\
                         ))->RELEASE(((YAP6__MetaClass*)( \
                          (((YAP6__Prototype*)((((YAP6__Object*)object)->WHAT)?(((YAP6__Object*)object)->WHAT):(object)))->HOW) ? \
                          (((YAP6__Prototype*)((((YAP6__Object*)object)->WHAT)?(((YAP6__Object*)object)->WHAT):(object)))->HOW) : \
                          (object)\
                         )),object))

#endif
