#ifndef SMOP_OO_H
#define SMOP_OO_H

/* See
 * http://www.perlfoundation.org/perl6/index.cgi?smop_oo_api
 */

/* This is the p6opaque responder interface
 * http://www.perlfoundation.org/perl6/index.cgi?smop_p6opaque_implementation
 */
extern SMOP__Object* SMOP__p6opaque__RI;

/* This is the default metaclass implementation
 * http://www.perlfoundation.org/perl6/index.cgi?smop_default_metaclass
 */
extern SMOP__Object* SMOP__MetaClass__Default;

extern SMOP__Object* SMOP__OO__LOWL__Method;
SMOP__Object* SMOP__OO__LOWL__Method_create(int multi,
                                            SMOP__Object* name,
                                            SMOP__Object* signature,
                                            SMOP__Object* (*code) (SMOP__Object* interpreter,
                                                                   SMOP__Object* method,
                                                                   SMOP__Object* capture));

extern SMOP__Object* SMOP__OO__LOWL__SubMethod;
SMOP__Object* SMOP__OO__LOWL__SubMethod_create(int multi,
                                               SMOP__Object* name,
                                               SMOP__Object* signature,
                                               SMOP__Object* (*code) (SMOP__Object* interpreter,
                                                                      SMOP__Object* method,
                                                                      SMOP__Object* capture));

extern SMOP__Object* SMOP__OO__LOWL__Signature;
extern SMOP__Object* SMOP__OO__LOWL__Attribute;
extern SMOP__Object* SMOP__OO__LOWL__Package;

#endif
