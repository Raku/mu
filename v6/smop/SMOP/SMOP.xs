#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

#include <smop.h>
#include <smop_native.h>
#include <smop_s1p.h>
#include <smop_identifiers.h>

MODULE = SMOP		PACKAGE = SMOP		

BOOT:
smop_init();

MODULE = SMOP       PACKAGE = SMOP::Object

void
DESTROY(SV* self, ...)
  CODE:
    SV* value = SvRV(self);
    SMOP__Object* object = (SMOP__Object*)SvIV(value);    
    SMOP_RELEASE(SMOP__GlobalInterpreter,object);
    SMOP_DISPATCH(SMOP__GlobalInterpreter, SMOP_RI(SMOP__GlobalInterpreter),
                  SMOP__ID__loop, SMOP__NATIVE__capture_create(SMOP__GlobalInterpreter,
                                                               SMOP_REFERENCE(SMOP__GlobalInterpreter, SMOP__GlobalInterpreter),
                                                               NULL, NULL));

SV*
__dispatch(char* name, SV* self, AV* positional, HV* named)
  CODE:
    printf("TODO!\n");

MODULE = SMOP       PACKAGE = SMOP::NATIVE::bool

SV*
true(...)
  CODE:
    SV* pointer = newSViv((int)SMOP__NATIVE__bool_true);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
false(...)
  CODE:
    SV* pointer = newSViv((int)SMOP__NATIVE__bool_false);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

MODULE = SMOP       PACKAGE = SMOP::NATIVE::int

SV*
create(SV* p5class, int i)
  CODE:
    SV* pointer = newSViv((int)SMOP__NATIVE__int_create(i));
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

int
fetch(SV* self)
  CODE:
    SV* value = SvRV(self);
    SMOP__Object* object = (SMOP__Object*)SvIV(value);
    if (SMOP_RI(object) == (SMOP__ResponderInterface*)SMOP__NATIVE__int) {
        RETVAL = SMOP__NATIVE__int_fetch(object);
    } else {
        printf("Calling SMOP::NATIVE::int->fetch on a non-native int.\n");
        RETVAL = 0;
    }
  OUTPUT:
    RETVAL

MODULE = SMOP       PACKAGE = SMOP::NATIVE::idconst

SV*
create(SV* p5class, char* val)
  CODE:
    SV* pointer = newSViv((int)SMOP__NATIVE__idconst_create(val));
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

char*
fetch(SV* self)
  CODE:
    SV* value = SvRV(self);
    SMOP__Object* object = (SMOP__Object*)SvIV(value);
    if (SMOP_RI(object) == SMOP_RI(SMOP__ID__new)) {
        int retsize;
        RETVAL = SMOP__NATIVE__idconst_fetch(object, &retsize);
    } else {
        printf("Calling SMOP::NATIVE::int->fetch on a non-native int.\n");
        RETVAL = 0;
    }
  OUTPUT:
    RETVAL
