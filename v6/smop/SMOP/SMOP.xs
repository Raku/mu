#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

#include <smop.h>
#include <smop_native.h>

MODULE = SMOP		PACKAGE = SMOP		

BOOT:
smop_init();

MODULE = SMOP       PACKAGE = SMOP::Object

SV*
AUTOLOAD(SV* self, ...)
  CODE:

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