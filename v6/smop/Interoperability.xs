#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"


#include <smop/base.h>
#include <smop/native.h>
#include <smop/s1p.h>
#include <smop/s0native.h>
#include <smop/mold.h>
#include <smop/p5.h>
#include <smop/capture.h>

SMOP__Object* SMOP__ID__goto;
SMOP__Object* SMOP__ID__setr;
MODULE = SMOP::Interoperability		PACKAGE = SMOP::Interoperability

BOOT:
    SMOP__ID__goto = SMOP__NATIVE__idconst_create("goto");
    SMOP__ID__setr = SMOP__NATIVE__idconst_create("setr");

void
goto_back(SV* ret)
  CODE:
    SMOP__Object* interpreter = SMOP__P5__smop_interpreter;


    SMOP__Object* wrapped_sv =  SMOP__P5__SV_create(SMOP__P5__smop_interpreter,(SMOP_REFERENCE(interpreter,SMOP__P5__smop_p5interpreter)),ret);

    SMOP_DISPATCH(interpreter,SMOP_RI(SMOP__P5__current_back),SMOP__ID__setr,
      SMOP__NATIVE__capture_create(interpreter,
        (SMOP__Object*[]) {
          SMOP_REFERENCE(interpreter,SMOP__P5__current_back),
          wrapped_sv,
          NULL
        },
        (SMOP__Object*[]) { NULL }
    ));

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter), SMOP__ID__goto,SMOP__NATIVE__capture_create(interpreter,(SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter),SMOP_REFERENCE(interpreter,SMOP__P5__current_back) , NULL}, (SMOP__Object*[]) {NULL}));


    SMOP__P5__transfer_to_main_coro(aTHX_ interpreter);

MODULE = SMOP::Interoperability       PACKAGE = SMOP::Object

SV*
AUTOLOAD(SV* self, ...)
  CODE:
    SV* value = SvRV(self);
    SMOP__Object* object = (SMOP__Object*)SvIV(value);    

    int len = SvCUR(cv);
    char* identifier = SvPVX(cv);

    SMOP__Object* interpreter = SMOP__P5__smop_interpreter;

    SMOP__Object* ret = SMOP_DISPATCH(interpreter,SMOP_RI(object),SMOP__NATIVE__idconst_createn(identifier,len),SMOP__NATIVE__capture_create(interpreter,(SMOP__Object*[]) {SMOP_REFERENCE(interpreter,object),NULL},(SMOP__Object*[]) {NULL}));


    RETVAL = SMOP__Object2SV(interpreter,aTHX_ ret);
    SMOP__P5__result_sv = RETVAL;

    SMOP__P5__transfer_to_main_coro(aTHX_ interpreter);

  OUTPUT:
    RETVAL
 

void DESTROY(SV* self)
  CODE:
    SV* value = SvRV(self);
    SMOP__Object* object = (SMOP__Object*)SvIV(value);    
    SMOP__Object* interpreter = SMOP__P5__smop_interpreter;
    SMOP_RELEASE(interpreter,object);
    SMOP__P5__transfer_to_main_coro(aTHX_ interpreter);


MODULE = SMOP::Interoperability       PACKAGE = SMOP::NATIVE::bool

SV*
true(...)
  CODE:
    SV* pointer = newSViv(PTR2IV(SMOP__NATIVE__bool_true));
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
false(...)
  CODE:
    SV* pointer = newSViv(PTR2IV(SMOP__NATIVE__bool_false));
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

int fetch(SV* self)
  CODE:
    SV* value = SvRV(self);
    SMOP__Object* object = INT2PTR(SMOP__Object*,SvIV(value));
    RETVAL = (object != SMOP__NATIVE__bool_false);
  OUTPUT:
    RETVAL

MODULE = SMOP::Interoperability       PACKAGE = SMOP::NATIVE::int

SV*
create(SV* p5class, int i)
  CODE:
    SV* pointer = newSViv(PTR2IV(SMOP__NATIVE__int_create(i)));
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
    int foo = SMOP__NATIVE__int_fetch(object);
    RETVAL = foo;
  OUTPUT:
    RETVAL

MODULE = SMOP::Interoperability       PACKAGE = SMOP::NATIVE::idconst

SV*
create(SV* p5class, char* val)
  CODE:
    SV* pointer = newSViv(PTR2IV(SMOP__NATIVE__idconst_create(val)));
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

char*
fetch(SV* self)
  CODE:
    SV* value = SvRV(self);
    SMOP__Object* object = INT2PTR(SMOP__Object*,SvIV(value));
    if (SMOP_RI(object) == SMOP_RI(SMOP__ID__goto)) {
        int retsize;
        RETVAL = SMOP__NATIVE__idconst_fetch_with_null(object, &retsize);
    } else {
        printf("Calling SMOP::NATIVE::idconst::fetch on a non idconst.\n");
        RETVAL = 0;
    }
  OUTPUT:
    RETVAL

