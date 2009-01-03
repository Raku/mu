#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

#include <smop.h>
#include <smop_native.h>
#include <smop_s1p.h>
#include <smop_identifiers.h>
#include <smop_mold.h>
#include <smop_p5.h>

MODULE = SMOP		PACKAGE = SMOP		

void
goto_back(SV* ret)
  CODE:
    SMOP__Object* interpreter = SMOP__P5__smop_interpreter;
    *SMOP__P5__current_coro_has_next = 0;


    SMOP_DISPATCH(interpreter,SMOP_RI(SMOP__P5__current_back),SMOP__ID__setr,SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,SMOP__P5__current_back),(SMOP__Object*[]) {SMOP__P5__SV_create(SMOP__P5__smop_interpreter,SMOP_REFERENCE(interpreter,SMOP__P5__smop_p5interpreter),ret),NULL},NULL));

    SMOP_DISPATCH(interpreter,SMOP_RI(interpreter),SMOP__ID__goto,SMOP_REFERENCE(interpreter,SMOP__P5__current_back));


    SMOP__P5__transfer_to_main_coro(interpreter,my_perl);

MODULE = SMOP       PACKAGE = SMOP::Object

SV*
AUTOLOAD(SV* self, ...)
  CODE:
    SV* value = SvRV(self);
    SMOP__Object* object = (SMOP__Object*)SvIV(value);    

    int len = SvCUR(cv);
    char* identifier = SvPVX(cv);

    SMOP__Object* interpreter = SMOP__P5__smop_interpreter;

    SMOP__Object* ret = SMOP_DISPATCH(interpreter,SMOP_RI(object),SMOP__NATIVE__idconst_createn(identifier,len),SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,object),NULL,NULL));


    RETVAL = SMOP__Object2SV(interpreter,my_perl,ret);
    SMOP__P5__result_sv = RETVAL;

    SMOP__P5__transfer_to_main_coro(interpreter,my_perl);

  OUTPUT:
    RETVAL
 
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

int fetch(SV* self)
  CODE:
    SV* value = SvRV(self);
    SMOP__Object* object = (SMOP__Object*)SvIV(value);
    RETVAL = (object != SMOP__NATIVE__bool_false);
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
        int foo = SMOP__NATIVE__int_fetch(object);
        RETVAL = foo;
    } else {
        printf("Calling SMOP::NATIVE::int::fetch on a non-native int.\n");
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
        RETVAL = SMOP__NATIVE__idconst_fetch_with_null(object, &retsize);
    } else {
        printf("Calling SMOP::NATIVE::idconst::fetch on a non idconst.\n");
        RETVAL = 0;
    }
  OUTPUT:
    RETVAL

MODULE = SMOP       PACKAGE = SMOP::Mold

SV*
create(SV* p5class, int ccount, SV* consts, SV* bytecode)
  CODE:
    AV* constsav = (AV*)SvRV(consts);
    int constslen = av_len(constsav) + 1;
    SMOP__Object** consts_arr = calloc(constslen+1,sizeof(void*));
    int i;
    for (i = 0; i < constslen; i++) {
        SV** e = av_fetch(constsav,i,0);
        SMOP__Object* object;
        if (SvROK(*e)) {
            SV* value = SvRV(*e);
            object = SMOP_REFERENCE(SMOP__GlobalInterpreter, (SMOP__Object*)SvIV(value));
        } else if (SvIOK(*e)) {
            object = SMOP__NATIVE__int_create(SvIV(*e));
        } else if (SvPOK(*e)) {
            STRLEN len;
            char* p = SvPV(*e,len);
            object = SMOP__NATIVE__idconst_createn(p,len);
        } else {
            printf("Unknown value sent to mold->create\n");
            object = SMOP__NATIVE__bool_false;
        }
        consts_arr[i] = object;
    }
    AV* codeav = (AV*)SvRV(bytecode);
    int codelen = av_len(codeav) + 1;
    int* code_arr = calloc(codelen+1,sizeof(void*));
    for (i = 0; i < codelen; i++) {
        SV** e = av_fetch(codeav,i,0);
        code_arr[i] = SvIV(*e);
    }
    SMOP__Object* mold = SMOP__Mold_create(ccount, consts_arr, codelen, code_arr);
    SV* pointer = newSViv((int)mold);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

MODULE = SMOP       PACKAGE = SMOP::MoldFrame

SV*
create(SV* p5class, SV* moldrv)
  CODE:
    SV* value = SvRV(moldrv);
    SMOP__Object* mold = (SMOP__Object*)SvIV(value);
    SMOP__Object* frame = SMOP__Mold__Frame_create(SMOP__GlobalInterpreter, SMOP_REFERENCE(SMOP__GlobalInterpreter,mold));
    SV* pointer = newSViv((int)frame);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL    

MODULE = SMOP       PACKAGE = SMOP::Interpreter

SV*
run(SV* p5class, SV* continuation)
  CODE:
    SV* value = SvRV(continuation);
    SMOP__Object* object = (SMOP__Object*)SvIV(value);
    SMOP_DISPATCH(SMOP__GlobalInterpreter, SMOP_RI(SMOP__GlobalInterpreter),
                  SMOP__ID__goto, SMOP__NATIVE__capture_create(SMOP__GlobalInterpreter,
                                                               SMOP_REFERENCE(SMOP__GlobalInterpreter, SMOP__GlobalInterpreter),
                                                               (SMOP__Object*[]){SMOP_REFERENCE(SMOP__GlobalInterpreter,object),NULL},
                                                               NULL));
    SMOP_DISPATCH(SMOP__GlobalInterpreter, SMOP_RI(SMOP__GlobalInterpreter),
                  SMOP__ID__loop, SMOP__NATIVE__capture_create(SMOP__GlobalInterpreter,
                                                               SMOP_REFERENCE(SMOP__GlobalInterpreter, SMOP__GlobalInterpreter),
                                                               NULL, NULL));


MODULE = SMOP       PACKAGE = SMOP::S1P

SV*
Scalar(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__Scalar);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
Hash_BValue(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__Hash_BValue);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
Array(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__Array);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
Array_Iterator(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__Array_Iterator);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
EndOfIterator(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__EndOfIterator);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
Str(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__Str);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
Hash(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__Hash);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
Code(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__Code);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
CCode(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__CCode);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
map(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__map);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
grep(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__grep);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
Multi(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__Multi);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
Attribute(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__Attribute);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
Method(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__Method);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
RootNamespace(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__RootNamespace);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
LexicalPrelude(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__LexicalPrelude);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
Capturize(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__Capturize);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
EmptyList(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__EmptyList);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
LexicalScope(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__LexicalScope);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
DefaultBlockSignature(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__DefaultBlockSignature);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
BindCaptureSignature(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__BindCaptureSignature);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
AdhocSignature(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__AdhocSignature);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
PurePrototypeHow(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__PurePrototypeHow);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL

SV*
ritest(SV* p5class)
  CODE:
    SV* pointer = newSViv((int)SMOP__S1P__ritest);
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("SMOP::Object", 0);
    RETVAL = sv_bless(object, class);
  OUTPUT:
    RETVAL
