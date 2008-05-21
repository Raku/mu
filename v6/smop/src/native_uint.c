#include <stdlib.h>
#include <smop.h>
#include <smop_lowlevel.h>

SMOP__Object *SMOP__NATIVE__uint;

typedef struct smop_native_uint_struct
{
  SMOP__Object__BASE unsigned int uintvalue;
} smop_native_uint_struct;

static SMOP__Object *
uint_message (SMOP__Object * interpreter,
	      SMOP__ResponderInterface * self,
	      SMOP__Object * identifier, SMOP__Object * capture)
{
  // todo
  SMOP_RELEASE (interpreter, capture);
  return SMOP__NATIVE__bool_false;
}

static SMOP__Object *
uint_reference (SMOP__Object * interpreter,
		SMOP__ResponderInterface * responder, SMOP__Object * obj)
{
  if ((SMOP__Object *) responder != obj)
    {
      smop_lowlevel_refcnt_inc (interpreter, responder, obj);
    }
  return obj;
}

static SMOP__Object *
uint_release (SMOP__Object * interpreter,
	      SMOP__ResponderInterface * responder, SMOP__Object * obj)
{
  if ((SMOP__Object *) responder != obj)
    {
      smop_lowlevel_refcnt_dec (interpreter, responder, obj);
    }
  return obj;
}

void
smop_native_uint_init ()
{
  SMOP__NATIVE__uint = calloc (1, sizeof (SMOP__ResponderInterface));
  ((SMOP__ResponderInterface *) SMOP__NATIVE__uint)->MESSAGE = uint_message;
  ((SMOP__ResponderInterface *) SMOP__NATIVE__uint)->REFERENCE =
    uint_reference;
  ((SMOP__ResponderInterface *) SMOP__NATIVE__uint)->RELEASE = uint_release;
  ((SMOP__ResponderInterface *) SMOP__NATIVE__uint)->id = "Native uint";
}

void
smop_native_uint_destr ()
{
  free (SMOP__NATIVE__uint);
}

SMOP__Object *
SMOP__NATIVE__uint_create (unsigned int value)
{
  SMOP__Object *ret = smop_lowlevel_alloc (sizeof (smop_native_uint_struct));
  ret->RI = (SMOP__ResponderInterface *) SMOP__NATIVE__uint;
  ((smop_native_uint_struct *) ret)->uintvalue = value;
  return ret;
}

unsigned int
SMOP__NATIVE__uint_fetch (SMOP__Object * value)
{
  smop_lowlevel_rdlock (value);
  unsigned int v = ((smop_native_uint_struct *) value)->uintvalue;
  smop_lowlevel_unlock (value);
  return v;
}
