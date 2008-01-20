#include <smop.h>


int main() {
  printf("1..9\n");

  smop_init();

  /* This is the equivalent of the following:
   * \(capture: continuation, continues, copy => current, debug => eval);
   */
  SMOP__Object* capture = 
    SMOP__NATIVE__capture_create(SMOP__ID__capture,
                                 (SMOP__Object*[]){ SMOP__ID__continuation,
                                                    SMOP__ID__continues },
                                 (SMOP__Object*[]){ SMOP__ID__copy,
                                                    SMOP__ID__current,
                                                    SMOP__ID__debug,
                                                    SMOP__ID__eval });

  if (!capture) printf("not ");
  printf("ok 1 - capture object created.\n");

  SMOP__Object* foo = SMOP__NATIVE__capture_invocant(capture);

  if (foo != SMOP__ID__capture) printf("not ");
  printf("ok 2 - gets the correct invocant.\n");

  foo = SMOP__NATIVE__capture_positional(capture, 1);
  if (foo != SMOP__ID__continuation) printf("not ");
  printf("ok 3 - gets the first positional.\n");

  foo = SMOP__NATIVE__capture_positional(capture, 2);
  if (foo != SMOP__ID__continues) printf("not ");
  printf("ok 4 - gets the second positional.\n");

  foo = SMOP__NATIVE__capture_positional(capture, 3);
  if (foo) printf("not ");
  printf("ok 5 - return NULL on non-existant positional.\n");

  foo = SMOP__NATIVE__capture_named(capture, SMOP__ID__copy);
  if (foo != SMOP__ID__current) printf("not");
  printf("ok 6 - gets by name.\n");

  foo = SMOP__NATIVE__capture_named(capture, SMOP__ID__debug);
  if (foo != SMOP__ID__eval) printf("not");
  printf("ok 7 - gets by name.\n");

  foo = SMOP__NATIVE__capture_named(capture, SMOP__ID__eval);
  if (foo) printf("not");
  printf("ok 8 - returns NULL on non-existant named.\n");

  SMOP_RELEASE(capture);

  printf("ok 9 - capture object destroyed.\n");

  smop_destr();
}
