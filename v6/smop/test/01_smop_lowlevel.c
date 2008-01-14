#include <smop.h>
#include <smop_lowlevel.h>

/*
 * In this test we want to test only the smop lowlevel runtime, to do
 * that, we're going to fake a lot of features the lowlevel runtime
 * counts on. Note that it may look weird that the lowlevel runtime
 * requires some features that comes from the high-level, but it's
 * also important to realise that there's a matter of timing
 * also. Which basically means that we know that at the time we need
 * the high-level features, they will be already available.
 *
 * The point where we need it is in the refcnt_dec function, which, on
 * the case of destroying an object, it will take the stack parameter
 * and build a Continuation-Passing-Style call to the object's
 * DESTROYALL method, just before the actual object destruction (by
 * calling free).
 *
 * This way, we must provide a fake stack and node objects and fake
 * method identifiers so the refcnt_dec can actually postpone the
 * object destruction properly. We also need to implement a fake
 * DESTROYALL method on the object itself.
 *
 * The methods we need to fake in this test are:
 *
 *   $obj.DESTROYALL
 *   Node.new
 *   $stack.current
 *   $stack.goto
 *   $stack.continues
 *
 * As in smop_lowlevel.h, the equivalent code for that is:
 *
 * my $continuation = ___STACK___.current();
 * my $first_node = Node.new(result => $continuation);
 * my $second_node = Node.new(responder => ___RI___($obj),
 *                            identifier => "DESTROYALL",
 *                            capture => \($obj: ));
 * $first_node.continuation($second_node);
 * my $third_node = Node.new(responder => SMOP_LOWLEVEL,
 *                           identifier => "FREE",
 *                           capture => \($obj ));
 * $second_node.continuation($third_node);
 * my $fourth_node = Node.new(result => ___STACK___);
 * $third_node.continuation($fourth_node);
 * my $fifth_node = Node.new(responder => SMOP__STACK__Operators,
 *                           identifier => SMOP__STACK__OP_Move_Capturize,
 *        capture => SMOP__STACK__OPCAPTURE_Move_Capturize.new(1,(4),(),1));
 * $fourth_node.continuation($fifth_node);
 * my $sixth_node = Node.new(responder => ___RI___(___STACK___),
 *                           identifier => "goto");
 * $fifth_node.continuation($sixth_node);
 * ___STACK___.goto($first_node);
 *
 */

int main(int argc, char** argv) {

  // Let's fake the identifiers.
  

  printf("1..2\n");



  return 0;
}

static SMOP__Object* 
