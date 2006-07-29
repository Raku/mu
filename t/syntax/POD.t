use v6-alpha;

use Test;

plan 8;

# L<S02/"Literals">
# L<S02/"Molecules" /Multiline comments/>
# others will be added later, or you can do it.

eval_ok '
=begin DATA LABEL1
LABEL1.1
LABEL1.2
LABEL1.3
=end DATA

=begin DATA LABEL2
LABEL2.1
LABEL2.2
=end DATA
', "=begin DATA works", :todo;

eval_is('%=DATA<LABEL1>[0]', 'LABEL1.1', '@=DATA<LABEL1>[0] is correct', :todo);
eval_is('%=DATA<LABEL1>[2]', 'LABEL1.3', '@=DATA<LABEL1>[2] is correct', :todo);
eval_is('~ %=DATA<LABEL1>', 'LABEL1.1LABEL1.2LABEL1.3', '~ %=DATA<LABEL1> is correct', :todo);

eval_is('~ $=LABEL2', 'LABEL2.1LABEL2.2', '~ $=LABEL2 is correct', :todo);
eval_is('$=LABEL2[1]', 'LABEL2.2', '$=LABEL2[1] is correct', :todo);

# S02 says this "=begin comment" "=end comment" shouldn't need a "=cut"
# to indicate the end of the pod.

eval_is( '
=begin comment

This is a comment with a "=cut".

=end comment
=cut

return "foo";
', 'foo');

eval_is( '
=begin comment

This is a comment without a "=cut".

=end comment

return "foo";
', 'foo');

