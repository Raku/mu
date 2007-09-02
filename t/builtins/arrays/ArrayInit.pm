use v6-alpha;

module ArrayInit;

=kwid

This module isn't good for anything, but I needed to put it somewhere for the
test file t/builtins/arrays/init-in-exported-sub.t to find it. If you are
reading this and know a better way for tests to import modules without
contaminating ext/, please go ahead and move things around. We're all
operating at our respective levels of -fu here, and my managing-a-code-repo-fu
is low in this respect. -- masak 2007-09-02

=cut

sub array_init() is export {
    my @array;
    push @array, 'just one element';
    return ~@array;
}
