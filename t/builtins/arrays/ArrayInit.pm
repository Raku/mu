use v6;

module ArrayInit;

=kwid

This module is a helper to init-in-exported-sub.t -- masak 2007-09-02

=cut

sub array_init() is export {
    my @array;
    push @array, 'just one element';
    return ~@array;
}
