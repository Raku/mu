use v6;

module ArrayInit;

=begin kwid

This module is a helper to init-in-exported-sub.t -- masak 2007-09-02

=end kwid

sub array_init() is export {
    my @array;
    push @array, 'just one element';
    return ~@array;
}
