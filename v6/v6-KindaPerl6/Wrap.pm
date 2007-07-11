#!/usr/bin/perl
use strict;use warnings;
package Wrap;
use Exporter 'import';
our @EXPORT=qw(use5);
sub to5 {
    my $what = shift;
    return $what->{_dispatch}($what,'p5landish');
}
sub use5 {
    my ($name,) = @_;
    return {_dispatch=>sub {
        my ($self,$method,@arguments) = @_;
        $name->$method(map {to5 $_} @arguments);
    }};
}
1;
