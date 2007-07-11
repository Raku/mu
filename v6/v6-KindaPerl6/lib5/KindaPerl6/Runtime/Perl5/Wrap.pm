#!/usr/bin/perl
use strict;use warnings;
package KindaPerl6::Runtime::Perl5::Wrap;
use Exporter 'import';
our @EXPORT=qw(use5);
sub to5 {
    my $what = shift;
    $what->{_dispatch}($what,'p5landish');
}
sub to6 {
    my $what = shift;
    if (ref $what) {
        {_value=>$what,
         _dispatch=>sub {
            my ($self,$method,@arguments) = @_;
            warn ".$method\n";
            #$self->
        }}
    }
}
sub use5 {
    my ($name,) = @_;
    {_dispatch=>sub {
        my ($self,$method,@arguments) = @_;
        to6 $name->$method(map {to5 $_} @arguments);
    }};
}
1;
