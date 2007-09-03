#!/usr/bin/perl
use strict;use warnings;
package KindaPerl6::Runtime::Perl5::Wrap;
use Exporter 'import';
our @EXPORT=qw(use5);
sub to5 {
    return map {
        $_->{_dispatch}($_,'p5landish');
    } @_;
}
sub to6 {
    return map {
        if (ref) {
            {_value=>$_,
            _dispatch=>sub {
                my ($self,$method,@arguments) = @_;
                if ($method eq 'p5landish') {
                    return $self->{_value};
                }
                #use Data::Dump::Streamer;
                #warn ".$method:",Dump($self);
                to6($self->{_value}->$method(to5 @arguments));
            }}
        }
    } @_;
}
sub use5 {
    my ($name,) = @_;
    {_dispatch=>sub {
        my ($self,$method,@arguments) = @_;
        to6 $name->$method(to5 @arguments);
    }};
}
1;
