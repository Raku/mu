#!/usr/bin/perl
package KindaPerl6::Runtime::Perl5::Wrap;
use strict;
use warnings;

sub import {
    my $caller = caller;

    no strict 'refs';
    *{"$caller\::use5"} = \&use5;
}

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

=begin

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
