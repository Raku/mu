=head1 KindaPerl6::Runtime::Perl5::DispatchSugar

=cut

package KindaPerl6::Runtime::Perl5::DispatchSugar;
use strict;
use warnings;

sub import {
    my $caller = caller;

    no strict 'refs';
    *{"$caller\::sugar"} = \&sugar;
}

=head2 sugar($)

sugar accepts a reference of some sort and blesses it into
KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch and returns the reference
of the blessed object

a hash reference should be passed in, otherwise
KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch will break.
( it requires $obj->{ _dispatch } )

TODO: $ref probably should be checked to make sure it is a hash reference.

=cut

sub sugar($) {
    my $ref = shift;
    bless($ref,"KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch");
    return $ref;
}

=head2 sugar_off

turns off sugar(ing).  any sugaring attempts will die with
"dispatch sugar turned off"

Oct 29th, 2007: as far as I can tell, (grep -R), no one is using sugar_off

=cut

sub sugar_off {
    no warnings 'redefine';
    *KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch::AUTOLOAD = sub {die "dispatch sugar turned off"};
}

=head1 KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch

=head1 SYNOPSIS

This package uses AUTOLOAD to call _dispatch in a hash reference in effect does the following

 my $method = sugar {
     _dispatch => sub {
         my ( $self, $subroutine, @args ) = @_
         if ( $subroutine eq 'unknown_sub' ) {
             do_something( @args );
         } elsif { # .. we get switch in perl6!
         }
     }
 };

 $method->unknown_sub(@args);

=head1 INTENTION

to replace $object->{_dispatch}($object,'method_name',...)
with $object->method_name(...)

::DISPATCH does it better

=cut

package KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch;

sub AUTOLOAD {
    my ($package,) = caller();
    #warn(join ("|",caller(),"\n")) if $package ne 'KindaPerl6::Runtime::Perl5::MOP';
    our $AUTOLOAD;
    $AUTOLOAD =~ s/.*:://;
    my ($self,@args) = @_;
    $self->{_dispatch}($self,$AUTOLOAD,@args);
}

sub DESTROY {
}

1;
