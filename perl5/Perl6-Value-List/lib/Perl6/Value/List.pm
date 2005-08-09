package Perl6::Value::List;

use strict;
our $VERSION = '0.01';
use constant Inf => 100**100**100;

sub new () {
    my $class = shift;
    my %param = @_;
    my $start = delete $param{start}; $start = sub{}    unless defined $start;
    my $end =   delete $param{end};   $end =   sub{}    unless defined $end;
    my $elems = delete $param{elems}; $elems = sub{ 0 } unless defined $elems;
    die 'invalid parameters' if keys( %param );
    return bless { start =>  $start, 
                   end =>    $end,
                   elems =>  $elems }, $class;
}

sub elems () { $_[0]->{elems}() }

sub shift () {
    my $self = shift;
    return if $self->elems <= 0;
    return $self->{start}();
}

sub pop () {
    my $self = shift;
    return if $self->elems <= 0;
    return $self->{end}();
}

1;
__END__

=head1 NAME

Perl6::Value::List - Perl extension for Perl6-like "List"

=head1 SYNOPSIS

  use Perl6::Value::List;
  
  my $list = Perl6::Value::List.new( ... );

=head1 DESCRIPTION

This module implements a "List" object.

new() without parameters is an empty list.

=head1 SEE ALSO

Pugs

=head1 AUTHOR

Flavio S. Glock, E<lt>fglock@Egmail.com<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Flavio S. Glock

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.


=cut
