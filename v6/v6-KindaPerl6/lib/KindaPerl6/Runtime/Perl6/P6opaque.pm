
use v6-alpha;
module Perl6;

$Perl6::P6opaque = {
    _dispatch => 
        (
        -> ( $method, $invocant, $caller? ) {
                if $method eq 'new' {
                    hash( _dispatch => $invocant<_dispatch>, );
                }
                else {  # method eq 'set'
                    -> ( $invocant, $slot, $data? ) {
                            $invocant{$slot} = $data;
                        }
                }
            }
        ),
};


=head1 NAME 

Perl6::P6opaque - a base data structure for Perl6 objects

=head1 SYNOPSIS

  {
    my $p = $Perl6::P6opaque<_dispatch>( 'new', $Perl6::P6opaque );
    say "new = ", $p.perl;
    $p<_dispatch>( 'set', $p ).( $p, '_value', 3 );
    say "after set = ", $p.perl;
    $p<_dispatch>( 'set', $p ).( $p, '_other', 42 );
    say "after set = ", $p.perl;
  }

=head1 DESCRIPTION

This module implements a 'P6opaque' class.

=head2 Methods

* new

* set

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

