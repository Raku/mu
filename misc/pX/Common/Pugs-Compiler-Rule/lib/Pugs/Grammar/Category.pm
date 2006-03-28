package Pugs::Grammar::Category;

# Documentation in the __END__
use 5.006;
use strict;
use warnings;

use Pugs::Grammar::Base;  # not 'use base'
use Pugs::Grammar::Rule;
use Pugs::Runtime::Match;
use Pugs::Emitter::Rule::Perl5;

sub new { $_[0] }

sub add_rule {
    warn "not implemented";
}

sub code { 
    warn "not implemented";
}

sub match {
    warn "not implemented";
}

sub perl5 {
    warn "not implemented";
}

1;

__END__

=head1 NAME 

Pugs::Grammar::Category - Engine for Perl 6 Rule categories

=head1 SYNOPSIS

    use Pugs::Grammar::Category;

    ...

=head1 DESCRIPTION

This module provides an implementation for Perl 6 Rule categories.  

=head1 METHODS

=head2 new ()

Class method.  Returns a category object.

options:

=item * none.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 Rules Spec: L<http://dev.perl.org/perl6/doc/design/syn/S05.html>

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
