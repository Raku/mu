
package Perl6::SubMethod;

use strict;
use warnings;

use base 'Perl6::Method';

1;

__END__

=pod

=head1 NAME

Perl6::SubMethod - Submethods in the Perl 6 Meta Model

=head1 DESCRIPTION

From Synopsis 12/Submethods

Apart from the keyword, submethod declaration and call syntax is identical to 
method syntax. You may mix methods and submethods of the same name within the 
class hierarchy, but only the methods are visible to derived classes via 
inheritance. A submethod is called only when a method call is dispatched 
directly to the current class.

??? do method foo()  and submethod foo() both get called ???

=head1 SUPERCLASS

=over 4

=item I<Perl6::Method>

=back

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
