
package Kwid::DOM::WS;

use Carp;
use strict;
use warnings;
use base 'Kwid::DOM::Node';

=head1 NAME

Kwid::DOM::WS - ignorable whitespace in a Kwid::DOM tree

=head1 SYNOPSIS

See L<Kwid::DOM::Node>.

=head1 DESCRIPTION

Sometimes you need to put in a little whitespace to fill an XML
document.  This node type is for that.

=head2 SUB-CLASS PROPERTIES

This node type keeps the C<source> property, and adds C<content>,
which is the whitespace to be represented in the normative XML.

=cut

sub content {
    my $self = shift;
    if ( @_ ) {
	# FIXME - unicode :)
	my $content = shift;
	$content =~ /\S/
	    && croak "tried to put non-whitespace in a whitespace node";
	$self->{content} = $content;
    } else {
	$self->{content};
    }
}

1;
