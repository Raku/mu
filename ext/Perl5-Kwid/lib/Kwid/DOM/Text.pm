
package Kwid::DOM::Text;

use strict;
use warnings;
use base 'Kwid::DOM::Node';

=head1 NAME

Kwid::DOM::Text - text node in a Kwid::DOM tree

=head1 SYNOPSIS

See L<Kwid::DOM::Node>.

=head1 DESCRIPTION

A C<Kwid::DOM::Text> represents a little slice of content in a Kwid
DOM tree.

It has one property - content.

The constructor for this class has a special shortcut syntax compared
to normal C<Kwid::DOM::Node>'s / C<Tree::DAG_Node>'s - instead of
specifying options as a hash;

 Kwid::DOM::Text->new({ content => "foo", source => "foo" });

You can just say;

 Kwid::DOM::Text->new("foo");

(also, the latter form is slightly more efficient, though this is
marginal in string COW environments)

=cut

field 'content';

sub _init {
    my $self = shift;
    my $o = shift;

    $self->content($o->{content}) if exists $o->{content};
}



1;
