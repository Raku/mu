
package Perldoc::DOM;
use strict;
use warnings;
use Spiffy -Base;

use base 'Perldoc::Sender';
use base 'Perldoc::Receiver';

use Perldoc::DOM::Node;
use Perldoc::DOM::Element;
use Perldoc::DOM::PI;
use Perldoc::DOM::Text;

=head1 NAME

Perldoc::DOM - Represent a Perldoc document, DOM-style

=head1 SYNOPSIS

 $kwoc = new Perldoc::DOM();

 my $body = $kwoc->root();
 my @next = $body->daughters();

 my $node = $kwoc->klink("S09#//para/");  # KLINK lookup

=head1 DESCRIPTION

A Perldoc::DOM is a directed acyclic graph, which is a Computer
Scientist's way of saying "tree" (cue: the Fast Show "aliens that say
'tree' skit").

=head1 CREATING A Perldoc::DOM TREE

C<Perldoc::DOM> trees are seldom created using the C<Tree::DAG_Node>
interface.

Normally, they will be constructed as a series of events fired in by a
L<Perldoc::Sender>, such as another L<Perldoc::DOM>, a
L<Perldoc::Preprocessor>, or a L<Perldoc::Parser>.

=cut

field 'root';  # is "Perldoc::DOM::Element"

sub new {
    my $class = ref $self || $self;

    $self = super;

    $self->root(Perldoc::DOM::Element->new({name => "pod"}));

    return $self;
}

sub emit_to {

}

1;

