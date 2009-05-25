
use v6;

class Perldoc::DOM;

#is Perldoc::Sender;
#is Perldoc::Receiver;

use Perldoc::DOM::Node;
use Perldoc::DOM::Element;
use Perldoc::DOM::PI;
use Perldoc::DOM::WS;
use Perldoc::DOM::Text;

has Perldoc::DOM::Element $.root;

method BUILD($self:) {
    $self.root ||= Perldoc::DOM::Element.new(:name<pod>);
}

=head1 NAME

Perldoc::DOM - Represent a Perldoc document, DOM-style

=head1 SYNOPSIS

 use Perldoc::DOM;

 $kwoc = Perldoc::DOM.new();

 my $body = $kwoc.root;
 my @next = $body.children;

 my $node = $kwoc.lookup("#link");  # PLINK lookup - t.b.c.

=head1 DESCRIPTION

A Perldoc::DOM is a directed acyclic graph, which is a Computer
Scientist's way of saying "tree" (cue: the Fast Show "aliens that say
'tree' skit").

=head1 CREATING A Perldoc::DOM TREE

C<Perldoc::DOM> trees are seldom created using the C<Tree> interface.

Normally, they will be constructed as a series of events fired in by
something that C<.does(Perldoc::Sender)>, such as another
C<Perldoc::DOM>, a C<Perldoc::Filter>, or a C<Perldoc::Parser>.

=cut
