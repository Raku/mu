
package Kwid::DOM::Node;

use strict;
use warnings;
use Carp;
use base 'Kwid::Base';

use base 'Tree::DAG_Node';

=head1 NAME

Kwid::DOM::Node - node in a Kwid::DOM tree

=head1 SYNOPSIS

 # construct a Kwid::DOM fragment from this:
 #
 #  =head1 NAME
 #
 #  foo
 #
 #  =cut
 #
 # corresponding normative XML tree;
 #
 #  <sect1><title>NAME</title><para>foo</para></sect1>

 # Kwid::DOM::Node representation;
 my $node = Kwid::DOM::Element->new
     ({ name => "sect1",
        source => "=head1 ",  # text "eaten" by this node
     });


 # no "source", as this is an "implied" tag
 my $title = Kwid::DOM::Element->new ({ name => "title" });
 $node->add_daughter($title);

 # text nodes are different, like W3C DOM - don't moan
 # kiddies, this is for your own good :)
 my $text = Kwid::DOM::Text->new ("NAME");

 # note that Texts *can* have an alternate source fragment, but it
 # defaults to be the same as the content, modulo whatever we end up
 # doing with whitespace for the round-tripping

 $title->add_daughter($text);

 # etc etc
 my $para = Kwid::DOM::Element->new({ name => "para",
                                      source => "\n\n" });
 $node->add_daughter($para);
 # alternate way of creating Texts with content
 $para->add_daughter(Kwid::DOM::Text->new
                         ({ content => "foo" }));

 # dummy nodes used only for reconstruction to source.
 # represented as processing instructions, or maybe comments
 # in the "normative XML"
 $node->add_daughter
    (Kwid::DOM::PI->new({ source => "\n\n=cut"}));

=head1 DESCRIPTION

Well, with that informative but utterly confusing synopsis, you should
be left with nothing but questions about what this object represents.

It represents a node in the Kwid DOM tree (see L<Kwid::DOM> for more).
The DOM tree has a root node, which is a "C<body>" element of sorts.

Different types of nodes in a Kwid DOM tree have different properties,
you should see the subclasses for more.

In short, those subclasses are;

=over

=item L<Kwid::DOM::Element>

It's all about meta-data and structure, not content!

=item L<Kwid::DOM::Text>

The Text nodes are where it's at, baby, yeah!

=item L<Kwid::DOM::PI>

Placeholder for dummy nodes that contain only source representation.

=back

=head1 API METHODS

These methods must be implemented by subclasses... though a default
implementation exists, too.

=over

=item B<source>

=item B<source($new_source)>

Returns the representation of this node in the original text, or at
least a valid representation of it that "honours the spirit of the
dialect", if the original form is not available or has been discarded.

=cut

# field 'source'; ?
sub source {
    my $self = shift;

    if ( @_ ) {
	$self->{source} = shift;
    } else {
	$self->{source};
    }
}

sub _init {
    my $self = shift;
    my $o = shift;

    $self->SUPER::_init($o);

    $self->{source} = $o->{source} if $o->{source};
}

sub new {
    my $inv = shift;
    my $class = ref $inv || $inv;
    $class ne __PACKAGE__ or croak "attempt to create a pure Node";
    return $inv->SUPER::new(@_);
}

1;

__END__

=head1 SEE ALSO

L<Tree::DAG_Node> - details on how to navigate the DAG.

=cut

