
class Perldoc::DOM::Text is Perldoc::DOM::Node;

has Str $.content;

# this should really be an acccessor - all it's doing is making the
# "source" property of the object fallback to "content"
method source($self:) {
    $.source || $.content;
}

method dom_fields($self:) {
    $self.SUPER::dom_fields, qw(content);
}

method dom_attr($self:) returns Hash {
    my %att = $self.SUPER::dom_attr();

    %att.delete<source>
        if %att.exists<source> and %att<source> eq %att<content>;

    %att;
}

sub event_type {
    return "characters";
}

=head1 NAME

Perldoc::DOM::Text - text node in a Perldoc::DOM tree

=head1 SYNOPSIS

See L<Perldoc::DOM::Node>.

=head1 DESCRIPTION

A C<Perldoc::DOM::Text> represents a little slice of content in a Perldoc
DOM tree.

It has one property - content.

The constructor for this class has a special shortcut syntax compared
to normal C<Perldoc::DOM::Node>'s / C<Tree::DAG_Node>'s - instead of
specifying options as a hash;

 Perldoc::DOM::Text->new({ content => "foo", source => "foo" });

You can just say;

 Perldoc::DOM::Text->new("foo");

(also, the latter form is slightly more efficient, though this is
marginal in string COW environments)

=cut
