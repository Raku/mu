package Data::Transform::Trivial::Context;
use strict;
use warnings;

=head1 NAME

Data::Transform::Trivial::Context - context holder for tree transformation

=head1 DESCRIPTION

Don't use this module. You can get at the values via the C<$_P> and
C<@_L> variables. See L<Data::Transform::Trivial::Rule/Exported
Globals>.

=head1 METHODS

=head2 C<new($nodes,$position)>

=cut

sub new {
    my ($class,$nodes,$pos)=(@_);
    return bless {nodes=>($nodes||[]),
                  position=>$pos,
              },$class;
}

=head2 C<current_nodes>

Returns the ref to the nodes

=cut

sub current_nodes {
    my ($self)=@_;
    return $self->{nodes};
}

=head2 C<position> or C<position($newpos)>

Gets or sets the position.

=cut

sub position {
    my ($self,$new)=@_;
    $self->{position}=$new if defined $new;
    return $self->{position};
}

1;
