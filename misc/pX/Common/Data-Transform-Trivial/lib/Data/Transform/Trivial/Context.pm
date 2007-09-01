package Data::Transform::Trivial::Context;
use strict;
use warnings;

sub new {
    my ($class,$nodes,$pos)=(@_);
    return bless {nodes=>($nodes||[]),
                  position=>$pos,
              },$class;
}

sub current_nodes {
    my ($self)=@_;
    return $self->{nodes};
}

sub position {
    my ($self)=@_;
    return $self->{position};
}

1;
