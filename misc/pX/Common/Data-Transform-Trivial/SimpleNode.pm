package SimpleNode;
use strict;
use warnings;
use List::MoreUtils qw(firstidx);

sub new {
    my ($class,$name,$attrs,$children)=@_;
    $children||=[];
    my $self=bless {name=>$name,
                  child=>$children,
                  attrs=>($attrs||{}),
              },$class;
    $_->{parent}=$self for grep {ref $_} @$children;
    return $self;
}

sub set_attr {
    my ($self,$name,$value)=@_;
    $self->{attrs}{$name}=$value;
}

sub attributes {
    my ($self)=@_;
    return wantarray ? %{$self->{attrs}} : $self->{attrs};
}

sub children {
    my ($self)=@_;
    return wantarray ? @{$self->{child}} : $self->{child};
}

sub name {
    my ($self)=@_;
    return $self->{name};
}

sub parent {
    my ($self)=@_;
    return $self->{parent};
}

sub position {
    my ($self)=@_;
    return 0 unless $self->parent;
    my $r=firstidx {$_ eq $self} $self->parent->children;
    warn "position: $r\n";
    return $r;
}

sub descendants_or_self {
    my ($self)=@_;
    my @ret=($self,map {$_->descendants_or_self} $_->children);
    return wantarray ? @ret : \@ret;
}

1;
