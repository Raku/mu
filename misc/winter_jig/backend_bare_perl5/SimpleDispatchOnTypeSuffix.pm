
package SimpleDispatchOnTypeSuffix;
use strict;
use warnings;

sub new {
    my($cls)=@_;
    my $self = {
	mapping => {},
	cached_regexp => undef,
    };
    bless $self,$cls;
}

sub dispatch_type_to {
    my($self,$type_suffix,$handler)=@_;
    $self->{mapping}{$type_suffix} = $handler;
    $self->{cached_regexp} = undef;
}

sub cache_initialize {
    my($self)=@_;
    my @keys = sort {length($b) <=> length($a)} keys(%{$self->{mapping}});
    my $pat = '('.join("|",@keys).')$';
    $self->{cached_regexp} = qr/$pat/;    
}

sub lookup {
    my($self,$obj)=@_;
    my $obj_typename = ref($obj) || 'SCALAR';
    $self->cache_initialize if not $self->{cached_regexp};
    $obj_typename =~ $self->{cached_regexp} or return undef;
    my $index = $1;
    $self->{mapping}{$index};
}

1;
