package Perldoc::DOM::Element;
use Perldoc::DOM::Node -Base;

=head1 NAME

Perldoc::DOM::Element - element in a Perldoc::DOM tree

=head1 SYNOPSIS

See L<Perldoc::DOM::Node>.

=head1 DESCRIPTION

An `element'.

All meta-information provided by dialects should be stored in this
tree as an attribute.  The property C<attr> is used for this.

=cut

# is there a Spiffy field way to do this?
sub attr {
    if ( defined(my $attr = shift) ) {
	if ( ref $attr eq "HASH" ) {
	    $self->{attr} = $attr;
	} else {
	    if ( my $value = shift ) {
		$self->{attr}{$attr} = $value;
	    } else {
		return $self->{attr}{$attr};
	    }
	}
    } else {
	return $self->{attr};
    }
}

sub _init {
    my $o = shift;

    $self->attr($o->{attr}||{});
    super($o);
}

sub dom_attr {
    my $att = super() or die;
    (%{$att}) = (%$att, %{$self->attr});
    $att;
}
