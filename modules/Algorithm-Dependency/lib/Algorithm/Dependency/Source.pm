#!pugs
use v6;

require Algorithm::Dependency-0.0.1;

class Algorithm::Dependency::Source-0.0.1;

# Algorithm::Dependency::Source implements a parent class for a source
# of items.

has $:loaded is Bool = 0;
has %:items_hash is Hash of Algorithm::Dependency::Item;
has @:items_array is Array of Algorithm::Dependency::Item;





method new( $class: ) returns Algorithm::Dependency::Source {

	# This can't be created directly, it must be through
	# a SUPER::new() call
	if ( $class eq __PACKAGE__ ) {
		die "Cannot directly instantiate Algorithm::Dependency::Source. You must use a subclass";
	}

	# Create the basic object
	return $class.bless( {
		# Has the source been loaded
		loaded      => 0,

		# Indexes
		items_hash  => undef,
		items_array => undef,
	} );
}

# Load the source
method load( $self: ) returns Bool {
	my $class = ref $self;

	# If this is a reload, clean up in preperation
	if ( $self.loaded ) {
		$self.loaded = 0;
		$self.items_hash = undef;
		$self.items_array = undef;
	}

	# Pass through to the real loader
	my @items = $self._load_item_list();
	@items or return @items;

	# Add the items
	for @items -> $item {
		unless $item.meta.isa(Algorithm::Dependency::Item) {
			die "$class\::_load_item_list() returned something that was not an Algorithm::Dependency::Item";
		}

		# Have we added this one already?
		my $id = $item.id();
		if ( $self.items_hash{$id} ) {
			# Duplicate entry
			return;
		}

		# Add it
		$self.items_array.push( $item );
		$self.items_hash{$id} = $item;
	}

	return $self.loaded = 1;
}

# See if loaded
method loaded( $self: ) returns Bool {
	return $self.loaded;
}

# Get a single item by id
method item( $self: $id ) returns Algorithm::Dependency::Item {
	$self.loaded or $self.load() or return;

	# Return the item ( or undef )
	return $self.items_hash{$id};
}

# Get a list of the items
method items( $self: ) returns Array of Algorithm::Dependency::Item {
	$self.loaded or $self.load() or return;
	return @{ $self.items_array };
}

# Check the integrity of the source.
method missing_dependencies( $self: ) returns Array {
	$self.loaded or $self.load() or return;
	
	# Merged the depends of all the items, and see if
	# any are missing.
	my %missing = $self.items.map:{ $_.depends() }.grep:{ ! $self.item($_) }.map:{ $_ => 1 };
	return %missing ?? %missing.keys.sort :: 0;
}





#####################################################################
# Catch methods our subclass should define but didn't

method :_load_item_list( $self: ) { die "Class $self failed to define the method _load_item_list" };

1;

__END__

=pod

=head1 NAME

Algorithm::Dependency::Source - Implements a source of heirachy items

=head1 DESCRIPTION

The Algorithm::Dependency::Source class provides an abstract parent class for
implementing sources for the heirachy data the algorithm will use. For an
example of an implementation of this, see
L<Algorithm::Dependency::Source::File>, which is bundled with the main
L<Algorithm::Dependency> package.

=head1 METHODS

=head2 new @arguments

Although you cannot directly use the C<new> constructor for
C<Algorithm::Dependency::Source>, it will work the same in all subclasses.

The constructor takes zero or more subclass specific arguments to define the
location of the source of the items, and returns a new object. Alrough it
may check that the arguments you passed are valid, the source will usually
NOT actually load the items from the source, instead defering the loading
until you need to use the items.

Returns a new object on success, or C<undef> on error.

=head2 load

The C<load> method is the public method used to actually load the items from
their storage location into the the source object. The method will
automatically called, as needed, in most circumstances. You would generally
only want to use C<load> manually if you think there may be some uncertainty
that the source will load correctly, and want to check it will work.

Returns true if the items are loaded successfully, or C<undef> on error.

=head2 item $name

The C<item> method fetches and returns the item object specified by the
name argument.

Returns an L<Algorithm::Dependency::Item> object on success, or C<undef> if
the named item does not exist in the source.

=head2 items

The C<items> method returns, as a list of objects, all of the items
contained in the source. The item objects will be returned in the same order
as that in the storage location.

Returns a list of L<Algorithm::Dependency::Item> objects on success, or
C<undef> on error.

=head2 missing_dependencies

By default, we are leniant with missing dependencies if the item is neved 
used. For systems where having a missing dependency can be very bad, the 
C<missing_dependencies> method checks all Items to make sure their 
dependencies exist.

If there are any missing dependencies, returns a reference to an array of
their ids. If there are no missing dependencies, returns 0. Returns 
C<undef> on error.

=head1 EXTENDING

Algorithm::Dependency::Source itself is a fairly thin module, and it is
intended that you will probably need to extend it to be able to extract
item data from whatever location you have stored them.

This is usually a fairly simple two step process.

=over 4

=item Overload the C<new> method.

Assuming your subclass takes some form or argument on creation, you will
need to overload the C<new> method to accept the arguments, validate them,
and store them in the source object.

=item Define the method C<_load_item_list>.

Leaving our parent's C<load> method to take care of conflict, errors, and
whatever, the C<_load_item_list> method is used to simply create a list of
L<Algorithm::Dependency::Item> objects from wherever you store the item,
and return them as a list.

=back

Having completed these two things, your subclass should be completed. For
an example of the code, have a look at the source for the simple subclass
L<Algorithm::Dependency::Source::File>.

=head1 SUPPORT

For general comments, contact the author.

To file a bug against this module, in a way you can keep track of, see the
CPAN bug tracking system.

http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Algorithm%3A%3ADependency

=head1 AUTHOR

Adam Kennedy (Maintainer), L<http://ali.as/>, cpan@ali.as

=head1 SEE ALSO

L<Algorithm::Dependency>, L<Algorithm::Dependency::Source::File>

=head1 COPYRIGHT

Copyright (c) 2003 Adam Kennedy. All rights reserved.
This program is free software; you can redistribute
it and/or modify it under the same terms as Perl itself.

The full text of the license can be found in the
LICENSE file included with this module.

=cut
