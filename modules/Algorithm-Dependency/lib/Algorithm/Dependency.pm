#!pugs
use v6;

require Algorithm::Dependency::Item-0.0.1;
require Algorithm::Dependency::Source-0.0.1;

class Algorithm::Dependency-0.0.1;

# Implements the basic, non-order significant dependency algorithm

has $:source is Algorithm::Dependency::Source;
has %:selected;
has $:ignore_orphans is Bool;





method new( $class: ) returns Algorithm::Dependency {
	my %options = @_;

	# Arguments are provided as a hash of options.
	# We expect at LEAST the source argument.
	my $source = UNIVERSAL::isa( $options{source}, 'Algorithm::Dependency::Source' )
		? $options{source} : return;

	# Create the object
	my $self = $class.bless( {
		source   => $source, # Source object
		selected => {},
	} );

	# Were we given the 'ignore_orphans' flag?
	if ( $options{ignore_orphans} ) {
		$self.ignore_orphans = 1;
	}

	# Done, unless we have been given some selected items
	unless ( UNIVERSAL::isa( $options{selected}, 'ARRAY' ) ) {
		return $self;
	}

	# Make sure each of the selected it's exists
	my %selected = ();
	foreach my $id ( @{ $options{selected} } ) {
		# Does the item exist?
		$source.item($id) or return;

		# Is it a duplicate
		$selected{$id} and return;

		# Add to the selected index
		$selected{$id} = 1;
	}

	$self.selected = %selected;
	return $self;
}





#####################################################################
# Basic methods

# Get the Source object
method source( $self: ) returns Algorithm::Dependency::Source { return $self.source }

# Get the list of all selected items
method selected_list( $self: ) returns Array { return $self.selected.keys.sort }

# Is a particular item selected
method selected( $self: $id ) returns Bool { return $self.selected{$id} }

# Shortcut to the source to get a particular item
method item( $self: $id ) returns Algorithm::Dependency::Item { return $self.source.item( $id ) }





#####################################################################
# Main algorithm methods

# For one or more items, return a reference to the list of all additional
# items we would have to select, in alphabetical order. Do not list any
# items that are already selected. Do as efficiently as possible, as the
# source database could get quite large. We return in alphabetic order,
# for consistency.
method depends( $self: @stack ) returns Array {
	@stack or return;

	# Prepare
	my @depends = ();
	my %checked = ();

	# Process the stack
	while ( my $id = shift @stack ) {
		# Does the id exist?
		my $Item = $self.source.item($id)
		or $self.ignore_orphans ? next : return;

		# Skip if selected or checked
		next if $checked{$id};

		# Add it's depends to the stack
		push @stack, $Item.depends;
		$checked{$id} = 1;

		# Add anything to the final output that wasn't one of
		# the original input.
		unless ( @_.grep:{ $id eq $_ } ) {
			push @depends, $id;
		}
	}

	# Remove any items already selected
	my $s = $self.selected;
	return @depends.grep:{ ! $s{$_} }.sort;
}

# For one or more items, create a schedule of all items, including the
# ones specified, we will need to act upon in order to do something to
# the original set. i.e. All the modules we would need to install, including
# all dependencies. Note that for this class, the order is not important,
# so we return in alphabetic order for consistentcy.
method schedule( $self: @items ) returns Array {
	@items or return;

	# Get their dependencies
	my @depends = $self.depends( @items ) or return;

	# Now return a combined list, removing any items already selected.
	# We are allowed to return an empty list.
	my $s = $self.selected;
	return (@items, @depends).grep:{ ! $s{$_} }.sort;
}

# As above, but don't pass what we want to schedule as a list, just do the
# schedule for everything.
method schedule_all( $self: ) {
	return $self.schedule( $self.source.items.map:{ $_.id } );
}

1;

__END__

=pod

=head1 NAME

Algorithm::Dependency - Algorithmic framework for implementing dependency tree

=head1 SYNOPSIS

  require Algorithm::Dependency;
  require Algorithm::Dependency::Source::File;

  # Load the data from a simple text file
  my $data_source = Algorithm::Dependency::Source::File.new( 'foo.txt' );

  # Create the dependency object, and indicate the items that are already
  # selected/installed/etc in the database
  my $dep = Algorithm::Dependency.new(
      source => $data_source,
      selected => [ 'This', 'That' ]
      ) or die 'Failed to set up dependency algorithm';

  # For the item 'Foo', find out the other things we also have to select.
  # This WON'T include the item we selected, 'Foo'.
  my $also = $dep.depends( 'Foo' );
  print $also
  	? "By selecting 'Foo', you are also selecting the following items: "
  		. join( ', ', @$also )
  	: "Nothing else to select for 'Foo'";

  # Find out the order we need to act on the items in.
  # This WILL include the item we selected, 'Foo'.
  my $schedule = $dep.schedule( 'Foo' );

=head1 DESCRIPTION

Algorithm::Dependency is a framework for creating simple read-only
dependency heirachies, where you have a set of items that rely on other
items in the set, and require actions on them as well.

Despite the most visible of these being software installation systems like
the CPAN installer, or debian apt-get, they are usefull in other situations.
This module intentionally uses implementation-neutral words, to avoid
confusion.

=head2 Terminology

The term C<ITEM> refers to a single entity, such as a single software
package, in the overall set of possible entities. Internally, this is a
fairly simple object. See L<Algorithm::Dependency::Item> for details.

The term C<SELECT> means that a particular item, for your purposes, has
already been acted up in the required way. For example, if the software
package had already been installed, and didn't need to be re-installed,
it would be C<SELECTED>.

The term C<SOURCE> refers to a location that contains the master set of
items. This will be very application specific, and might be a flat file,
some form of database, the list of files in a folder, or generated
dynamically.

=head2 General Description

Algorithm::Dependency implements algorithms relating to dependency
heirachies. To use this framework, all you need is a source for the master
list of all the items, and a list of those already selected. If your
dependency heirachy doesn't require the concept of items that are already
selected, simply don't pass anything to the constructor for it.

Please note that the class Algorithm::Dependency does NOT implement an
ordering, for speed and simplicity reasons. That is, the C<schedule> it
provides is not in any particular order. If item 'A' depends on item 'B',
it will not place B before A in the schedule. This makes it unsuitable for
things like software installers, as they typically would need B to be
installed before A, or the installation of A would fail.

For dependency heirachies requiring the items to be acted on in a particular
order, either top down or bottom up, see L<Algorithm::Dependency::Ordered>.
It should be more applicable for your needs. This is the the subclass you
would probably use to implement a simple ( non-versioned ) package
installation system. Please note that an ordered heirachy has additional
constraints. For example, circular dependencies ARE legal in a
non-ordered heirachy, but ARE NOT legal in an ordered heirachy.

=head2 Extending

A module for creating a source from a simple flat file is included. For
details see L<Algorithm::Dependency::Source::File>. Information on creating
a source for your particular use is in L<Algorithm::Dependency::Source>.

=head1 METHODS

=head2 new %options

The constructor creates a new context object for the dependency algorithms to
act in. It takes as argument a series of options for creating the object.

=over 4

=item source => $Source

The only compulsory option is the source of the dependency items. This is
an object of a subclass of L<Algorithm::Dependency::Source>. In practical terms,
this means you will create the source object before creating the
Algorithm::Dependency object.

=item selected => [ 'A', 'B', 'C', etc... ]

The C<selected> option provides a list of those items that have already been
'selected', acted upon, installed, or whatever. If another item depends on one
in this list, we don't have to include it in the output of the C<schedule> or
C<depends> methods.

=item ignore_orphans => 1

Normally, the item source is expected to be largely perfect and error free.
An 'orphan' is an item name that appears as a dependency of another item, but
doesn't exist, or has been deleted.

By providing the C<ignore_orphans> flag, orphans are simply ignored. Without
the C<ignore_orphans> flag, an error will be returned if an orphan is found.

=back

The C<new> constructor returns a new Algorithm::Dependency object on success,
or C<undef> on error.

=head2 source

The C<source> method retrieves the L<Algorithm::Dependency::Source> object
for the algorithm context.

=head2 selected_list

The C<selected_list> method returns, as a list and in alphabetical order, the
list of the names of the selected items.

=head2 selected $name

Given an item name, the C<selected> method will return true if the item is
selected, false is not, or C<undef> if the item does not exist, or an error
occurs.

=head2 item $name

The C<item> method fetches and returns the item object, as specified by the
name argument.

Returns an L<Algorithm::Dependency::Item> object on success, or C<undef> if
an item does not exist for the argument provided.

=head2 depends $name1, ..., $nameN

Given a list of one or more item names, the C<depends> method will return a
reference to an array containing a list of the names of all the OTHER items
that also have to be selected to meet dependencies.

That is, if item A depends on B and C then the C<depends> method would
return a reference to an array with B and C. ( C<[ 'B', 'C' ]> )

If multiple item names are provided, the same applies. The list returned
will not contain duplicates.

The method returns a reference to an array of item names on success, a
reference to an empty array if no other items are needed, or C<undef> on
error.

=head2 schedule $name1, ..., $nameN

Given a list of one or more item names, the C<depends> method will return,
as a reference to an array, the ordered list of items you should act upon.

This would be the original names provided, plus those added to satisfy
dependencies, in the prefered order of action. For the normal algorithm,
where order it not important, this is alphabetical order. This makes it
easier for someone watching a program operate on the items to determine
how far you are through the task and makes any logs easier to read.

If any of the names you provided in the arguments is already selected, it
will not be included in the list.

The method returns a reference to an array of item names on success, a
reference to an empty array if no items need to be acted upon, or C<undef>
on error.

=head2 schedule_all;

The C<schedule_all> method acts the same as the C<schedule> method, but 
returns a schedule that selected all the so-far unselected items.

=head1 TO DO

Add the C<check_source> method, to verify the integrity of the source.

Possibly add Algorithm::Dependency::Versions, to implement an ordered
dependency tree with versions, like for perl modules.

Currently readonly. Make the whole thing writable, so the module can be used
as the core of an actual dependency application, as opposed to just being
a tool.

=head1 SUPPORT

Bugs should be submitted via the CPAN bug tracker, located at

L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Algorithm%3A%3ADependency>

For general comments, contact the author.

=head1 AUTHOR

Adam Kennedy (Maintainer), L<http://ali.as/>, cpan@ali.as

=head1 SEE ALSO

L<Algorithm::Dependency::Ordered>, L<Algorithm::Dependency::Item>,
L<Algorithm::Dependency::Source>, L<Algorithm::Dependency::Source::File>,

=head1 COPYRIGHT

Copyright (c) 2003 - 2004 Adam Kennedy. All rights reserved.
This program is free software; you can redistribute
it and/or modify it under the same terms as Perl itself.

The full text of the license can be found in the
LICENSE file included with this module.

=cut
