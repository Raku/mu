#!pugs
use v6;

require Algorithm::Dependency-0.0.1;

class Algorithm::Dependency::Ordered-0.0.1 is Algorithm::Dependency;

# This package implements a version of Algorithm::Dependency where the order
# of the schedule is important.
#
# For example, when installing software packages, often their dependencies
# not only need to be installed, but be installed in the correct order.
#
# The much more complex .schedule method of this class takes these factors
# into account. Please note that while circular dependencies are possible
# and legal in unordered dependencies, they are a fatal error in ordered
# dependencies. For that reason, the schedule method will return an error
# if a circular dependency is found.





method schedule( $self: @items ) returns Array {
	my $source = $self.source;
	@items or return;
	@items.grep:{ ! $source.item($_) } and return;

	# The actual items to select will be the same as for the unordered
	# version, so we can simplify the algorithm greatly by using the
	# normal unordered .schedule method to get the starting list.
	my $rv = $self.SUPER::schedule( @items );
	my @queue = $rv ? @$rv : return;

	# Get a working copy of the selected index
	my %selected = %{ $self.selected };

	# If at any time we check every item in the stack without finding
	# a suitable candidate for addition to the schedule, we have found
	# a circular reference error. We need to create a marker to track this.
	my $error_marker = '';

	# Create the schedule we will be filling.
	my @schedule = ();

	# Begin the processing loop
	while ( my $id = shift @queue ) {
		# Have we checked every item in the stack?
		$id eq $error_marker and return;

		# Are there any un-met dependencies
		my $Item = $self.source.item( $id ) or return;
		my @missing =  $Item.depends.grep:{ ! $selected{$_} };

		# Remove orphans if we are ignoring them
		if ( $self.ignore_orphans ) {
			@missing = @missing.grep:{ $self.source.item($_) };
		}

		if ( @missing ) {
			# Set the error marker if not already
			$error_marker = $id unless $error_marker;

			# Add the id back to the end of the queue
			push @queue, $id;
			next;
		}

		# All dependencies have been met. Add the item to the schedule and
		# to the selected index, and clear the error marker.
		push @schedule, $id;
		$selected{$id} = 1;
		$error_marker = '';
	}

	# All items have been added
	return @schedule;
}

1;

__END__

=pod

=head1 NAME

Algorithm::Dependency::Ordered - Implements an ordered dependency heirachy

=head1 DESCRIPTION

Algorithm::Dependency::Ordered implements the most common variety of
L<Algorithm::Dependency>, the one in which the dependencies of an item must
be acted upon before the item itself can be acted upon.

In use and semantics, this should be used in exactly the same way as for the
main parent class. Please note that the output of the C<depends> method is
NOT changed, as the order of the depends is not assumed to be important.
Only the output of the C<schedule> method is modified to ensure the correct
order.

For API details, see L<Algorithm::Dependency>.

=head1 SUPPORT

Bugs should be submitted via the CPAN bug tracker, located at

http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Algorithm%3A%3ADependency

For general comments, contact the author.

=head1 AUTHOR

Adam Kennedy (Maintainer), L<http://ali.as/>, cpan@ali.as

=head1 SEE ALSO

L<Algorithm::Dependency>

=head1 COPYRIGHT

Copyright (c) 2003 - 2004 Adam Kennedy. All rights reserved.
This program is free software; you can redistribute
it and/or modify it under the same terms as Perl itself.

The full text of the license can be found in the
LICENSE file included with this module.

=cut
