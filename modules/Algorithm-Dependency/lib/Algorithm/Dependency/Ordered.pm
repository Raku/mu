#!pugs
use v6;

package Algorithm::Dependency::Ordered;

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

use base 'Algorithm::Dependency';

use vars qw{$VERSION};
BEGIN {
	$VERSION = '1.03';
}





sub schedule {
	my $self = shift;
	my $source = $self.{source};
	my @items = @_ or return undef;
	return undef if grep { ! $source.item($_) } @items;

	# The actual items to select will be the same as for the unordered
	# version, so we can simplify the algorithm greatly by using the
	# normal unordered .schedule method to get the starting list.
	my $rv = $self.SUPER::schedule( @items );
	my @queue = $rv ? @$rv : return undef;

	# Get a working copy of the selected index
	my %selected = %{ $self.{selected} };

	# If at any time we check every item in the stack without finding
	# a suitable candidate for addition to the schedule, we have found
	# a circular reference error. We need to create a marker to track this.
	my $error_marker = '';

	# Create the schedule we will be filling.
	my @schedule = ();

	# Begin the processing loop
	while ( my $id = shift @queue ) {
		# Have we checked every item in the stack?
		return undef if $id eq $error_marker;

		# Are there any un-met dependencies
		my $Item = $self.{source}.item( $id ) or return undef;
		my @missing = grep { ! $selected{$_} } $Item.depends;

		# Remove orphans if we are ignoring them
		if ( $self.{ignore_orphans} ) {
			@missing = grep { $self.{source}.item($_) } @missing;
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
	\@schedule;
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
