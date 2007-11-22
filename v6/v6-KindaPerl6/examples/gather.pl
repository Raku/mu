use v5;

# TODO - test with nested iterators

use threads;
use strict;
use warnings;

our $take;

sub gather {
    my $code = shift;
    my @results;
    # TODO don't use local(), use a "gather object" instead
    local $take = sub {
        print "taking \n";
        push @results, +shift;
    };
    # TODO start a thread
    # TODO set a "finished" flag on return

    my $thr = threads->new( $code );
    $thr->join;  # fixme

    #$code->();  # no threads

    # TODO return an iterator
    return \@results;
}

sub take {
    # TODO block until the iterator requires data
    $take->( +shift );
}

my $array =
    gather(
        sub {
            for (1,2,3) {
                print "gathering $_ \n";
                # TODO finish cleanly when the iterator is destroyed
                take( $_ );
            }
        },
    );

# TODO get data from the iterator instead
print "got: ", @$array, "\n";

=begin

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
