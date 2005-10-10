#!/usr/bin/perl

use strict;
use warnings;

use YAML;
#my @data = map { YAML::LoadFile($_) } @ARGV;
#
#my $combined = combine_tests(@data);
#
#print YAML::Dump($combined);
print YAML::Dump(combine_tests(map { YAML::LoadFile($_) } @ARGV));
#
# Annotates the last test tree with the earlier one's
# and returns the modified reference.
# Note: no copy is made!
#
# Adds a more_events[] with events to each event for which
# there are variations among the tests.
# A revision is added to each member of the more_events[].
#
# more_revisions[] is added with revision number of the older versions.
#
sub combine_tests{
    #
    # Collect all the test results indexed on filename/number
    # into %sum
    #
    my %sum;
    my @revisions;
    for my $data (@_) {
    	my $revision = $data->{revision};
    	push @revisions, $revision;
        for my $file (@{ $data->{meat}{test_files} }) {
            my $fname = $file->{file};
            for my $event (@{ $file->{events} }) {
    			my $n = $event->{num};
                push @{ $sum{$fname}[$n] }, {%$event, revision => $revision };
            }
        }
    }
    
    #
    # Remove all events which are *identical*
    #
    my $all_the_same = sub {
    	my $first = shift;
    	for my $o ( @_ ) {
            for my $k ( keys %$o ) {
    			next if $k eq 'revision';
                return 0 unless defined $o->{$k};
    			return 0 unless $first->{$k} eq $o->{$k};
            }
    	}
        return 1;
    };
    
    while( my ($fname, $events) =  each %sum ) {
        for(my $i=1; $i<@$events; $i++) {
            my $event = $events->[$i];
    		if($all_the_same->(@{ $events->[$i] })) {
    			undef $events->[$i];
    		} else {
                pop @{ $events->[$i] };
            }
        }
    }
    
    #
    # Now, walk the target tree and add anomalies
    # to each event.
    # Note that I use the test number for reference, which will
    # blow up in our faces when tests are inserted.
    # It is better then checking for the names of the tests 
    # since they are often the same for different tests.
    #
    my $target = $_[-1];
    for my $file (@{ $target->{meat}{test_files} }) {
        my $fname = $file->{file};
        for my $event (@{ $file->{events} }) {
            my $n = $event->{num};
            if( defined $sum{$fname}[$n]){
                $event->{more_events} = $sum{$fname}[$n];
            }
        }
    }
    
    #
    # Add revisions which were added, not the target though
    #
    pop @revisions;
    $target->{more_revisions} = [ @revisions ] if @revisions;
	$target;
}
