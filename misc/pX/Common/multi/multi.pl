#!/usr/bin/perl
use warnings;
use strict;
package Thing;
use Moose;

package Paper;
use Moose;
extends 'Thing';

package Rock;
use Moose;
extends 'Thing';

package Scissors;
use Moose;
extends 'Thing';

package main;

use Data::Dump::Streamer;
use Test::More 'no_plan';
use Carp qw(croak);
my $paper    = new Paper;
my $scissors = new Scissors;
my $rock     = new Rock;

sub class($) {
    my ($class) = @_;
    return sub {$_[0]->isa($class)};
}
sub generate {
    my @variants = @_;
    return sub {
        choose([@variants],@_);
    }
}
sub legal {
    my ($code,@constraints) = @{shift @_};
    return 0 if $#constraints != $#_;
    for my $i (0 .. $#_) {
        unless ($constraints[$i]->($_[$i])) {
            #print "illegal:",Dump([$constraints[$i],$_[$i]]);
            return 0;
        }
    }
    return 1;
}
ok legal ([sub {return 1},class "Paper",class "Rock"],$paper,$rock);
ok legal ([sub {return 1},class "Paper",class "Thing"],$paper,$rock);
ok !legal ([sub {return 1},class "Paper",class "Rock"],$paper,$scissors);
sub choose {
    my @voting = grep {legal($_,@_)} @{shift @_};
    if ($#voting == 0) {
        return $voting[0][0]->(@_);
    } else {
        croak "ambiguity in mmd";
    }
}
my @choice = (
    [sub {return 1},class "Paper",class "Rock"],
    [sub {return 0},class "Paper",class "Scissors"],
    [sub {return 0},class "Paper",class "Paper"],
);
*defeats = generate(@choice);
ok  defeats ($paper,$rock);
ok !defeats ($paper,$scissors);
#ok  defeats ($paper,$rock);
#ok !defeats ($paper,$paper);
#ok !defeats ($paper,$scissors);
#print Dump(
#    map {[legal($_,$paper,$rock),$_]} choose([@choice],$paper,$rock)
#);
