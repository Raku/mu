#!/usr/bin/perl -w
use strict;

use Storable;
use Getopt::Long;
use YAML;

GetOptions \our %Conf, qw(help|h nodes|n=s initial=s@ debug=s except=s@);

if ($Conf{help}) {
    print <<'END';
  run with -n filename containing nodes data produced by --nodes
of graphfuncs.pl. Will print initial set sizes for all functions
by default, or use -initial <fname>, possibly many times, to
specify the initial set. -except <fname> introduces a function to
ignore, can iterate. -debug <fname> is a function to print more
information about as components are computed.
END
    exit 0;
}
 
our $nodes = YAML::LoadFile($Conf{nodes}) or
        die "can't load nodes data: $!";

our %ignoring;

$ignoring{$_} = 1 && print "Ignoring: $_\n" for (@{$Conf{except}});
                
if (+$Conf{initial}) {
        print "Initial set: " . join(' ', @{$Conf{initial}}) . "\n";
	doUnion (@{$Conf{initial}});
} else {
        print "CC sizes:\n";
        for (keys %$nodes) {
            print "$_: " . union({$_=>1}, $_) . "\n";
        }
}
exit 0;

sub doUnion {
	my %ccset = map { $_ => 1 } @_;
        union (\%ccset, $_) for keys %ccset;
        print "doUnion: initial set size is " . scalar(keys %ccset) . "\n";

        print "\nFinding candidates.\n\n";

        candidates(\%ccset);
	#while (my $new = candidates(\%ccset)) {
        #    union(\%ccset, $new);
	#}
}

sub union {
    my ($set, $new) = @_;
    my $oldsize = keys %$set;
    my $debug = $new eq ($Conf{debug}||="");

    return 0 if $ignoring{$new};

    my $size = scalar(keys %$set) - 1; # to force one run even if $new is in
    $set->{$new} = 1;
    while (scalar(keys %$set) > $size) {
        $size = keys %$set;
        for my $f (keys %$set) {
            for my $t (@{$nodes->{$f}}) {
                next if $set->{$t} or $ignoring{$t};
	        print "unionising $new: adding $t on account of $f\n" if $Conf{verbose} or $Conf{debug} eq $new;
                $set->{$t} = 1;
            }
        }
    }
    #print ::Y({post=>{set=>$set, new=>$new}});
    print "union: while adding $new, entered with $oldsize, leaving with $size\n"
        if $Conf{verbose} or $Conf{debug} eq $new;
    return $size - $oldsize; # new member count
}

sub candidates {
    my %cands;
    my $ccset = shift;
    for my $cand (keys %$nodes) {
        #print "trying: $cand\n";
        next if exists $ccset->{$cand} or $ignoring{$cand};
        #print "passed: $cand\n";
        $cands{$cand} = [ scalar @{$nodes->{$cand}}, 
                         # total callees for func
          , union(Storable::dclone($ccset), $cand)];  # new contributions
        #print "$cand: totall $cands{$cand}[0] $cands{$cand}[1]\n";
    }

    # print ten best candidates
    print_cand($_, $cands{$_}) for
        sort {score(@{$cands{$b}}) <=> score(@{$cands{$a}})}
        keys %cands;

    # prompt the user
    # return list of newly selected functions
}

sub print_cand {
    my ($cand, $data) = @_;
    my ($total, $new) = @$data;
    printf "$cand: %s ($total total, $new new)\n", score($total, $new);
}

sub score {
    my ($total, $new) = @_;
    return 0 if $total == $new;
    ($total - $new) / $total;
}

#sub clone { Load(Dump($_[0])) }
sub ::Y { require YAML; YAML::Dump(@_) }
sub ::YY { require Carp; Carp::confess(::Y(@_)) }
