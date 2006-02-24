#!/usr/bin/perl -w
use strict;

use Storable;
use Getopt::Long;
use YAML;

GetOptions \our %Conf, qw(allccs|a=s initial=s@);

our $allccs = YAML::LoadFile($Conf{allccs}) or
        die "can't load CC data: $!";

if (+$Conf{initial}) {
	doUnion (@{$Conf{initial}});
} else {
	doUnion($_) for (keys %$allccs);
}
exit 0;

sub doUnion {
	my %ccset = map { $_ => 1 } @_;

	while (my $new = candidates(\%ccset)) {
	    union(\%ccset, $new);
	}
}

sub union {
    my ($set, $new) = @_;
    my $size = keys %$set;
    #print ::Y({pre=>{set=>$set, new=>$new}});

    $set->{$new} = 1;
    #$set->{$_} = 1 for (ref $allccs->{$new} ? @{$allccs->{$new}} : $allccs->{$new});
	for my $f (ref $allccs->{$new} ? @{$allccs->{$new}} : $allccs->{$new}) {
		warn "union: adding $f\n";
		$set->{$f} = 1;
	}
die "$size -> ", scalar keys(%$set);
    #print ::Y({post=>{set=>$set, new=>$new}});
    return keys(%$set) - $size; # new member count
}

sub candidates {
    my %cands;
	my $ccset = shift;
    for my $cand (keys %$allccs) {
        next if exists $ccset->{$cand};
        $cands{$cand} = [ (ref $allccs->{$cand} ? scalar @{$allccs->{$cand}} : defined $allccs->{$cand} ? 1 : 0)       # total callees for func
                        , union(Storable::dclone($ccset), $cand)];  # new contributions
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
    printf "$cand: %s ($total total, $new new)", score($total, $new);
}

sub score {
    my ($total, $new) = @_;
    return 0 if $total == $new;
    ($total - $new) / $total;
}

#sub clone { Load(Dump($_[0])) }
sub ::Y { require YAML; YAML::Dump(@_) }
sub ::YY { require Carp; Carp::confess(::Y(@_)) }
