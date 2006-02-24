#!/usr/bin/perl -w
use strict;

use Storable;
use Getopt::Long;
use YAML;

GetOptions \our %Conf, qw(allccs|a=s initial=s@);

our %ccset = map { $_ => 1 } @{ $Conf{initial} };

our $allccs = YAML::LoadFile($Conf{allccs}) or
        die "can't load CC data: $!";

while (my $new = candidates()) {
    union(\%ccset, $new);
}
exit 0;

sub union {
    my ($set, $new) = @_;
    my $size = keys %$set;
    #print ::Y({pre=>{set=>$set, new=>$new}});

    $set->{$new} = 1;
    $set->{$_} = 1 for (ref $allccs->{$new} ? @{$allccs->{$new}} : $allccs->{$new});

    #print ::Y({post=>{set=>$set, new=>$new}});
    return keys(%$set) - $size; # new memeber count
}

sub candidates {
    my %cands;
    for my $cand (keys %$allccs) {
        next if exists $ccset{$cand};
        $cands{$cand} = [ scalar @{$allccs->{$cand}}      # total callees for func
                        , union(Storable::dclone(\%ccset), $cand)];  # new contributions
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
