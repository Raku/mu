#!/usr/bin/perl -w

use List::Util qw(sum);
use strict;

print "5x5 matrix in one line: " unless @ARGV;
my $matrix = shift || <>;
chomp $matrix;
$matrix ||= "abcdefghijklmnopqrstuvwxy";
my @matrix = [ ('_') x 7 ];
push @matrix, [ '_', (split //, substr $matrix, 0, 5, ''), '_' ] while $matrix;
push @matrix, [ ('_') x 7 ];

my @adj;

for my $y (1..5) {
    for my $x (1..5) {
        for my $dx (-1..1) {
            for my $dy (-1..1) {
                $dy or $dx or next;
                $matrix[$y + $dy][$x + $dx] eq '_' and next;
                push @{ $adj[$y][$x] }, { y => $y + $dy, x => $x + $dx };
            }
        }
    }
}

sub build_re {
    my ($y, $x, $todo, $had) = @_;
    my $r = $matrix[$y][$x] or die "y=$y,x=$x is empty (@_)";
    --$todo or return $r;
    my %had = $had ? %$had : ("$y/$x" => 1);  # copy
    
    my @next = map {
        $had{"$_->{y}/$_->{x}"}++ 
            ? () 
            : build_re($_->{y}, $_->{x}, $todo, \%had)
    } @{ $adj[$y][$x] };

    @next or return $r;

    return $todo == 1
        ? $r . (@next == 1 ? "@next?" : '[' . join('', @next) . ']?')
        : $r . '(?:' . join('|', @next) . ')' . ($todo < 4 ? '?' : '');
}

my @re;

for my $y (1..5) {
    for my $x (1..5) {
        push @re, build_re $y, $x, 6;
    }
}

my $re = join '|', @re;
$re = "^(?:$re)\\z";  # Don't compile yet - once is enough

my %scores = (
  a => 1, b => 3, c => 3, d => 2, e => 1, f => 4, g => 2, h => 4, i => 1, 
  j => 8, k => 5, l => 1, m => 3, n => 1, o => 1, p => 3, q =>10, r => 1, 
  s => 1, t => 1, u => 1, v => 4, w => 4, x => 8, y => 4, z =>10
);
$_ *= 10 for values %scores;

my @matches;
open my $fh, '/usr/share/dict/american-english' or die $!;

substr(join('', @{ $matrix[1] }), 1, 5) =~ /$re/ or die;  # Precompile
while (<$fh>) {
    chomp;
    next if tr/a-z//c;  # Regex would destroy the compiled one
    // and push @matches, [ $_, sum map $scores{$_}, split // ];
        # Re-use precompiled regex
}

my @sorted = sort { 
    $b->[1] <=> $a->[1]                   # high score .. low score
    || length $a->[0] <=> length $b->[0]  # short .. long
    || $a->[0] cmp $b->[1]                # a .. z
} @matches;

printf "MATRIX IS WORTH %d POINTS\n", sum map $_->[1], @sorted;
printf "%3d %s\n", $_->[1], $_->[0] for @sorted;

