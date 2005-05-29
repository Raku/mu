#!/usr/bin/pugs
use v6;

print "5x5 matrix in one line: " unless @*ARGS;
my $matrix = @*ARGS[0] || =<>;
chomp $matrix;
$matrix ||= "abcdefghijklmnopqrstuvwxy";

$matrix.chars == 25 or die "Matrix length MUST be 25 characters.\n";
my @matrix  = [ '_' xx 7 ];
push @matrix, [ '_', (split "", substr $matrix, 0, 5, ''), '_' ] while $matrix;
push @matrix, [ '_' xx 7 ];
my @adj;
for 1..5 -> $y {
    for 1..5 -> $x {
        for -1..1 -> $dx {
            for -1..1 -> $dy {
                $dy or $dx or next;
                @matrix[$y + $dy][$x + $dx] eq '_' and next;
                push @adj[$y][$x], { x => ($x + $dx), y => ($y + $dy) };
            }
        }
    }
}


sub build_re ($y, $x, $todo is copy, ?%had is copy) {
    %had{"$y/$x"} = 1;
    my $r = @matrix[$y][$x] or die "y=$y,x=$x is empty";
    --$todo or return $r;
    
    my @next; #= gather {
        for @adj[$y][$x] -> $adj {
            %had{"$adj<y>/$adj<x>"}++ and next;
            #take build_re $adj<y>, $adj<x>, $todo, %had;
            push @next, build_re $adj<y>, $adj<x>, $todo, %had;
        }
    #} or return $r;
    @next or return $r;

    return $todo == 1 
        ?? "$r\<[{ @next.join('') }]>?"
        :: "$r\[{ @next.join('|') }]{ $todo < 4 ?? '?' :: '' }";
}
my @re;
#say build_re 1, 1, 5;
#say "done";
#exit;

#my $re = #rule { 
#    ^ [ <$(
            for 1..5 -> $y {
                for 1..5 -> $x {
       push @re, build_re $y, $x, 6;
                }
            }
#        }
#    )> ] $ 
#};

$re = join('|', @re);

=foo

my %scores = (
  a => 1, b => 3, c => 3, d => 2, e => 1, f => 4, g => 2, h => 4, i => 1, 
  j => 8, k => 5, l => 1, m => 3, n => 1, o => 1, p => 3, q =>10, r => 1, 
  s => 1, t => 1, u => 1, v => 4, w => 4, x => 8, y => 4, z =>10
);
%scores.values >>*= 10;

gather {
    for slurp '/usr/share/dict/words' :chomp err die {
        next if /<-[a-z]>/;
        /$re/ and take { word => $_, score => %scores{ .letters }.sum };
    }
}
==> sort { -.<score> }, { .<word>.length }, { .<word> };
==> my @words;

sayf 'MATRIX IS WORTH %d POINTS' <== sum @words>>[0];
sayf '%3d %s' <== $_[1], $_[0] for @words;

