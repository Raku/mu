#!perl6

# life.p6 adopted for perl6 after:
#
# // life.cola
# //
# // Game of life
# //
# // Copyright (C) 2002 Melvin Smith
# //
#
# (c) 2002 by Leopold Toetsch

# we don't have »is rw« or call string by refs, so
# input / output are int arrays - slooow
#
sub Print(@world) {
    my ($i, $j);
    for (0..15) -> $i {
	for (0..15) -> $j {
	    print @world[$i * 16 + $j] ?? '*' :: ' ';
	}
	print "\n" ;
    }
    print "----------------\n";
}

sub Generate(@input) {
    my ($cell, $neighbours, $i);
    my $len = 256; #@input;
    my ($pos, $offset);
    #my str $birth = "   *     ";
    #my str $death = "  **     ";
    my @death = (0,0,1,1,0,0,0,0,0);

    my @output = @input;

    loop ( $cell = 0; $cell < $len; $cell++ ) {
	$neighbours = 0;
	$i = $cell + $len;
	$neighbours++ if @input[($i - 1) % $len];
	$neighbours++ if @input[($i + 1) % $len];
	$neighbours++ if @input[($i - 17) % $len];
	$neighbours++ if @input[($i + 17) % $len];
	$neighbours++ if @input[($i - 16) % $len];
	$neighbours++ if @input[($i + 16) % $len];
	$neighbours++ if @input[($i - 15) % $len];
	$neighbours++ if @input[($i + 15) % $len];
	if (@input[$cell]) {
            if (@death[$neighbours]) {
                @output[$cell] = 1;
            }
            else {
                @output[$cell] = 0;
            }
	}
        else {
            if ($neighbours == 3) {
                @output[$cell] = 1;
            }
	}
    }

    return @output;
}

#static void Main()

sub main() {
    my  @world = (
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
	1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
	0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    );

    my ($i, $j, @new);
    my $gen = @ARGS[0] || 100;

    print "Running ", $gen, " generations\n";

    my $ts = time;
    loop ( $j= 0 ; $j < $gen; $j++ ) {
	@world = Generate @world;
        Print(@world);
    }
    my $tdelta = time() - $ts + 1;

    my $ratio = $gen / $tdelta;
    say "Gens/s: ", $ratio;
}

main();

