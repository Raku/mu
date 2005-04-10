#!/usr/bin/perl6

use v6;

my $string = "an apple a day";

# Does Scalar.chars returns array in array context?
my @array = $string.chars;

# unpack() not yet documented in S29 as of this writing. Currently this
# example clearly won't work for unicode
@array = unpack("C*", $string);

# loop through the [unicode] chars
for $string.chars { 
	# do something with $1
}

# find the unique characters in a string
my %seen;
for $string.chars -> $char {
    %seen{$char}++;
}
say "unique chars are: " ~ sort %seen.keys;

# add all the unicode character values together
$sum = 0;
for $string.codes -> $univalue { # is .codes right for getting unicode nums?
    $sum += $univalue;
}
say "sum is $sum";

#-----------------------------
$sum = unpack("%32C*", $string);

#-----------------------------
# checksum.p6
#!/usr/bin/perl6

use v6;

# sum - compute 16-bit checksum of all input files
my $checksum = 0;
for =<> -> $line { 
	$checksum += unpack("%16C*", $line); # unpack not documented yet
}
$checksum %= (2 ** 16) - 1;
say "$checksum";

#-----------------------------
#% perl sum /etc/termcap
#1510
#-----------------------------
#% sum --sysv /etc/termcap
#1510 851 /etc/termcap
#-----------------------------

#-----------------------------
# slowcat.p6
#!/usr/bin/perl6

use v6;

# emulate a   s l o w   line printer
# usage: slowcat [-DELAY] [files ...]
#
# following is bad code, for a start it'll do the wrong thing on some inputs
# i don't really think it should be left this way, but i'm sticking to the
# original examples for the moment --gcomnz
my $DELAY = (@*ARGS[0] =~ m/^-([.\d]+)/) ?? (shift @*ARGS, $1) :: 1;

# output buffer modification is probably becoming a $* variable or a trait 
# on $*OUT, but i can't find a doc for it
$| = 1;  
for =<> -> $line {
    for $line.chars -> $char {
        print $char;
		sleep $DELAY; # sleep restricts us to second multiples
		# perl 5 version uses a select() hack for sub-second delays but i'm
		# avoiding that in lieu of further documentation
    }
}

