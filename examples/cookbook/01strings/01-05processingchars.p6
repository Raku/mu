#!/usr/bin/perl6

use v6;

=head1 Processing a String one Character at a Time

You want to process individual characters of a string

=cut

my $string = "an apple a day";

# split the unicode elements
# the below syntax is not approved yet, see http://tinyurl.com/6whlj for 
# discussion
my @array = $string[];
# XXX This is list context, not array context.
# Juerd: is is innappropriate to call the above an @array? should i call
# it @list (and other examples below) for learner purposes? i'm looking for
# something definitive on this, but haven't found it yet
# No, it is not. In fact, @list would be terribly wrong.
# The list context I mentioned is on the RHS of the assignment. An earlier
# version of this file said something about array context. Array context is the
# context for arrays/arrayrefs, like the first argument of push. In Perl 6,
# array context just scalar context that wants Array, in Perl 5, array context
# is built into the language and cannot be provided by user defined subs.
# In other light, @array is a very bad name, but in that light, @list is
# equally silly. The @ already says it's an array, and @chars is probably a
# much better name.


# use unpack to do the same thing
# s/C/U work with unicode (thanks Juerd)
@array = unpack("C*", $string);

# loop through the [unicode] chars
for $string[] { 
	# do something with $_
}

# concise syntax for running a function on a loop through [unicode] chars
say $_ for $string[];

# find the unique characters in a string
# XXX Don't we want a .unique or .uniq method? 
my %seen;
for $string[] -> $char {
    %seen{$char}++;
}
say "unique chars are: " ~ sort %seen.keys;

# concise syntax for the same
# going out on a limb here, i feel next to certain that my paranthesis won't
# cut it, so i'm hoping to learn from corrections to this
my %seen;
say sort (%seen{$_}++ for $string.chars).keys;

# add all the unicode character values together
say "sum is &sum($string.codes)";

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
say $checksum;

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
    for $line[] -> $char {
        print $char;
		sleep $DELAY; # sleep restricts us to second multiples
# XXX I thought Perl 6 would use high res sleep by default?
		# perl 5 version uses a select() hack for sub-second delays but i'm
		# avoiding that in lieu of further documentation
    }
}

