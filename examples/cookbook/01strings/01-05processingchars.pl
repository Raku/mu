#!/usr/bin/perl6

use v6;

=head1 NAME

Perl 6 Cookbook: Processing Strings Character by Character

=head1 Summary

You want to process strings one character at a time.

=head1 Solution

You can easily fill an array with all the Unicode characters in a string.

    # split the unicode elements
    # TODO: CONFIRM: the below syntax is not approved yet, 
    # see http://tinyurl.com/6whlj for discussion
    my @chars = $string[];

    # use unpack to do the same thing
    @array = unpack("C*", $string);

If what you want do do is loop through the characters, you don't need to assign
to an array. Use a for loop instead on the string in list context.`

    # loop through the Unicode chars
    for $string[] { 
        # do something with $_
    }

If you only need to run one command during the loop then you can be even more
concise.

    # concise loop through Unicode chars
    say $_ for $string[];

Calling the string in a list context automatically returns the appropriate 
Unicode level required. This means that you don't have to do any special
syntax to deal with multiple languages and encodings.

To find all the unique characters in a string, assign them all to a hash, which
will automatically deduplicate them.

    # find the unique characters in a string
    my %seen;
    for $string[] -> $char {
        %seen{$char}++;
    }
    say "unique chars are: " ~ sort %seen.keys;

    # concise syntax for the same
    my %seen;
    say sort (%seen{$_}++ for $string.chars).keys;

XXX Don't we want a .unique or .uniq method? 
    -- seems to be a question for perl6lang --gcomnz
    -- either way I'm sure i'm doing both are the long way, gotta be 
        something more concise, but i'm too tired to put for the 
        effort at this moment --gcomnz

Add all the unicode character values together

    say "sum is &sum($string.codes)";
    -- What on earth is the point of this? I'm about to get rid
        of it, seems totally useless, if it even works in the first
        place --gcomnz
        XXX Agreed

=head1 Example Script 1

A simple checksum script example: checksump.p6

    #!/usr/bin/perl6
    
    use v6;
    
    # checksum.p6 - compute 16-bit checksum of all input files
    my $checksum = 0;
    for =<> -> $line { 
         # XXX unpack not documented yet
        $checksum += unpack("%16C*", $line);
    }
    $checksum %= (2 ** 16) - 1;
    say $checksum;

Usage:

    $ checksum.p6 /etc/termcap
    1510

Compare results with the common sum command.

    $ sum --sysv /etc/termcap
    1510 851 /etc/termcap

=head1 Example Script 2

An on-screen line printer example: slowcat.p6

-- XXX following is bad code, for a start it'll do the wrong thing on some 
inputs i don't really think it should be left this way, but i'm sticking to 
the original examples for the moment --gcomnz

    #!/usr/bin/perl6
    
    use v6;
    
    # emulate a   s l o w   line printer
    # usage: slowcat [-DELAY] [files ...]
    #
    my $DELAY = (@*ARGS[0] =~ m/^-([.\d]+)/) ?? (shift @*ARGS, $0) !! 1;
    
    # output buffer modification is probably becoming a $* variable 
    # or a trait on $*OUT, but i can't find a doc for it --gcomnz
    $| = 1;  
    for =<> -> $line {
        for $line[] -> $char {
            print $char;
            # perl 5 version uses a select() hack for sub-second
            # delays but i'm avoiding that in lieu of further 
            # documentation --gcomnz
            # XXX sleep not documented yet
            sleep $DELAY; 
            # XXX I thought Perl 6 would use high res sleep by 
            # default?
            #     -- pugs seems to currently sleep for 
            #         one second intervals --gcomnz
        }
    }

=cut
