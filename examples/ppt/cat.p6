#!/usr/bin/pugs

use v6;

use Getopt::Std;
my %opts = getopts("benstuv");

%opts<v>++ if %opts<e> | %opts<t>;
%opts<b> = 0 if %opts<n>;

# unbuffer output if %opts<u>;

my $empty = 0; # for -s option
my $linenum = 1;

while $_ = =<> {
    $_ = chomp $_; # "is chomped", when we have it

    # squeeze runs of blank lines to one
    if %opts<s> {
        next if ! rx:P5/./ && $empty++;
    } else {
        $empty = 0;
    }

    $_ ~~ s:P5:g{\t}{^I} if %opts<t>;

    $_ ~~ s:P5:g/(.)/{backwhack($0)}/ if %opts<v>;
    
    $_ ~= "$" if %opts<e>;

    if %opts<n> {
        say sprintf "%6d  %s", $linenum++, $_;
    } elsif %opts<b> {
        if rx:P5/./ {
            say;
        } else {
            say sprintf "%6d  %s", $linenum++, $_;
        }
    } else {
        say;
    }
}

sub backwhack ($ch) {
    given ord $ch {
        when 0x00 .. 0x1f  { "^" ~ chr 0x40 + $_ }  # the spec used octal
        when ({$_ > 0x7f}) { "U+" ~ "$_".as("%x") } # but this is the unicode era
        default { $ch }
    }
}
