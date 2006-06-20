package v6-pugs;

# invokes the Perl6-to-Perl5 compiler and creates a .pmc file

# TODO!

use FindBin '$Bin';
use lib
    "$Bin/lib",
    "$Bin/../../../../lib",
    "$Bin/../Pugs-Compiler-Rule/lib",
    "$Bin/../Pugs-Utils/lib",
    "$Bin/../Pugs-Compiler-Precedence/lib",
;

use Pugs::Compiler::Perl6;
use Data::Dumper;
use strict;
use warnings;

sub import {
    print "p6-pugs\n";
}

warn caller;

    my @a=<>;
    my $src = join('', @a);
    my $p6 = Pugs::Compiler::Perl6->compile( $src );
    #use YAML;
    #print Dump $match->();
    #print dump_tree $p6;
    #print Dumper $p6;
    # print "tail: ", substr( ${$match}->{tail}, 0, 20 ),"...\n";
    print "P5: ",$p6->{perl5}, "\n";
    exit;


1;
