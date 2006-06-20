package P6;

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

    my $filename = (caller)[1];
    # print "P6 loaded from '$filename'\n";
    open FILE, "<", $filename
        or die "can't open $filename for compilation";
    my @a=<FILE>;
    close FILE;
    my $src = join('', @a);
    my $p6 = Pugs::Compiler::Perl6->compile( $src );
    #use YAML;
    #print Dump $match->();
    #print dump_tree $p6;
    #print Dumper $p6;
    # print "tail: ", substr( ${$match}->{tail}, 0, 20 ),"...\n";
    #print "P5: ",$p6->{perl5}, "\n";

    my $out = $filename;
    $out =~ s/\.(pl|pm)$/\.pmc/;
    die "this is not a .pl or .pm file"
        unless $out =~ /\.pmc/;

    open FILE, ">", $out
        or die "can't open $out for writing";

    print FILE $p6->{perl5}, "\n";

    # print "created .pmc file: $^X $out \n";
    exec $^X, $out 
        unless $filename =~ /\.pm$/;
1;
