package v6;

# invokes the Perl6-to-Perl5 compiler and creates a .pmc file

use strict;
use warnings;

my $filename = (caller)[1];
my $pmc = $filename;
$pmc =~ s/\.(pl|pm|t)$/\.pmc/;
die "this is not a .pl, .t or .pm file"
    unless $pmc =~ /\.pmc/;
# test file dates
my $pmc_is_uptodate = (-s $pmc and (-M $pmc <= -M $filename));
# print "up to date: $pmc_is_uptodate\n";
if ( $pmc_is_uptodate ) {
    do $pmc 
        unless $filename =~ /\.pm$/;
    exit 0;
    # return 1;
}

my $p6;
# delay use'ing until we need it
eval 
  q(
    use FindBin '$Bin';
    use lib
        "$Bin/lib",
        "$Bin/../Pugs-Compiler-Rule/lib",
        "$Bin/../Pugs-Utils/lib",
        #"$Bin/../Pugs-Compiler-Precedence/lib",
        
        "$Bin/../lib",
        "$Bin/../../Pugs-Compiler-Rule/lib",
        "$Bin/../../Pugs-Utils/lib",
        #"$Bin/../../Pugs-Compiler-Precedence/lib",
    ;

    use Pugs::Compiler::Perl6;
    use Data::Dumper;

    # print "P6 loaded from '$filename'\n";
    open FILE, "<", $filename
        or die "can't open $filename for compilation";
    my @a=<FILE>;
    close FILE;
    my $src = join('', @a);
    $p6 = Pugs::Compiler::Perl6->compile( $src );
    #use YAML;
    #print Dump $match->();
    #print dump_tree $p6;
    #print Dumper $p6;
    # print "tail: ", substr( ${$match}->{tail}, 0, 20 ),"...\n";
    #print "P5: ",$p6->{perl5}, "\n";

    open FILE, ">", $pmc
        or die "can't open $pmc for writing";

    print FILE $p6->{perl5}, "\n";

    # print "created .pmc file: $^X $pmc \n";
  );
warn "compilation error: ", $@ if $@;
eval $p6->{perl5}  
    unless $filename =~ /\.pm$/;
exit 0;

1;
