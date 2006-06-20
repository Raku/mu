package P6;

# invokes the Perl6-to-Perl5 compiler and creates a .pmc file

    my $filename = (caller)[1];

    my $pmc = $filename;
    $pmc =~ s/\.(pl|pm)$/\.pmc/;
    die "this is not a .pl or .pm file"
        unless $pmc =~ /\.pmc/;

    # test file dates
    my $pmc_is_uptodate = (-s $pmc and (-M $pmc <= -M $filename));
    # print "up to date: $pmc_is_uptodate\n";

    if ( $pmc_is_uptodate ) {
        exec $^X, $pmc 
            unless $filename =~ /\.pm$/;
        exit;
    }

    # delays use'ing until we need it
eval q(
    use FindBin '$Bin';
    use lib
        "$Bin/lib",
        "$Bin/../../../../lib",
        "$Bin/../Pugs-Compiler-Rule/lib",
        "$Bin/../Pugs-Utils/lib",
        "$Bin/../Pugs-Compiler-Precedence/lib",
    ;

    use strict;
    use warnings;

    use Pugs::Compiler::Perl6;
    use Data::Dumper;

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

    open FILE, ">", $pmc
        or die "can't open $pmc for writing";

    print FILE $p6->{perl5}, "\n";

    # print "created .pmc file: $^X $pmc \n";
    exec $^X, $pmc 
        unless $filename =~ /\.pm$/;
);
warn $@ if $@;

1;
