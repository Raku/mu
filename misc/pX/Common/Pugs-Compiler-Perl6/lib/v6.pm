package v6;

# invokes the Perl6-to-Perl5 compiler and creates a .pmc file

use strict;
use warnings;
use Module::Compile-base;
use File::Basename;

my $bin;
BEGIN { $bin = ((dirname(__FILE__) || '.') . "/..") };
use lib (
    "$bin/lib",
    "$bin/../Pugs-Compiler-Rule/lib",
    "$bin/../Pugs-Utils/lib",
    #"$bin/../Pugs-Compiler-Precedence/lib",
    
    "$bin/../lib",
    "$bin/../../Pugs-Compiler-Rule/lib",
    "$bin/../../Pugs-Utils/lib",
    #"$bin/../../Pugs-Compiler-Precedence/lib",
);

sub pmc_can_output { 1 }

sub pmc_compile {
    my ($class, $source) = @_;

    my $file = (caller(4))[1];
    if ($file !~ /\.pm$/i) {
        # Do the freshness check ourselves
        my $pmc = $file.'c';
        my $pmc_is_uptodate = (-s $pmc and (-M $pmc <= -M $file));
        if ($pmc_is_uptodate) {
            do $pmc; die $! if $!; exit 0;
        }
    }

    require Pugs::Compiler::Perl6;
    my $p6 = Pugs::Compiler::Perl6->compile( $source );

    #use Data::Dumper;
    #use YAML;
    #print Dump $match->();
    #print dump_tree $p6;
    #print Dumper $p6;
    # print "tail: ", substr( ${$match}->{tail}, 0, 20 ),"...\n";
    #print "P5: ",$p6->{perl5}, "\n";

    $p6->{perl5} =~ s/do\{(.*)\}/$1/s;
    return $p6->{perl5}."\n";
}

1;
