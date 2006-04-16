use lib 
     './lib',
    '../Pugs-Compiler-Rule/lib',
    '../Pugs-Compiler-Precedence/lib',
;

use Pugs::Grammar::Perl6;
use strict;
use warnings;
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

if ( @ARGV ) {
    my @a=<>;
    my $src = join('', @a);
    my $match = Pugs::Grammar::Perl6->parse( $src );
    #use YAML;
    #print Dump $match->();
    print Dumper $match->();
    print "tail: ", ${$match}->{tail},"\n";
    exit;
}

#use Test::More 'no_plan';
print q(#if key:<val> {10 + $a / "abc"}),"\n";
my $match = Pugs::Grammar::Perl6->parse(<<'PERL6');
if key:<val> {
    10 + $a / "abc"
}
1;
PERL6
use YAML;
print Dump $match->();

