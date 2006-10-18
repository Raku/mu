use lib 
    '../Pugs-Compiler-Rule/lib',
    '../Pugs-Compiler-Precedence/lib',
;

use strict;
use warnings;

use Test::More tests => 2;
use Data::Dumper;

use_ok( 'Pugs::Grammar::Pod' );

{
    my $match = Pugs::Grammar::Pod->parse( "=begin ...\n some text\n=end\n" );
    # print Dumper $match->();
my $VAR1 = {
  'pod_block' => ' ...
 some text
=end
'
};
    is_deeply( $match->(), $VAR1, 'begin-end' );
}

