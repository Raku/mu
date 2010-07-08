use strict;
use warnings;
use AST::Helpers;
use AST;
use Test::More;
use YAML::XS;
my $empty = [];
trailing_return($empty);
is_deeply($empty,[],"trailing return doesn't modify an empty list");
my $integer = [integer 12];
isa_ok($integer->[0],'AST::IntegerConstant');
trailing_return($integer);
isa_ok($integer->[0],'AST::IntegerConstant');

done_testing;
