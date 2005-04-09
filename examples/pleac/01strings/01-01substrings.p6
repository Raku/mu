#!perl6

use v6;

my ($string, $offset, $count) = ('Pugs is da bomb', 2, 5);
say $string.substr $offset, $count;
say $string.substr $offset;

# fails with 
# Cannot modify constant item
#App "&substr" [] [Var "$string",Var "$offset"]
#substr($string, $offset) = "gilism ain't for wimps";
#say $value;
$string.substr $offset = 'stuff';
say $string;
say $offset;
