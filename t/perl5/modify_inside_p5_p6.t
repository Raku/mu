use v6-alpha;
use Test;
plan(2);

my $x = 'test';
my $y = 'case';
{
  use v5;
  $x .= 'ing';

  #the following code hangs pugs.
 {
    use v6-alpha;
    $y ~= 'book';
  };
};

is $x, 'testing', "scalar modified inside perl5 block";
is $y, 'casebook', "scalar modified inside perl6 block inside perl5 block";
