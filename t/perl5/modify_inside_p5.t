use v6-alpha;
use Test;
plan(1);

my $x = 'test';
my $y = 'case';
{
  use v5;
  $x .= 'ing';
};

is $x, 'testing', "scalar modified inside perl5 block";
