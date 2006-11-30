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
    # Smoke does not complete because of this
    # Uncomment the following two lines when this is fixed
    #use v6-alpha;
    #$y ~= 'book';
  };
};

is $x, 'testing', "scalar modified inside perl5 block";

# Also uncomment this and remove flunk
#is $y, 'casebook', "scalar modified inside perl6 block inside perl5 block";
flunk("Hangs: scalar modified inside perl6 block inside perl5 block");
