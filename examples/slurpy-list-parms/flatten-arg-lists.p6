"# -------------------------------".say;
"# flattening arg lists".say;
"# -------------------------------".say;

sub foo($x, $y, $z) {
  "x foo $x".say;
  "y foo $y".say;
  "z foo $z".say;
}

sub boo(*$x, *$y, *$z) {
  "x boo $x".say;
  "y boo $y".say;
  "z boo $z".say;
}

sub goo(*@x, *@y, *@z) {
  @x[1].say;
  @y[1].say;
  @z[1].say;
}

my @onetothree = 1..3;       # array stores three scalars

foo(1,2,3);                  # okay:  three args found

#foo(@onetothree);           # error: only one arg
boo(@onetothree);            # ok

##### DOES NOT WORK THOUGH S06 SAYS IT SHOULD:
#foo(*@onetothree);          # okay:  @onetothree flattened to three args
#boo(*@onetothree);           # fails


# The * operator flattens lazily -- the array is only flattened if 
# flattening is actually required within the subroutine. To flatten 
# before the list is even passed into the subroutine, use the 
# unary prefix ** operator:

#foo(**@onetothree);          # array flattened before &foo called



