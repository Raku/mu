#!/usr/bin/env perl6

use v6;

# Definition:
#   Haskell:   IO a
#   Perl 6:    { a }

# return :: (Monad m) => a -> m a
sub mreturn($a) { return { $a } }

# (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
sub mbind(
  Code $ma,        # m a
  Code $f          # (a -> m b)
       --> Code
) {
  return {
    my $a  = $ma();    # Run m a, yielding a
    my $mb = $f($a);   # Give $f the a, yielding m b
    $mb();             # Return b
  };
}

# (>>) :: (Monad m) => m a -> m b -> m b
sub mbind_ignore_result(Code $ma, Code $mb) {
  return {
    $ma();     # Run m a, ignoring the result
    $mb();     # Run and return m b
  };
}

# fail :: (Monad m) => String -> m a
# (We can't/shouldn't use "fail" in Perl 6, too, as "fail" is a builtin
# in Perl 6.)
sub mfail(Str $a) {
  return {
    die $a;
  };
}

sub sequence_(*@actions where {$_.all ~~ Code} ) {
  return {
    $_() for @actions;
    Nil;
  };
}

sub sequence(*@actions where {$_.all ~~ Code} ) {
  return {
    my @res = @actions.map(-> $action { $action() });
    @res;
  };
}

sub mapM(Code $f, *@input) {
  return {
    @input.map($f).map(-> $i { $i() });
  };
}

# getLine :: IO String
# getLine = ...implemented by the compiler...
sub getLine() {
  return {
    my $line = $*IN.get;
    $line;
  };
}

# putStrLn :: String -> IO ()
# putStrLn = ...implemented by the compiler...
sub putStrLn(Str $x) { return { say $x; Nil } }


# Now some example code:
# This is the one that gets run. 
{
  say "Write a couple of lines here ->";
  my $line_from_user = getLine();
  # Nothing is read yet.
  my $echo = mbind($line_from_user, -> $x { putStrLn($x) });
  # Nothing is read or printed yet.
  my @actions = (getLine(), $echo);
  # Again, nothing printed yet.
  my $both = sequence_(@actions);
  # Only now there're two lines read from the user, and the second one is
  # echoed back.
  $both();
}

{
  # let actions = [getLine, getLine, getLine]
  # actions :: [IO String]
  say "Write three lines";
  my @actions = (getLine(), getLine(), getLine());

  # let results = sequence actions
  # results :: IO [String]
  my $results = sequence(@actions);

  my $echo_prefixed = mbind($results, -> @results {
				   say @results;
    mapM(-> Str $x { putStrLn($x) }, @results);
  });
  $echo_prefixed();
}
