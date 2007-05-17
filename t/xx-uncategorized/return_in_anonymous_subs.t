use v6-alpha;

use Test;

# eval "sub foo { my $ret = { return 3 }; $ret() }; foo().say;"
# ./pugs -e 'sub foo { my $ret = { return 3 }; $ret() }; foo().say;'

plan 2;
skip_rest 'This test is obsolete - See S04 (If you pass a closure object outside of its official "sub" scope, it is illegal to return from it.)';
exit;

# See thread "return() in pointy blocks" by Ingo Blechschmidt on p6l
# L<"http://www.nntp.perl.org/group/perl.perl6.language/21745">
# We may have to change the expected results of this test if Larry changes his
# mind (which I don't hope).

sub bar (Code $return) { $return(42) } 

# { return }
{
  sub foo1 (Code $code) {
    my $return_to_caller = -> $ret { return $ret }; 

    $code($return_to_caller); 
    return 23; 
  } 

  is foo1(&bar), 42, "return() inside anonymous subs works", :todo<bug>; 
}

# same, but the "return" is nested in two (instead of one) blocks:
{
  sub foo2 (Code $code) {
    my $return_to_caller = -> $ret {
      (-> $ret_ { return $ret_ })($ret);
    };

    $code($return_to_caller); 
    return 23; 
  } 

  is foo2(&bar), 42, "return() inside anonymous subs works", :todo<bug>; 
}

=pod

=begin more-discussion-needed

Problem is: How does a plain reference to return know which scope to leave?

# &return
{
  sub baz (Code $return) { $return(42); return 23 }

  is baz(&return), 42, 'calling &return works', :todo<bug>;
}
