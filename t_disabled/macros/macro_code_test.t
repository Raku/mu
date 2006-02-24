#!/usr/bin/pugs

use v6;
use Test;

# Avoiding accidental multiple execution 
# as occurs in C's defines and lisp macros
# This can also ensure a correct order of evaluation

# This needs to be decided
# is CST (Concrete)
# is Thunk
# is Proxy
# is Reduced
# is Bound
# is Once
# is Evaluated
# is EvaluatedOnce
# is Eager
macro max ($x is CST, $y is CST) {
    return CODE { ($x > $y) ?? $x !! $y };
}

my ($x, $y) = (1,2);

my $got = max($x++, $y++);

say qc'$x = {$x} $y = {$y}';

is($x,   2, '$x incremented once');
is($y,   3, '$x incremented once');
is($got, 3, '$got incremented max');

=pod

=head1 CODE & AST splicing options

=head2 Splicing Problem

The contents of CODE blocks need to be able to refer to values
as any other closure does, but also define graft points for
other asts.

=head2 Solution Space

First principle, adding CODE before a block doesn't change things
  - it's still a closure
  - if you mention $var it means the $var in the surrounding lexical scope
    (mentioning new vars may bind at the macro use)
  - subs continue to be bound to the local definitions
  - macros continue to be expanded at parse time (CODE parse time)

Second principle, to do something special, say something special.

Differences: possible variable/sub not defined errors may bind at macro uses
or are delayed.

macro parameters may be ASTs, strings, or possibly a singly-evaluated-ASTs,
the point is to wrap or warp them into the returned AST|string.
We want an easy way to splice the input AST into the output AST
The output AST is usually a CODE block somehow using the macro arg asts:

  CODE { my $x; $x; $input_ast; $captured_var; $var_at_call }

=head2 Some Suggested Solutions

 * Roles, anything that does QuasiQuote is spliced including ASTs,
   ThunkishASTS and certain strings
   literal($ast) macro to talk about an ast as a value
 
 * signature such as CODE ($ast) { $var; $ast }
   (idea--: duplicates macro's param list, violates sig -> application w/ args 
    expectation with subs)
 
 * twigil used for interpolating AST/strings

 * escape meta-syntax (lisp's solution)  
    CODE { say "Exp (\qq[~$package]) = ", \qq[$package] }
 
 * set of escape macros which only apply in CODE blocks glue($ast)

More complex restructuring of the input AST would require walking
the AST (or munging the string).  Nothing spec'd for that yet.
We would likw to ensure only valid ASTs can be produced.

Perl's rules may be able to scan the tree with 
L<S05/"Matching against non-strings"> and just s:g///
to produce the output ast.  

If a tree grammar tool reaches the Perl 6 user space then that can be used

=head1 Recursive Macros

We started thinking about these, but then realized we'd need to
know how they work, so we started thinking about these, ...

These are banned in C but permitted in Lisp (and a source of
infinite loops during compilation).  We could adopt either method
or try to reduce the likely pain.  Lisp allows a recursive expander
function, but not a recursive expansion.

It seems impossible to parse beyond a recursive macro call,
so we can ask Turing's oracle or give up.

One remaining possibility to add the macro expansion to a todo list that
is run after the macro definition is completed.  The effects of the
macro may then be different from the non-recursive case.
It is different to a mere sub call however, as it happens immediately
after the definition.

Finally, a recursive call could merely be a subroutine call, allowing
recursion at runtime (which is compile time as far as the macro's caller
is concerned).  This is perhaps close to C++ template expansion games.

Recursive macros seem to be an unneeded feature, ast's equivalent
to those from a recursive macro can be contructed using normal runtime tools, 
including subroutine recursion.

=head1 Quoting Declarations

Can declarations and pragmas be quoted?
$Larry "I don't see why not"

 CODE { sub x { ... }; macro y { ... }; use force; }

The AST needs to be rescanned anyway, so perform the appropriate
actions at that time in the correct scope.

=cut
# vim: syntax=perl6:
