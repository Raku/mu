module Parse::Rule::Core;

=pod

Medium is an abstract class (currently with no operations whatsoever)
that represents a position in a stream of stuff.  The usual example of
Medium is Text (see Media.pm), which stores a string and a position in
that string.  But it could very well store an array with an index, or a
closure-generated recurrence, or an elephant.

=cut

class Medium { }

=pod

C<Match> is the object that holds scope-local state; i.e. state that
gets allocated anew whenever you enter a new rule or parenthesized
group.  This is approximately the Match object that userland folks will
see, but it has some other clutter because of unclear design.

    $.start      - The place where this subrule/group started.
    $.end        - The place where this subrule/group ended.  C<Medium>
                   represents positions I<between> elements, so if you're
                   thinking of these as numbers, this will be one past the
                   last character matched.
    $.match_num  - Numeric captures: $/[0], $/[1], ...
    $.match_name - Named captures: $/<foo>, $/<bar>, ...
    $.multidex   - The "quantifier index".  This is an array that is an
                   index into the multidimensional arrays of the match
                   object, telling captures where in the deep structure
                   to store what they've captured.

=cut

class Match {
    has Medium $.start;
    has Medium $.end;
    
    has Match $.capture_num;
    has Match $.capture_name;

    has $.multidex;  # Match doesn't seem the right place to store this
                     # but neither does Result
                     # maybe there's a level of indirection in between

    submethod BUILD () {
        $.capture_num //= [];
        $.capture_name //= {};
        $.multidex //= [];
    }
}

=pod

C<Result> is the match state that is passed around.  It essentially
forms a linked list through the rule as it is executed, but the linked
list is done in code (i.e. with closures).  It has the following
members:

    $.backtrack - Parsers tail-call this if they fail (see below)
    $.marks     - A hash that stores backtrack closures to various.
                  points earlier in the match.  This is used to
                  implement backtracking controls.  a*: ...
    $.pos       - The current position of the match (also holds the
                  string that is being matched against).
    $.match     - State that is scope-local.  A new match object gets
                  allocated when you enter a new rule or parenthesized
                  group.

=cut

class Result {
    has Code $.backtrack;
    has Hash $.marks;
    has Medium $.pos;
    has Match $.match;

    submethod BUILD () {
        $.marks //= {};
    }
}

=pod

A C<Parser> is simply an object wrapper around a closure.  The closure,
C<$.parse>, takes two parameters:  a C<Result> object and a continuation
(which takes in turn a C<Result> object as a parameter).  If the parser
succeeds, it tail-calls the continuation (i.e. returns whatever the
continuation returns) with an updated C<Result> object.  If the parser
fails, it tail-calls $result.backtrack()().  Backtracking parsers such
as quantifiers are implemented by passing a C<Result> object that has a
$.backtrack closure that calls back into the parser.

=cut

class Parser {
    has Code $.parse;
}

# vim: ft=perl6 :
