#!/usr/bin/perl
=pod
2005 September 7

This file is a development snapshot of the prolog regex engine I
started over the weekend.  A "look at what I'm up to".

It passes most (95%) of a perl 5.9 re test file (re_tests), but it's
real p5 re coverage is much less.  The test passing was simply to
permit refactoring with confidence.

It uses Language::Prolog::Yaswi, which requires swi-prolog be
installed with non-default settings, and so is not intended for
"general" use.  It's more of a pugs bootstrap exercise.

This code is currently completely in flux.  It is simultaneously being
overhauled, cleaned up, and p6 added.  It's basically three days old,
and only now is changing from an experimental "let's see if something
like this can work" to "ok, this looks doable".  So yes, the code is
an utter mess.

The project objectives are nominally:
  1- Provide an environment in which p6 rules can be run against strings,
  sufficient to permit recommencing writing a grammar for perl6.
  This will likely be pure p5/prolog, with no p6 runtime associated.
Then, 
  - aid in prototyping the regexp system - the regexp tree, its
  compilation to pil, how regexp modification macros work, etc.
  Working with a p5 accessible runtime (pil2js or pilrun), and/or
  motivating implementations in p5/p6/haskell.
  - perhaps serve as a reference implementation for a while.  By
  sacrificing ease of installation and portability, we may have
  bought fairly simple code with tolerable performance.

However, pugs critical path is currently more oo than rules, my time
is limited, and there are other bootstrap approaches to rules, so this
file may or may not go anywhere.  The plan as of this moment is to
continue cleaning and adding p6, tuck it into the pugs distribution
somewhere with test files, and do objective 1 above.

If anyone would like to help out, let me know, and I'll upload more of
the development environment.

Recognition is due
  Salvador Fandino Garcia  for Language::Prolog::Yaswi,
    http://search.cpan.org/~salva/Language-Prolog-Yaswi-0.08/
and
  Robert D. Cameron for his "Perl Style Regular Expressions in Prolog".
    http://www.cs.sfu.ca/people/Faculty/cameron/Teaching/383/regexp-plg.html
Without their work, I almost certainly would have punted.
Also to iblech, and Michael Schilli of JavaScript::SpiderMonkey, whose
successful work got me thinking about trying a prolog plugin.

Fyi,
Yaswi apparently requires swi prolog be built ./configure --enable-shared.
=cut
# export LD_LIBRARY_PATH=/usr/local/lib/pl-5.5.27/lib/x86_64-linux
my $prolog = <<'END';
%======================================================================

:- style_check(-discontiguous). % remove is plausible

p5_start(Z)    --> p5_concatenation(W), p5_alternatives(W, Z), !.
p5_start(noop) --> {true}.

p5_alternatives(W, Z) --> "|", p5_concatenation(X), p5_alternatives(union(W,X), Z), !.
p5_alternatives(W, W) --> {true}.


p6_start(Z)    --> p6_conjunct(W), p6_alternatives(W, Z).

p6_conjunct(Z) --> p6_concatenation(W), p6_conjunctions(W,Z).

p6_alternatives(W, Z) --> "|", p6_conjunction(X), p6_alternatives(union(W,X), Z), !.
p6_alternatives(W, W) --> {true}.

p6_conjunctions(W, Z) --> "&", p6_concatenation(X), p6_conjunctions(intersect(W,X), Z), !.
p6_conjunctions(W, W) --> {true}.


p5_concatenation(Z)    --> p5_quantified(W), p5_concatenation_more(W, Z), !.
p5_concatenation(noop) --> {true}.
p5_concatenation_more(W, Z) --> p5_quantified(X), p5_concatenation_more(conc(W,X), Z), !.
p5_concatenation_more(W, W) --> {true}.

p6_concatenation(Z)    --> p6_quantified(W), p6_concatenation_more(W, Z), !.
p6_concatenation_more(W, Z) --> p6_quantified(X), p6_concatenation_more(conc(W,X), Z), !.
p6_concatenation_more(W, W) --> {true}.


p5_quantified(Z) --> p5_elemental(W), p5_quantified_op(W, Z).
p5_quantified_op(W, Z) --> p56_quantified_op(W,Z), !.
p5_quantified_op(W, Z) --> p5_range(W, Z), !.
p5_quantified_op(W, W) --> {true}.

p6_quantified(Z) --> p6_elemental(W), p6_quantified_op(W, Z).
p6_quantified_op(W, Z) --> p56_quantified_op(W,Z), !.
p6_quantified_op(W, Z) --> p6_range(W, Z), !.
p6_quantified_op(W, W) --> {true}.

p56_quantified_op(W, ques_ng(W)) --> "??", !.
p56_quantified_op(W, star_ng(W)) --> "*?", !.
p56_quantified_op(W, plus_ng(W)) --> "+?", !.
p56_quantified_op(W, ques(W))    --> "?", !.
p56_quantified_op(W, star(W))    --> "*", !.
p56_quantified_op(W, plus(W))    --> "+", !.

p5_range(W, Z) --> "{", !, p5_range_bounds(Min,Max), "}", p5_range_greed(W,Min,Max,Z), !.
p5_range_greed(W,Min,Max,repeat_ng(W,Min,Max)) --> "?", !.
p5_range_greed(W,Min,Max,   repeat(W,Min,Max)) --> {true}.

p5_range_bounds(Min,Max) --> digits_int(Min), !, p5_range_bounds_RHS(Min,Max), !.
p5_range_bounds( 0 ,Max) --> ",", digits_int(Max).
p5_range_bounds_RHS(_Min,Max) --> ",", digits_int(Max), !.
p5_range_bounds_RHS(_Min,-)   --> ",", !.
p5_range_bounds_RHS(Min ,Min) --> {true}.

%# XXX - spaces permitted around **
p6_range(W, Z) --> "**{", !, p6_range_bounds(Min,Max), "}", p6_range_greed(W,Min,Max,Z), !.
p6_range_greed(W,Min,Max,repeat_ng(W,Min,Max)) --> "?", !.
p6_range_greed(W,Min,Max,   repeat(W,Min,Max)) --> {true}.

p6_range_bounds(Min,Max) --> digits_int(Min), !, p6_range_bounds_RHS(Min,Max), !.
p6_range_bounds( 0 ,Max) --> "...", digits_int(Max).
p6_range_bounds_RHS(_Min,Max) --> "..", digits_int(Max), !.
p6_range_bounds_RHS(_Min,-)   --> "...", !.
p6_range_bounds_RHS(Min ,Min) --> {true}.



p5_elemental(group(R))            --> "(?:",  !, p5_start(R), ")".
p5_elemental(standalone(R))       --> "(?>",  !, p5_start(R), ")".
p5_elemental(lookahead(R))        --> "(?=",  !, p5_start(R), ")".
p5_elemental(lookahead_not(R))    --> "(?!",  !, p5_start(R), ")".
p5_elemental(lookbehind(R))       --> "(?<=", !, p5_start(Rx), {re_reverse(Rx,R)}, ")".
p5_elemental(lookbehind_not(R))   --> "(?<!", !, p5_start(Rx), {re_reverse(Rx,R)}, ")".
p5_elemental(condition(Rk,R1,R2)) --> "(?", p5_condition_body(Rk,R1,R2), !.
p5_elemental(X)                   --> "(?", p5_modifier_spec(M), !, p5_modifier_scope(M,X).
p5_elemental(cap(X))              --> "(", !, p5_start(X), ")".
p5_elemental(bos)   --> "\\A".
p5_elemental(eosnl) --> "\\Z".
p5_elemental(eos)   --> "\\z".
p5_elemental(bol)   --> "^".
p5_elemental(eol)   --> "$". %"
p5_elemental(word_boundary)     --> "\\b", !.
p5_elemental(word_boundary_not) --> "\\B", !.
p5_elemental(dot)       --> ".".
p5_elemental(R)         --> p56_char_class(R).
p5_elemental(R)         --> p5_simple_escape_sequence(R).
p5_elemental(negSet(X)) --> "[^", !, p5_charset_def(X), "]".
p5_elemental(posSet(X)) --> "[", p5_charset_def(X), "]".
p5_elemental(matchref(I)) --> p5_matchref(I).
p5_elemental(ws(C))   --> [C], {isSpacePerl(C)}, !.
p5_elemental(char(C)) --> [C], {\+(p5_metachar([C]))}.
p5_elemental(char(C)) --> "\\", [C], {p5_metachar([C])}.
%# XXX - needs overhaul
p5_metachar("\\").
p5_metachar("\|").
p5_metachar("?").
p5_metachar("*").
p5_metachar("+").
p5_metachar("\.").
p5_metachar("[").
p5_metachar("^").
p5_metachar("$"). %"
p5_metachar("(").
p5_metachar(")").
p5_metachar("{").

isP6metacharacter(C) :- member(C,"\{\}\[\]\(\)\^\$\.\|\*\+\?\#\\").
p6_metachar(C) --> [C], {isP6metacharacter(C)}.

p5_matchref(I) --> "\\", digits_int(I1), {I is I1 - 1},
  {I1 \= 0}, {(I1 =< 9; fail)}, !.

p5_condition_body(Rk,R1,R2) --> p5_condition_test(Rk), p5_concatenation(R1), p5_condition_alternative(R2).
p5_condition_alternative(R)    --> "|", p5_concatenation(R), ")", !.
p5_condition_alternative(noop) --> ")". 
p5_condition_test(lookahead(R))      --> p5_elemental(lookahead(R)).
p5_condition_test(lookahead_not(R))  --> p5_elemental(lookahead_not(R)).
p5_condition_test(lookbehind(R))     --> p5_elemental(lookbehind(R)).
p5_condition_test(lookbehind_not(R)) --> p5_elemental(lookbehind_not(R)).
p5_condition_test(condi(I))          --> "(", digits_int(I1), ")", {I is I1 - 1}.

p5_modifier_scope(M,modify(M))         --> ")", !.
p5_modifier_scope(M,group_modify(M,R)) --> ":", !, p5_start(R), ")", !.

p5_modifier_spec(L) --> p5_modifier_spec_pos(L).
p5_modifier_spec_pos([i|L])  --> "i", p5_modifier_spec_pos(L), !.
p5_modifier_spec_pos([m|L])  --> "m", p5_modifier_spec_pos(L), !.
p5_modifier_spec_pos([s|L])  --> "s", p5_modifier_spec_pos(L), !.
p5_modifier_spec_pos([x|L])  --> "x", p5_modifier_spec_pos(L), !.
p5_modifier_spec_pos(L)      --> "-", p5_modifier_spec_neg(L), !.
p5_modifier_spec_pos([])     --> {true}.
p5_modifier_spec_neg([-i|L]) --> "i", p5_modifier_spec_neg(L), !.
p5_modifier_spec_neg([-m|L]) --> "m", p5_modifier_spec_neg(L), !.
p5_modifier_spec_neg([-s|L]) --> "s", p5_modifier_spec_neg(L), !.
p5_modifier_spec_neg([-x|L]) --> "x", p5_modifier_spec_neg(L), !.
p5_modifier_spec_neg([])     --> {true}.

p5_charset_def([char(45)|L]) --> "-", p5_charset_items_opt(L0), p5_charset_def_x(L0,L), !. % 45 is -
p5_charset_def([char(93)|L]) --> "]", p5_charset_items_opt(L0), p5_charset_def_x(L0,L), !. % 93 is ]
p5_charset_def(L)            -->      p5_charset_items(L0), p5_charset_def_x(L0,L).
p5_charset_def_x(L0,L1) --> "-", {append(L0,[char(45)],L1)}, !.
p5_charset_def_x(L0,L0) --> {true}.
p5_charset_items([Item1|MoreItems])    --> p5_charset_item(Item1), p5_charset_items_opt(MoreItems).
p5_charset_items_opt([Item1|MoreItems]) --> p5_charset_item(Item1), p5_charset_items_opt(MoreItems).
p5_charset_items_opt([])                --> {true}.
p5_charset_item(class_neg(X)) --> "[:^", charset_identifier(X), ":]", !.
p5_charset_item(class_pos(X)) --> "[:",  charset_identifier(X), ":]", !.
p5_charset_item(R) --> p56_char_class(R).
p5_charset_item(R) --> p5_simple_escape_sequence(R).
p5_charset_item(setrange(A,B)) --> p5_charset_item(char(A)), "-", p5_charset_range_x(B).
p5_charset_range_x(_) --> p5_charset_item(class_pos(_)), {!, fail}.
p5_charset_range_x(_) --> p5_charset_item(class_neg(_)), {!, fail}.
p5_charset_range_x(B) --> p5_charset_item(char(B)).
p5_charset_item(char(C)) --> [C], {\+(set_metachar([C]))}.
p5_charset_item(char(C)) --> "\\", [C], {set_metachar([C])}.
%# XXX - should permit \misc, no?
set_metachar("\\").
set_metachar("]").
set_metachar("-").
p5_charset_item(char(C)) --> [C], {[C] = "-"}.

charset_identifier(A) --> alnums(L), { atom_codes(A,L) }.

p56_char_class(class_pos(digit))         --> "\\d", !.
p56_char_class(class_neg(digit))         --> "\\D", !.
p56_char_class(class_pos(space_perl))    --> "\\s", !.
p56_char_class(class_neg(space_perl))    --> "\\S", !.
p56_char_class(class_pos(word))          --> "\\w", !.
p56_char_class(class_neg(word))          --> "\\W", !.

p6_char_class(class_pos(horizontal_space))  --> "\\h", !.
p6_char_class(class_neg(horizontal_space))  --> "\\H", !.
p6_char_class(class_pos(vertical_space))    --> "\\v", !.
p6_char_class(class_neg(vertical_space))    --> "\\H", !.
p6_char_class(class_pos(newline))           --> "\\n", !. %# XXX - not quite?
p6_char_class(class_neg(newline))           --> "\\N", !.

p5_simple_escape_sequence(char(C)) --> "\\n", {[C] = "\n"}, !. %# XXX - not really
p5_simple_escape_sequence(char(C)) --> "\\r", {[C] = "\r"}, !.
p5_simple_escape_sequence(char(C)) --> "\\t", {[C] = "\t"}, !. %# XXX - more

p6_simple_escape_sequence(char(C)) --> "\\t", {[C] = "\t"}, !.
p6_simple_escape_sequence(char(C)) --> "\\r", {[C] = "\r"}, !.
p6_simple_escape_sequence(char(C)) --> "\\f", {[C] = "\f"}, !.
p6_simple_escape_sequence(char(C)) --> "\\e", {[C] = [0x1B]}, !.


% XXX - connect to above
p5_comment_opt --> p5_comment(_), !.
p5_comment_opt --> {true}.
p5_comment(L) --> "(?#", !, p5_comment_body(L), ")".
p5_comment_body([C|L]) --> [C], {[C] \== ")"}, p5_comment_body(L), !.
p5_comment_body([])    --> {true}.

p6_comment --> "#", rest_of_line(_).
rest_of_line([10])  --> [10], !.
rest_of_line([C|T]) --> [C], rest_of_line(T).
rest_of_line([]).


p6_octidecimal_escape_sequence(X) --> "\\0", p6_oct_spec(X), !.
p6_oct_spec(char(C)) --> octaldigit3_int(C), !.
p6_oct_spec(R)       --> [Open], brackets(Open,Close), p6_oct_chars(R), [Close].
p6_oct_chars(R) --> octdigits_int(C), p6_oct_chars_more(char(C),R).
p6_oct_chars_more(Rc0,conc(R,char(C))) --> ";", octdigits_int(C), p6_oct_chars_more(Rc0,R), !.
p6_oct_chars_more(Rc0,Rc0)             --> {true}.

p6_hexidecimal_escape_sequence(X) --> "\\", ("x";"X"), !, p6_hex_spec(X), !.
p6_hex_spec(char(C)) --> hexdigits4_int(C), !.
p6_hex_spec(char(C)) --> hexdigits2_int(C), !.
p6_hex_spec(R)       --> [Open], brackets(Open,Close), p6_hex_chars(R), [Close].
p6_hex_chars(R) --> hexdigits_int(C), p6_hex_chars_more(char(C),R).
p6_hex_chars_more(Rc0,conc(R,char(C))) --> ";", hexdigits_int(C), p6_hex_chars_more(Rc0,R), !.
p6_hex_chars_more(Rc0,Rc0)             --> {true}.

p6_named_character(char(C)) --> "\\", ("c","C"), [Open], brackets(Open,Close),
  p6_named_character_tail(Name,Close), !, uncode_charname_to_char(Name,C), !.
p6_named_character_tail([],Close)    --> [Close], !.
p6_named_character_tail([C,T],Close) --> [C], p6_named_character_tail(T,Close).


brackets("{","}"). brackets("(",")"). brackets("[","]").
brackets("<",">"). brackets(">","<").
brackets("/","/"). brackets("!","!"). brackets("=","=").
brackets("?","?"). brackets("#","#").
brackets([0xabd],[0xbbd]). brackets([0xbbd],[0xabd]).


digits_int(I) --> digit(D0), digits(D), { number_chars(I, [D0|D]) }.
digits([D|T]) --> digit(D), digits(T), !.
digits([])    --> [].
digit(D) --> [D], { code_type(D, digit) }.

hexdigits_int(I) --> hexdigit(D0), hexdigits(D), { number_chars(I, [42,120,D0|D]) }.% [42,120]="0x"
hexdigits([D|T]) --> hexdigit(D), hexdigits(T), !.
hexdigits([])    --> [].
hexdigit(D) --> [D], { code_type(D, xdigit(_)) }.

hexdigits4_int(I) -->
  hexdigit(D0), hexdigit(D1),
  hexdigit(D2), hexdigit(D3), {number_chars(I,[42,120,D0,D1,D2,D3])}.
hexdigits2_int(I) -->
  hexdigit(D0), hexdigit(D1), {number_chars(I,[42,120,D0,D1])}.

octdigits_int(I) --> octdigit(D0), octdigits(D), { number_chars(I, [42,111,D0|D]) }.% [42,111]="0o"
octdigits([D|T]) --> octdigit(D), octdigits(T), !.
octdigits([])    --> [].
octdigit(D) --> [D], { member(D,"01234567") }.

octdigits3_int(I) -->
  octdigit(D0), octdigit(D1), octdigit(D2), {number_chars(I,[42,111,D0,D1,D2])}.


alnums([C|T]) --> [C], {code_type(C,alnum)}, alnums(T), !.
alnums([]) --> {true}.

uncode_charname_to_char(Name,C) :- cached_uncode_charname_to_char(Name,C), !.
uncode_charname_to_char(Name,C) :-
  atom_codes(AName,Name),
  concat_atom(['use charnames ":full";unpack("U*","\\N{',AName,'}\n")'],Cmd),
  !, perl5_eval(Cmd,[C,10]), !, % XXX - warn on failure
  assert(cached_uncode_charname_to_char(Name,C)), !.

%-------------------------

re_reverse(conc(Ra0,Rb0),conc(Rb1,Ra1)) :- re_reverse(Ra0,Ra1), re_reverse(Rb0,Rb1), !.
%# XXX - not quite right... need some symetric op pairs.
re_reverse(bos,eos).
re_reverse(eos,bos).
re_reverse(bol,eol).
re_reverse(eol,bol).
%# XXX - captures in reverse dont currently work.
%# XXX - more...
re_reverse(R,R).

%----------------------------------------------------------------------

re_rewrite(R0,R1) :- re_rewrite(R0,R1,rewr(0),_). 

re_rewrite(plus(cap(plus(R))),R1,RW0,RW1) :-
  re_rewrite(cap(plus(R)),R1,RW0,RW1).
re_rewrite(cap(R0),capture(N,R1),RW0,RW1) :-
  !, RW0 = rewr(N), N1 is N + 1, RWx = rewr(N1),
  re_rewrite(R0,R1,RWx,RW1).

re_rewrite(R0,R1,RW0,RW1) :-
  R0 =.. [P|A], re_rewrite_list(A,A1,RW0,RW1), R1 =.. [P|A1], !.
re_rewrite(X,X,RW0,RW0).
re_rewrite_list([],[],RW0,RW0).
re_rewrite_list([T0|L0],[T1|L1],RW0,RW1) :-
  re_rewrite(T0,T1,RW0,RWx), re_rewrite_list(L0,L1,RWx,RW1).

re_parse(Pat,Re) :-
  p5_start(Re0,Pat,[]),
  re_rewrite(Re0,Re).

%----------------------------------------------------------------------

rm(noop,F0,F0,B0,B0,E0,E0).

rm(standalone(Re),F0,F1,B0,B1,E0,E1) :- 
  rm(Re,F0,F1,B0,B1,E0,E1), !. % this cut is semantic.

rm(condition(condi(I),Ra,Rb),F0,F1,B0,B1,E0,E1) :- !, 
  %# XXX - move (is "capture matched" not)
  ( \+(match_captures_get(I,E0,_)) ; match_captures_get(I,E0,X), var(X) )
  -> rm(Rb,F0,F1,B0,B1,E0,E1) ; rm(Ra,F0,F1,B0,B1,E0,E1).
rm(condition(Rk,Ra,Rb),F0,F1,B0,B1,E0,E1) :- !, 
  rm(Rk,F0,_,B0,_,E0,_)
  -> rm(Ra,F0,F1,B0,B1,E0,E1) ; rm(Rb,F0,F1,B0,B1,E0,E1).

rm(lookahead(Re),F0,F0,B0,B0,E0,E0)     :-    rm(Re,F0,_,B0,_,E0,_).
rm(lookahead_not(Re),F0,F0,B0,B0,E0,E0) :- \+(rm(Re,F0,_,B0,_,E0,_)).

rm(lookbehind(Re),F0,F0,B0,B0,E0,E0)     :-    rm(Re,B0,_,F0,_,E0,_).
rm(lookbehind_not(Re),F0,F0,B0,B0,E0,E0) :- \+(rm(Re,B0,_,F0,_,E0,_)).

rm(modify(M),F0,F0,B0,B0,E0,E1) :- 
  set_modifiers(E0,M,E1).
rm(group_modify(M,Re),F0,F1,B0,B1,E0,E1) :- 
  set_modifiers(E0,M,Ex),
  rm(Re,F0,F1,B0,B1,Ex,Ey),
  set_modifiers_from(Ey,E0,E1).

rm(group(Re),F0,F1,B0,B1,E0,E1) :-
  rm(Re,F0,F1,B0,B1,E0,Ex),
  set_modifiers_from(Ex,E0,E1).


rm(union(Re1,_Re2),F0,F1,B0,B1,E0,E1) :- rm(Re1,F0,F1,B0,B1,E0,E1).
rm(union(_Re1,Re2),F0,F1,B0,B1,E0,E1) :- rm(Re2,F0,F1,B0,B1,E0,E1).

rm(intersection(Re1,Re2),F0,F1,B0,B1,E0,E1) :-
  rm(Re1,F0,F1,B0,B1,E0,Ex),
  rm(Re2,F0,F1,B0,B1,Ex,E1).

rm(conc(Re1,Re2),F0,F1,B0,B1,E0,E1) :- 
  rm(Re1,F0,Fx,B0,Bx,E0,Ex),
  rm(Re2,Fx,F1,Bx,B1,Ex,E1).


rm(ques_ng(_Re),F0,F0,B0,B0,E0,E0).
rm(ques_ng(Re),F0,F1,B0,B1,E0,E1) :- rm(Re,F0,F1,B0,B1,E0,E1).

rm(star_ng(_Re),F0,F0,B0,B0,E0,E0).
rm(star_ng(Re),F0,F1,B0,B1,E0,E1) :-
  rm(Re,      F0,Fx,B0,Bx,E0,Ex),
  ( (F0 == Fx) -> (Fx = F1, Bx = B1, Ex = E1) ; rm(star_ng(Re),Fx,F1,Bx,B1,Ex,E1) ).

rm(plus_ng(Re),F0,F1,B0,B1,E0,E1) :-
  rm(Re,      F0,Fx,B0,Bx,E0,Ex),
  ( (F0 == Fx) -> (Fx = F1, Bx = B1, Ex = E1) ; rm(star_ng(Re),Fx,F1,Bx,B1,Ex,E1) ).

rm(ques(Re),F0,F1,B0,B1,E0,E1) :- rm(Re,F0,F1,B0,B1,E0,E1).
rm(ques(_Re),F0,F0,B0,B0,E0,E0).

rm(star(Re),F0,F1,B0,B1,E0,E1) :-
  rm(Re,      F0,Fx,B0,Bx,E0,Ex),
  ( (F0 == Fx) -> (Fx = F1, Bx = B1, Ex = E1) ; rm(star(Re),Fx,F1,Bx,B1,Ex,E1) ).
rm(star(_Re),F0,F0,B0,B0,E0,E0).

rm(plus(Re),F0,F1,B0,B1,E0,E1) :-
  rm(Re,      F0,Fx,B0,Bx,E0,Ex),
  ( (F0 == Fx) -> (Fx = F1, Bx = B1, Ex = E1) ; rm(star(Re),Fx,F1,Bx,B1,Ex,E1) ).


rm(repeat(Re,Min, - ),F0,F1,B0,B1,E0,E1) :-
  rm_repeat_min(Re,Min,F0,Fx,B0,Bx,E0,Ex),
  rm(star(Re),Fx,F1,Bx,B1,Ex,E1).
rm(repeat(Re,Min,Max),F0,F1,B0,B1,E0,E1) :- 
  number(Max), Min =< Max,
  rm_repeat_min(Re,Min,F0,Fx,B0,Bx,E0,Ex),
  Left is Max - Min,
  rm_repeat_max(Re,Left,Fx,F1,Bx,B1,Ex,E1).

rm(repeat_ng(Re,Min, - ),F0,F1,B0,B1,E0,E1) :-
  rm_repeat_min(Re,Min,F0,Fx,B0,Bx,E0,Ex),
  rm(star_ng(Re),Fx,F1,Bx,B1,Ex,E1).
rm(repeat_ng(Re,Min,Max),F0,F1,B0,B1,E0,E1) :- 
  number(Max), Min =< Max,
  rm_repeat_min(Re,Min,F0,Fx,B0,Bx,E0,Ex),
  Left is Max - Min,
  rm_repeat_max_ng(Re,Left,Fx,F1,Bx,B1,Ex,E1).


rm_repeat_min(_Re,0,F0,F0,B0,B0,E0,E0).
rm_repeat_min(Re,N,F0,F1,B0,B1,E0,E1) :-
  rm(Re,F0,Fx,B0,Bx,E0,Ex),
  N1 is N - 1,
  rm_repeat_min(Re,N1,Fx,F1,Bx,B1,Ex,E1).

rm_repeat_max(_Re,0,F0,F0,B0,B0,E0,E0).
rm_repeat_max(Re,N,F0,F1,B0,B1,E0,E1) :-
  rm(Re,F0,Fx,B0,Bx,E0,Ex),
  N1 is N - 1,
  rm_repeat_max(Re,N1,Fx,F1,Bx,B1,Ex,E1).
rm_repeat_max(_Re,_,F0,F0,B0,B0,E0,E0).

rm_repeat_max_ng(_Re,0,F0,F0,B0,B0,E0,E0).
rm_repeat_max_ng(_Re,_,F0,F0,B0,B0,E0,E0).
rm_repeat_max_ng(Re,N,F0,F1,B0,B1,E0,E1) :-
  rm(Re,F0,Fx,B0,Bx,E0,Ex),
  N1 is N - 1,
  rm_repeat_max_ng(Re,N1,Fx,F1,Bx,B1,Ex,E1).


rm(capture(N,Re),F0,F1,B0,B1,E0,E1) :-
  % match_captures_nest(E0,Ex0),
  % rm(Re,F0,F1,B0,B1,Ex0,Ex),
  rm(Re,F0,F1,B0,B1,E0,Ex),
  append(Str,F1,F0),
  length(F0,NPos),
  match_captures_set(N,[Str,NPos,[]],Ex,Ey),
  set_modifiers_from(Ey,E0,E1).
  % match_captures(E0,C0),
  % match_captures(Ex,Cx),
  % append(C0,[[Str,NPos,[]]],Cy), append(Cy,Cx,C1),
  % match_captures(Ex,C1,E1).
rm(matchref(I),F0,F1,B0,B1,E0,E1) :-
  match_captures_get(I,E0,M), nonvar(M), %# XXX - excess caution
  M = [Str|_], %# XXX - need api
  re_parse(Str,Re), %# XXX - eeeeeeeep.  and need to quote.
  rm(Re,F0,F1,B0,B1,E0,E1).


rm(dot,         [C|F1],F1,B0,[C|B0],E0,E0) :- C \= 10, !. %\n
rm(dot,         [C|F1],F1,B0,[C|B0],E0,E0) :- flag_s(E0), !.
%#rm(ws(C),      [C|F1],F1,B0,B0,E0,E0) :- flag_x(E0), !. XXX - so how???
rm(ws(C),       [C|F1],F1,B0,[C|B0],E0,E0) :- !.
rm(char(C),     [C|F1],F1,B0,[C|B0],E0,E0) :- !.
rm(char(C),     [Ci|F1],F1,B0,[C|B0],E0,E0) :- flag_i(E0), case_insensitive_char_eq(C,Ci), !.
rm(class_pos(X),[C|F1],F1,B0,[C|B0],E0,E0) :- class_member(class_pos(X),C), !.
rm(class_neg(X),[C|F1],F1,B0,[C|B0],E0,E0) :- class_member(class_neg(X),C), !.
rm(eos,         [],[],B0,B0,E0,E0) :- !.
rm(eosnl,       [],[],B0,B0,E0,E0) :- !.
rm(eosnl,       [10],[10],B0,B0,E0,E0) :- !.
rm(bos,         F0,F0,[],[],E0,E0) :- !.
rm(bol,         F0,F0,[],[],E0,E0) :- !.
rm(bol,         [X|Fx],[X|Fx],[10|Bx],[10|Bx],E0,E0) :- flag_m(E0), !.
rm(eol,         [],[],B0,B0,E0,E0) :- !.
rm(eol,         [10],[10],B0,B0,E0,E0) :- !.
rm(eol,         [10|Fx],[10|Fx],B0,B0,E0,E0) :- flag_m(E0), !.
rm(word_boundary,[C|Fx],[C|Fx],[],[],E0,E0) :- !, char_type(C,csym).
rm(word_boundary,[],[],[C|Bx],[C|Bx],E0,E0) :- !, char_type(C,csym).
rm(word_boundary,F0,F0,B0,B0,E0,E0) :- 
   F0 = [CF|_], B0 = [CB|_],
   ( char_type(CF,csym) -> \+(char_type(CB,csym)) ; char_type(CB,csym) ), !.
rm(word_boundary_not,F0,F0,B0,B0,E0,E0) :- 
   F0 = [CF|_], B0 = [CB|_],
   ( char_type(CF,csym) -> char_type(CB,csym) ; \+(char_type(CB,csym)) ), !.

rm(negSet(Set),[C|F1],F1,B0,[C|B0],E0,E0) :- \+(charset_member(E0,C,Set)).
rm(posSet(Set),[C|F1],F1,B0,[C|B0],E0,E0) :-    charset_member(E0,C,Set).

class_member(class_pos(space_perl),C) :- !,   (char_type(C,space) ; C = 11). %\v chr(11)
class_member(class_neg(space_perl),C) :- !, \+(char_type(C,space)), C \= 11.
class_member(class_pos(word)      ,C) :- !,    char_type(C,csym).
class_member(class_neg(word)      ,C) :- !, \+(char_type(C,csym)).
class_member(class_pos(X)         ,C) :- catch(   char_type(C, X),_,fail), !.
class_member(class_neg(X)         ,C) :- catch(\+(char_type(C, X)),_,fail), !.
class_member(class_pos(xdigit)    ,C) :- !,    char_type(C,xdigit(_)).
class_member(class_neg(xdigit)    ,C) :- !, \+(char_type(C,xdigit(_))).
class_member(class_pos(ascii)     ,C) :- !,    isAscii(C).
class_member(class_neg(ascii)     ,C) :- !, \+(isAscii(C)).
class_member(class_pos(print)     ,C) :- !,    isPrint(C).
class_member(class_neg(print)     ,C) :- !, \+(isPrint(C)).

isAscii(C)     :- 0 =< C, C =< 127.
isPrint(C)     :- (char_type(C,alnum);char_type(C,punct);C=32),!. % 32 = space
isSpacePerl(C) :- (char_type(C,space) ; C = 11), !. %\v chr(11)

case_insensitive_char_eq(Ca,Cb) :-
  char_type(Ca,to_lower(C)), char_type(Cb,to_lower(C)).

charset_member(_E,C,[char(C)|_]) :- !.
charset_member(E0,C,[char(X)|_]) :- flag_i(E0), case_insensitive_char_eq(C,X), !.
charset_member(_E,C,[setrange(C1,C2)|_]) :- C1 =< C, C =< C2, !.
charset_member(E0,C,[setrange(C1,C2)|_]) :-
   flag_i(E0),
   code_type(C,to_lower(Cx)), code_type(C1,to_lower(C1x)), code_type(C2,to_lower(C2x)),
   C1x =< Cx, Cx =< C2x, !.
charset_member(_,C,[class_pos(X)|_])    :- class_member(class_pos(X),C), !.
charset_member(_,C,[class_neg(X)|_])    :- class_member(class_neg(X),C), !.
charset_member(E0,C,[_|T]) :- charset_member(E0,C,T).

%----------------------------------------------------------------------

rmSomewhere(Re,F0,F1,B0,B1,E0,E1) :-
  rm(Re,F0,F1,B0,B1,E0,Ex),
  append(Str,F1,F0),
  length(F0,NPos),
  match_captures(Ex,Cx),
  C1 = [Str,NPos,Cx],
  match_captures(Ex,C1,E1).
rmSomewhere(Re,[C|Fx],F1,B0,B1,E0,E1) :-
  rmSomewhere(Re,Fx,F1,[C|B0],B1,E0,E1).

rmGlobal(Re,F0,F1,B0,B1,E0,E1) :-
  rmSomewhere(Re,F0,Fx,B0,Bx,E0,Ex),
  (rmGlobal(Re,Fx,F1,Bx,B1,Ex,E1), !; F1 = Fx, B1 = Bx, E1 = Ex).

%----------------------------------------------------------------------

replace_at_n(0,Replace,[],[Replace]) :- !.
replace_at_n(0,Replace,[_|T],[Replace|T]) :- !.
replace_at_n(N,Replace,[],[_H|T1]) :- !, N1 is N - 1, replace_at_n(N1,Replace,[],T1).
replace_at_n(N,Replace,[H|T],[H|T1]) :- !, N1 is N - 1, replace_at_n(N1,Replace,T,T1).
replace_at_n_if_undef(0,Replace,[],[Replace]) :- !.
replace_at_n_if_undef(0,Replace,[X|T],[Replace|T]) :- !, var(X).
replace_at_n_if_undef(N,Replace,[],[_H|T1]) :- !, N1 is N - 1, replace_at_n_if_undef(N1,Replace,[],T1).
replace_at_n_if_undef(N,Replace,[H|T],[H|T1]) :- !, N1 is N - 1, replace_at_n_if_undef(N1,Replace,T,T1).
ground_list([],_,[]).
ground_list([H0|T0],D,[H1|T1]) :-
  (var(H0), H1 = D ; ground_list(H0,D,H1) ; H1 = H0), !, ground_list(T0,D,T1).

initialE(amat([],flgs(-,-,-,-,-,-,-,-,-,-))).
match_captures(amat(C0,_G),C0).
match_captures(amat(_C0,G),C1,amat(C1,G)).
match_captures_nest(amat(_C0,G),amat([],G)).
match_captures_push(amat(C0,G),CL,amat(C1,G)) :- append(C0,CL,C1).
match_captures_set(N,X,amat(C0,G),amat(C1,G)) :- replace_at_n(N,X,C0,C1).
match_captures_set_if_undef(N,X,amat(C0,G),amat(C1,G)) :- replace_at_n_if_undef(N,X,C0,C1).
match_captures_get(N,amat(C0,_G),X) :- nth0(N,C0,X).

set_modifiers(E0,[Flag|M],E1) :- set_flag(E0,Flag,Ex), set_modifiers(Ex,M,E1).
set_modifiers(E0,[],E0).
set_flag(E0,i,E1)  :- set_flag_i(E0,i,E1), !.
set_flag(E0,-i,E1) :- set_flag_i(E0,-,E1), !.
set_flag(E0,m,E1)  :- set_flag_m(E0,m,E1), !.
set_flag(E0,-m,E1) :- set_flag_m(E0,-,E1), !.
set_flag(E0,s,E1)  :- set_flag_s(E0,s,E1), !.
set_flag(E0,-s,E1) :- set_flag_s(E0,-,E1), !.
set_flag(E0,x,E1)  :- set_flag_x(E0,x,E1), !.
set_flag(E0,-x,E1) :- set_flag_x(E0,-,E1), !.
set_flag(E0,_,E0).
set_modifiers_from(E0,Em,E1) :-
  E0 = amat(C0,_),
  Em = amat(_,flgs(A,B,C,D,E,F,G,H,I,J)),
  E1 = amat(C0,flgs(A,B,C,D,E,F,G,H,I,J)).
set_flag_i(amat(C0,flgs(_,B,C,D,E,F,G,H,I,J)),X,amat(C0,flgs(X,B,C,D,E,F,G,H,I,J))).
set_flag_m(amat(C0,flgs(A,_,C,D,E,F,G,H,I,J)),X,amat(C0,flgs(A,X,C,D,E,F,G,H,I,J))).
set_flag_s(amat(C0,flgs(A,B,_,D,E,F,G,H,I,J)),X,amat(C0,flgs(A,B,X,D,E,F,G,H,I,J))).
set_flag_x(amat(C0,flgs(A,B,C,_,E,F,G,H,I,J)),X,amat(C0,flgs(A,B,C,X,E,F,G,H,I,J))).
flag_i(amat(_,flgs(i,_,_,_,_,_,_,_,_,_))).
flag_m(amat(_,flgs(_,m,_,_,_,_,_,_,_,_))).
flag_s(amat(_,flgs(_,_,s,_,_,_,_,_,_,_))).
flag_x(amat(_,flgs(_,_,_,x,_,_,_,_,_,_))).

%----------------------------------------------------------------------

listify(X,L) :- atom(X), atom_codes(X,L), !.
listify(X,L) :- string_to_list(X,L), !.
listify(L,L).
listify_lists([X|Xs],[L|Ls]) :- listify(X,L), listify_lists(Xs,Ls).
listify_lists([],[]).

domatch(R,T,O) :-
  listify(R,RS), listify(T,F0),
  re_parse(RS,Re),
  initialE(E0), !,
  rmSomewhere(Re,F0,_F1,[],_B1,E0,E1),
  match_captures(E1,C1),
  ground_list(C1,'undef',O).

END
#======================================================================

use strict;
use warnings;
BEGIN {
    require Language::Prolog::Yaswi::Low;
    @Language::Prolog::Yaswi::Low::args = ($Language::Prolog::Yaswi::Low::args[0], @ARGV);
}
use Language::Prolog::Yaswi qw(:query :interactive :load :assert :context :run);
#$Language::Prolog::Yaswi::swi_converter->pass_as_opaque('UNIVERSAL');
#Predicates perl5_call/3, perl5_eval/2 and perl5_method/4 will be available

do{open F,">deleteme_pro.pl";print F $prolog;close F;} if 1;
swi_inline($prolog);

use Language::Prolog::Types::overload;
use Language::Prolog::Sugar
	    functors => [qw(foo tokenize domatch)],
	    vars => [qw (L X Y Z)];
#use Language::Prolog::Types qw(:ctors);
use Benchmark qw(:all);
require "m02.pl";
require "re05.pl";

sub mkMatch {
    my($len,$a)=@_;
    return MatchX->new()->set_as_failed() if !defined $a;
    my($strA,$npos,$A)=@$a;
    my $str = join("",map{chr($_)} @$strA);
    my $cap = [map{mkMatch($len,$_)}@$A];
    my $from = $len-$npos;
    my $to = $from + length($str);
    MatchX->new()->set(1,$str,$cap,{},$from,$to);
}
sub rx {
    my($re,$mods)=@_;
    my $rem = $re;  $rem = "(?$mods)(?:$rem)" if $mods;
    #print "re>$rem<\n";
    sub {
	my($s)=@_;
	#print "s>$s<\n";
	#use Data::Dumper; print Dumper($s);
	my $ret = swi_find_one(domatch($rem,$s,L),L);
	if ($ret) {
	    #print ">>$ret<<\n";
	    my $res = eval($ret); die "bug $@\n$ret" if $@;
	    #use Data::Dumper;
	    #print Dumper($res);
	    mkMatch(length($s),$res);
	}
	else { MatchX->new()->set_as_failed() }
    };
}
my $rx = \&rx;
$/=undef;
do {
    Pkg_re_tests::test(\&rx);
    exit;
  } if !@ARGV;

do {
print "----\n";

my $r = rx(' +|([0-9]+|\\+|-)');
print $r->('12 + 4 - 29'),"\n";
print $r->('x12 blah + 4 - 29'),"\n";
print $r->('12 + 4 - 29')->describe,"\n";
print $r->('xxx')->describe,"\n";
print $rx->('abc')->('abc')->describe,"\n";
print $rx->('(a+|b)*')->('ab')->describe,"\n";

print "----\n";
}if(1);

Language::Prolog::Yaswi::swi_toplevel;

__END__
% :- use_module(library(time)). call_with_time_limit,  but allegedly cant from perl. :(

# XXX - Known bugs
() isnt an unsuccessful capture.
(a\1) isnt handled properly
(a(b)?)* aba  doesnt have $2 as an unsuccessful match.
//x isnt being handled (re_rewrite step?)
lots of stuff with an XXX above
crlf handling unix specific
