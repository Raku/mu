#!/usr/bin/pugs

use v6;
use Test;

=pod

Control block tests

Most tests are still TODO. 
These are based on L<S04/"Closure traits">

=cut

plan 20;

# don't read this linearly, some tests are not in the order they're written it
# (or at least, should not be ;-)

# TODO, based on synopsis 4:
#
# * KEEP, UNDO, PRE, POST, CONTROL
#   CATCH is tested in t/base/try.t
#
# * $var will undo, etc
#
# * blocks appearing multiple times
#
# * semantics of FIRST vs INIT, in terms of closures 
#
# * LEAVE type blocks in the context of CATCH
#
# * PRE/POST in classes is not the same as LEAVE/ENTER


my $var = 1;
# defined in BEGIN
my $bvar = 1; # FIXME: this parses as a sub: my $bvar = BEGIN { 3 };
my $var_at_begin;
my $bvar_at_begin;
# defined in CHECK
my $cvar;
my $cvar_at_begin;
my $var_at_check;
# defined in INIT
my $ivar;
my $ivar_at_check;
my $var_at_init;
# defined in END
my $evar;
my $var_at_end;

# FIXME: CHECK {
{
	$cvar = 1;
	$var_at_check = $var;
	$ivar_at_check = $ivar;
};

# FIXME: BEGIN {
{
	$var_at_begin = $var;
	$bvar_at_begin = $bvar;
	$cvar_at_begin = $cvar;
};

# L<S04/"Closure traits" /BEGIN/>
is($var_at_begin, undef, '{ $var = 1 } not yet assigned when BEGIN block run', :todo(1));
# L<S04/"Closure traits" /can also be used within an expression/>
ok($bvar_at_begin, 'but { $bvar = BEGIN { 1 } } was');
# L<S04/"Closure traits" /CHECK/>
is($cvar_at_begin, undef, 'CHECK var not defined at BEGIN time', :todo(1));

# L<S04/"Closure traits" /INIT/>
ok($ivar, "INIT var defined at begining of runtime", :todo(1));
is($var_at_init, undef, 'INIT block ran before { $var = 1 }');

# FIXME: INIT {
{
	$var_at_init;
	$ivar = 1;
};

END {
	# L<S04/"Closure traits" /END/>
	ok($evar,  "END var was defined");
	ok($var_at_end, 'and also saw $var');
};

END {
	$evar = 1;
	$var_at_end = $var;
};

# L<S04/"Closure traits" /END/>
is($evar, undef, "END var was not defined yet");
is($var_at_end, undef, '$var was not yet seen by END');


my (@first, @enter, @leave, @last, @next) = ();

for (1 .. 3) -> $i {
	# FIXME: these don't parse yet 
	#LAST  { push @last, $i }
	#LEAVE { push @leve, [ $i, +@enter ] }
	#ENTER { push @enter, [ $i, +@leave ] }
	#FIRST { push @first, $i }
	#NEXT { push @next, $i }
	#next if $i % 2 == 1;
}

# L<S04/"Closure traits" /FIRST/>
is(+@first, 1, "FIRST ran once", :todo(1));
is(@first[0], 1, "only on 1", :todo(1));

# L<S04/"Closure traits" /LAST/>
is(+@last, 1, "LAST ran once", :todo(1));
is(@last[0], 1, "only on 3", :todo(1));

# L<S04/"Closure traits" /ENTER/>
is(+@enter, 3, "ENTER ran thrice", :todo(1));

# L<S04/"Closure traits" /LEAVE/>
is(+@leave, 3, "ENTER ran thrice", :todo(1));

is(@enter[0][1], 0, "enter and leave are in proper order", :todo(1));
is(@enter[2][1], 2, "...", :todo(1));
is(@leave[0][1], 1, "...", :todo(1));
is(@leave[2][1], 3, "...", :todo(1));

# L<S04/"Closure traits" /NEXT/>
is(+@next, 2, "NEXT ran twice, for each odd number in loop", :todo(1));

