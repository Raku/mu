#!/usr/bin/pugs
#
# $Id: lambda-calculus.p6,v 0.1 2005/09/05 12:16:18 dankogai Exp dankogai $
#
# Please remember to update t/examples/examples.t and rename
# examples/output/algorithms/lambda-calculus if you rename/move this file.

use v6;

=for reference

Message-Id: <A77477E9-C139-48D2-86C2-CE60842FA659@dan.co.jp>
    http://www.nntp.perl.org/group/perl.perl6.language/22991

Message-Id: <20050905061111.GA25085@aut.dyndns.org>
    http://www.nntp.perl.org/group/perl.perl6.language/22992

=cut

# Shmuck.  This doesn't work yet
#our $VERSION = sprintf "%d.%02d", q$Revision: 0.1 $ =~ /(\d+)/g;
our $VERSION = '0.01';

# macro lambda { 'sub' } 

our $ZERO = sub($f){ sub($x){ $x }};
our $SUCC = sub($n){ sub($f){ sub($x){ $f.($n.($f)($x)) }}};
our $ADD  = sub($m){ sub($n){ sub($f){ sub($x){ $m.($f)($n.($f)($x)) }}}};
our $MULT = sub($m){ sub($n){ sub($f){ $m.($n.($f)) }}};
our $POW  = sub($m){ sub($n){ $n.($m) }};

=for alternative

our $ZERO = -> $f { -> $x { $x } };
our $SUCC = -> $n { -> $f { -> $x { $f.($n.($f)($x)) }}};
# ...

=cut

sub num2int($n){ $n.(sub($i){ 1 + $i })(0) }
sub num2str($n){ 'sub($f){ sub($x) { '
                     ~ $n.( sub($s){sprintf '$f.(%s)', $s } )('$x') 
                         ~ ' }}' }
sub int2num($n){ return $n == 0 ?? $ZERO :: $SUCC.(int2num($n - 1)) }

=for alternative

our $ADD  =  sub($m){ sub($n){ $n.($SUCC)($m) }};
our $MULT =  sub($m){ sub($n){ $n.($ADD.($m))($ZERO) }};
our $POW  =  sub($m){ sub($n){ $n.($MULT.($m))($SUCC.($ZERO)) }};

=cut

my $one     = $SUCC.($ZERO);
my $two     = $SUCC.($one);
my $four    = $ADD.($two)($two);
my $eight   = $MULT.($two)($four);
my $sixteen = $POW.($four)($two);
for($one, $two, $four, $eight, $sixteen) -> $n {
    $n.(sub($i){ 1 + $i})(0).say
};

