use v6-alpha;
use Test;
plan 6;

# L<S29/Num/"Num provides a number of constants">

=head1 DESCRIPTION

Basic tests for builtin Num::pi

=cut

sub approx(Num $a, Num $b) {
  my $EPSILON = 0.0001;
  ($EPSILON > abs($a - $b));
}

# See also: L<"http://theory.cs.iitm.ernet.in/~arvindn/pi/"> :)
my $PI = 3.14159265358979323846264338327950288419716939937510;

ok(approx(eval("Num::pi "), $PI), 
                        "Num::pi");

ok(approx(eval("use Num :constants; pi"), $PI), 
                        "pi imported by use Num :constants");  

ok(approx(eval("use Num :constants; 3 + pi()"), $PI+3), "
                        3+pi(), as a sub");

ok(approx(eval("use Num :constants; pi() + 3"), $PI+3),
                        "pi()+3, as a sub");

ok(approx(eval("use Num :constants; 3 + pi"),   $PI+3), 
                        "3+pi, as a bareword");

ok(approx(eval("use Num :constants; pi + 3"),   $PI+3), 
                        "pi+3, as a bareword");
