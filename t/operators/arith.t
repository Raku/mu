#!/usr/bin/pugs

use v6;
require Test;

plan(132);

my $five = abs(-5);

unless ($five == 5) {
    say "Bail out!";
    say "Unreliable abs()";
    exit();
}

sub try ($ok, ?$todo = '') { 
    if ($todo) { 
        &todo_ok.goto($ok,$todo); 
    } else {
        &ok.goto($ok);
    }
}
sub tryeq ($lhs, $rhs, ?$todo = '') {
    if ($todo) {
        &todo_ok.goto($lhs == $rhs,$todo ~ " " ~ $lhs ~ " != " ~ $rhs);
    } else {
        &ok.goto($lhs == $rhs);
    }
}
sub tryeq_sloppy ($lhs, $rhs, ?$todo1 = '') {
    my $todo = $todo1;  # TODO is rw
    $todo = ' # TODO ' ~ $todo if $todo;
    if ($lhs == $rhs) {
        if ($todo) { 
            &todo_ok.goto($lhs==$rhs,$todo);
        } else {
            &ok.goto($lhs==$rhs,$todo);
        }
    } else {
        my $error = abs($lhs - $rhs) / $lhs;
        if ($todo) {
            &todo_ok.goto($error<1e-9,$todo ~ " # " ~ $lhs ~ " is close to " ~ $rhs);
        } else {
            &ok.goto($error<1e-9);
        }
    }
}

tryeq  13 %  4, 1;
tryeq -13 %  4, 3;
tryeq  13 % -4, -3;
tryeq -13 % -4, -1;

my $limit = 1e6;

try abs( 13e21 %  4e21 -  1e21) < $limit;
try abs(-13e21 %  4e21 -  3e21) < $limit;
try abs( 13e21 % -4e21 - -3e21) < $limit;
try abs(-13e21 % -4e21 - -1e21) < $limit;

# UVs, IVs, etc make no sense but the tests are useful anyhow.

# UVs should behave properly

tryeq 4063328477 % 65535, 27407;
tryeq 4063328477 % 4063328476, 1;
tryeq 4063328477 % 2031664238, 1;
tryeq 2031664238 % 4063328477, 2031664238;

# These should trigger wrapping on 32 bit IVs and UVs

tryeq 2147483647 + 0, 2147483647;

# IV + IV promote to UV
tryeq 2147483647 + 1, 2147483648;
tryeq 2147483640 + 10, 2147483650;
tryeq 2147483647 + 2147483647, 4294967294;
# IV + UV promote to NV
tryeq 2147483647 + 2147483649, 4294967296;
# UV + IV promote to NV
tryeq 4294967294 + 2, 4294967296;
# UV + UV promote to NV
tryeq 4294967295 + 4294967295, 8589934590;

# UV + IV to IV
tryeq 2147483648 + -1, 2147483647;
tryeq 2147483650 + -10, 2147483640;
# IV + UV to IV
tryeq -1 + 2147483648, 2147483647;
tryeq -10 + 4294967294, 4294967284;
# IV + IV to NV
tryeq -2147483648 + -2147483648, -4294967296;
tryeq -2147483640 + -10, -2147483650;

# Hmm. Don t forget the simple stuff
tryeq 1 + 1, 2;
tryeq 4 + -2, 2;
tryeq -10 + 100, 90;
tryeq -7 + -9, -16;
tryeq -63 + +2, -61;
tryeq 4 + -1, 3;
tryeq -1 + 1, 0;
tryeq +29 + -29, 0;
tryeq -1 + 4, 3;
tryeq +4 + -17, -13;

# subtraction
tryeq 3 - 1, 2;
tryeq 3 - 15, -12;
tryeq 3 - -7, 10;
tryeq -156 - 5, -161;
tryeq -156 - -5, -151;
tryeq -5 - -12, 7;
tryeq -3 - -3, 0;
tryeq 15 - 15, 0;

tryeq 2147483647 - 0, 2147483647;
tryeq 2147483648 - 0, 2147483648;
tryeq -2147483648 - 0, -2147483648;

tryeq 0 - -2147483647, 2147483647;
tryeq -1 - -2147483648, 2147483647;
tryeq 2 - -2147483648, 2147483650;

tryeq 4294967294 - 3, 4294967291;
tryeq -2147483648 - -1, -2147483647;

# IV - IV promote to UV
tryeq 2147483647 - -1, 2147483648;
tryeq 2147483647 - -2147483648, 4294967295;
# UV - IV promote to NV
tryeq 4294967294 - -3, 4294967297;
# IV - IV promote to NV
tryeq -2147483648 - +1, -2147483649;
# UV - UV promote to IV
tryeq 2147483648 - 2147483650, -2;
# IV - UV promote to IV
tryeq 2000000000 - 4000000000, -2000000000;

# No warnings should appear;
my $a;
$a += 1;
tryeq $a, 1;
undef $a;
$a += -1;
tryeq $a, -1;
undef $a;
$a += 4294967290;
tryeq $a, 4294967290;
undef $a;
$a += -4294967290;
tryeq $a, -4294967290;
undef $a;
$a += 4294967297;
tryeq $a, 4294967297;
undef $a;
$a += -4294967297;
tryeq $a, -4294967297;

my $s;
$s -= 1;
tryeq $s, -1;
undef $s;
$s -= -1;
tryeq $s, +1;
undef $s;
$s -= -4294967290;
tryeq $s, +4294967290;
undef $s;
$s -= 4294967290;
tryeq $s, -4294967290;
undef $s;
$s -= 4294967297;
tryeq $s, -4294967297;
undef $s;
$s -= -4294967297;
tryeq $s, +4294967297;

# Multiplication

tryeq 1 * 3, 3;
tryeq -2 * 3, -6;
tryeq 3 * -3, -9;
tryeq -4 * -3, 12;

# check with 0xFFFF and 0xFFFF
tryeq 65535 * 65535, 4294836225;
tryeq 65535 * -65535, -4294836225;
tryeq -65535 * 65535, -4294836225;
tryeq -65535 * -65535, 4294836225;

# check with 0xFFFF and 0x10001
tryeq 65535 * 65537, 4294967295;
tryeq 65535 * -65537, -4294967295;
tryeq -65535 * 65537, -4294967295;
tryeq -65535 * -65537, 4294967295;

# check with 0x10001 and 0xFFFF
tryeq 65537 * 65535, 4294967295;
tryeq 65537 * -65535, -4294967295;
tryeq -65537 * 65535, -4294967295;
tryeq -65537 * -65535, 4294967295;

# These should all be dones as NVs
tryeq 65537 * 65537, 4295098369;
tryeq 65537 * -65537, -4295098369;
tryeq -65537 * 65537, -4295098369;
tryeq -65537 * -65537, 4295098369;

# will overflow an IV (in 32-bit)
tryeq 46340 * 46342, 0x80001218;
tryeq 46340 * -46342, -0x80001218;
tryeq -46340 * 46342, -0x80001218;
tryeq -46340 * -46342, 0x80001218;

tryeq 46342 * 46340, 0x80001218;
tryeq 46342 * -46340, -0x80001218;
tryeq -46342 * 46340, -0x80001218;
tryeq -46342 * -46340, 0x80001218;

# will overflow a positive IV (in 32-bit)
tryeq 65536 * 32768, 0x80000000;
tryeq 65536 * -32768, -0x80000000;
tryeq -65536 * 32768, -0x80000000;
tryeq -65536 * -32768, 0x80000000;

tryeq 32768 * 65536, 0x80000000;
tryeq 32768 * -65536, -0x80000000;
tryeq -32768 * 65536, -0x80000000;
tryeq -32768 * -65536, 0x80000000;

# 2147483647 is prime. bah.

tryeq 46339 * 46341, 0x7ffea80f;
tryeq 46339 * -46341, -0x7ffea80f;
tryeq -46339 * 46341, -0x7ffea80f;
tryeq -46339 * -46341, 0x7ffea80f;

# leading space should be ignored

tryeq 1 + " 1", 2;
tryeq 3 + " -1", 2;
tryeq 1.2, " 1.2";
tryeq -1.2, " -1.2";

# divide

tryeq 28/14, 2;
tryeq 28/-7, -4;
tryeq -28/4, -7;
tryeq -28/-2, 14;

tryeq 0x80000000/1, 0x80000000;
tryeq 0x80000000/-1, -0x80000000;
tryeq -0x80000000/1, -0x80000000;
tryeq -0x80000000/-1, 0x80000000;

# The example for sloppy divide, rigged to avoid the peephole optimiser.
tryeq_sloppy "20." / "5.", 4;

tryeq 2.5 / 2, 1.25;
tryeq 3.5 / -2, -1.75;
tryeq -4.5 / 2, -2.25;
tryeq -5.5 / -2, 2.75;

# Bluuurg if your floating point can't accurately cope with powers of 2
# [I suspect this is parsing string-to-float problems, not actual arith]
tryeq_sloppy 18446744073709551616/1, 18446744073709551616; # Bluuurg
tryeq_sloppy 18446744073709551616/2, 9223372036854775808;
tryeq_sloppy 18446744073709551616/4294967296, 4294967296;
tryeq_sloppy 18446744073709551616/9223372036854775808, 2;

{
  # The peephole optimiser is wrong to think that it can substitute intops
  # in place of regular ops, because i_multiply can overflow.
  # (Perl 5) Bug reported by "Sisyphus" (kalinabears@hdc.com.au)
  my $n = 1127;
  my $float = ($n % 1000) * 167772160.0;
  tryeq_sloppy $float, 21307064320;

  # On a 32 bit machine, if the i_multiply op is used, you will probably get
  # -167772160. It's actually undefined behaviour, so anything may happen.
  my $int = ($n % 1000) * 167772160;
  tryeq $int, 21307064320;

}

                             

