use v6-alpha;

use Test;

plan 6;

# L<S29/Container/"=item each">

=pod

Tests of

  our Lazy multi Container::each( Bool :$shortest, Bool :$finite, *@@list );

=cut

my $array = [1..6];
my %hash  = 'a'..'f' >>=><< 1..6;

# basic

my @ans01 = gather {
    for each( 1..6 ) -> $v {
        take($v);
    }
}

ok(@ans01.fmt('%s',':') eq '1:2:3:4:5:6', 'each over a list');

my @ans02 = gather {
    for each($array) -> $v {
        take($v);
    }
}

ok(@ans02.fmt('%s',':') eq '1:2:3:4:5:6', 'each over an array');

my @ans03 = gather {
    for each( %hash ) -> $v {
        take(~$v);
    }
}

ok(@ans03.fmt('%s',':') eq 'a 1:b 2:c 3:d 4:e 5:f 6', 'each over a hash');

# slice

my @ans04 = gather {
    for each(1..9; $array) -> $l, $a is copy {
        $a //= '.';
        take($l~$a);
    }
}

ok(@ans04.fmt('%s', ':') eq '11:22:33:44:55:66:7.:8.:9.',
    'each over a slice');

# :shortest

eval_ok('each(:shortest, 1..9; [1..6]', 'parse of each(:shortest, ...)',
   :todo<feature>);

# :finite

eval_ok('each(:finite, 1..Inf; [1..6]', 'parse of each(:finite, ...)',
   :todo<feature>);
