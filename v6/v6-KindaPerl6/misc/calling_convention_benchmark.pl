use Benchmark qw(:all);
use strict;
my $sum;
$sum = sub {
    if ($_[0] == 1) {
        return 1;
    } else {
        return $sum->($_[0]-1) + $_[0];
    }
};
our $sum_our;
$sum_our = sub {
    if ($_[0] == 1) {
        return 1;
    } else {
        return $sum_our->($_[0]-1) + $_[0];
    }
};
sub sum {
    if ($_[0] == 1) {
        return 1;
    } else {
        return sum($_[0]-1) + $_[0];
    }
}
use Data::Bind;
Data::Bind->sub_signature(\&sum_Data_Bind,{ var => '$n' });
sub sum_Data_Bind {
    my $n;
    Data::Bind->arg_bind(\@_);
    if ($n == 1) {
        return 1;
    } else {
        return sum_Data_Bind([\($n-1)]) + $n;
    }
}
sub sum2 {
    if (${$_[0][0]} == 1) {
        return 1;
    } else {
        return sum2([\(${$_[0][0]}-1)]) + ${$_[0][0]};
    }
}
cmpthese(-1,{
        'my $ref'=>sub {$sum->(1000)},
        'our $ref'=>sub {$sum_our->(1000)},
        'our &code'=>sub {sum(1000)},
        'Data::Bind'=>sub {sum_Data_Bind([\1000])},
        'Data::Bind style'=>sub {sum2([\1000])},
});

