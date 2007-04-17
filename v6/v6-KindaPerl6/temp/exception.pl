
use Benchmark qw(:all) ;
use strict;

cmpthese(1000000, {
    'die' => sub { 
        eval {
            die bless { answer => 42 }, 'Return';
        };
    },
    'return' => sub { 
        return bless { answer => 42 }, 'Return';
    },
    'end' => sub { 
        bless { answer => 42 }, 'Return';
    },
});

__END__
eval {
    my $r = bless { answer => 42 }, 'Return';
    die $r;
};
print $@;
