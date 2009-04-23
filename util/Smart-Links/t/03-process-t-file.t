use strict;
use warnings;

use Test::More tests => 4 + 1;
use Test::NoWarnings;

use FindBin;

use Smart::Links;

{
    my $sl = Smart::Links->new;
    $sl->process_t_file('eg/a/t/01.t');
    is $sl->link_count, 1;
    is $sl->broken_link_count, 0;

    is_deeply $sl->{errors}, [];
    is_deeply $sl->{linktree}, {
        'S02' => {
            'Mutable types' => [
            [
                'Array',
                [
                    'eg/a/t/01.t',
                    '7',
                    '12'
                ]
            ]
            ]
        }
        }, 'linktree';
}
#diag explain $sl;
