use strict;
use warnings;

use Test::More tests => 5 + 5 + 1;
use Test::NoWarnings;

use FindBin;

use Smart::Links;

{
    my $sl = Smart::Links->new;
    $sl->process_t_file('eg/a/t/01.t');
    is $sl->link_count, 1, 'link_count';
    is $sl->broken_link_count, 0, 'broken_link_count';
    is $sl->{invalid_link}, 0, 'invalid link';
    
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
    #diag explain $sl;
}

#{
#    my $sl = Smart::Links->new;
#    $sl->process_t_file('eg/a/t/undef.t');
#    is $sl->link_count, 8;
#    is $sl->broken_link_count, 0;
#    is_deeply $sl->{errors}, [];
##    is_deeply $sl->{linktree}, {
##    }, 'linktree';
#
#    diag explain $sl;
#}
#

# The following is not recognized even though
# t/spec/S05-metasyntax/assertions.t has this
{
    my $sl = Smart::Links->new;
    $sl->process_t_file('eg/a/t/assertions.t');
    is $sl->link_count, 1, 'link_count';
    is $sl->{invalid_link}, 1, 'invalid_link';
    is $sl->broken_link_count, 0, 'broken_link_count';
    #is_deeply $sl->{errors}, [], 'errors';
    like $sl->{errors}[0][0], qr/Could not parse smartlink/, 'errors';
    is_deeply $sl->{linktree}, {
     'S07' => {
       'Other Extensible metasyntax (C<< <...> >>)' => [
         [
           '"A leading C<?{> or C<!{> indicates a code:"',
           [
             'eg/a/t/assertions.t',
             '19',
             '22'
           ]
         ]
       ]
    }
    }, 'linktree';

#    diag explain $sl;
}
