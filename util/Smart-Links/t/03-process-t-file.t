use strict;
use warnings;

use Test::More;
my $tests;

use Test::NoWarnings;
BEGIN { $tests++ }

plan tests => $tests;

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
	BEGIN { $tests += 5 }
}


# The following was not recognized even though
# t/spec/S05-metasyntax/assertions.t has this
# Now one of them is recognized the other gets 
# reported as error
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
	BEGIN { $tests += 5 }
}

{
    my $sl = Smart::Links->new;
    $sl->process_t_file('eg/a/t/undef.t');
    is $sl->link_count, 6, 'link_count';
    is $sl->broken_link_count, 0, 'broken_link_count';
    is_deeply $sl->{errors}, [];
    is_deeply $sl->{linktree}, {
         'S05' => {
       'Match objects' => [
         [
           '"they will all be undefined" closure "let keyword"',
           [
             'eg/a/t/undef.t',
             50,
             69
           ]
         ]
       ]
     },
     'S06' => {
       'Named parameters' => [
         [
           'Named parameters are optional',
           [
             'eg/a/t/undef.t',
             '73',
             81
           ]
         ]
       ],
       'Optional parameters' => [
         [
           'Missing optional arguments',
           [
             'eg/a/t/undef.t',
             '70',
             72
           ]
         ]
       ]
     },
     'S10' => {
       'Autoloading' => [
         [
           undef,
           [
             'eg/a/t/undef.t',
             '82',
             '97'
           ]
         ]
       ]
     },
     'S29' => {
       'Scalar' => [
         [
           '"=item undef"',
           [
             'eg/a/t/undef.t',
             '21',
             31
           ]
         ],
         [
           '"=item undefine"',
           [
             'eg/a/t/undef.t',
             '32',
             49
           ]
         ]
       ]
     }
    }, 'linktree';
	BEGIN { $tests += 4 }

    #diag explain $sl;
}


