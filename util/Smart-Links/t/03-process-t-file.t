use strict;
use warnings;

use Test::More;
my $tests;

use Test::NoWarnings;
BEGIN { $tests++ }

plan tests => $tests;

use FindBin;

use Text::SmartLinks;

# L<Text::SmartLinks/process_t_file>

{
    my $sl = Text::SmartLinks->new;
    my $file = 'eg/a/t/01.t';
    $sl->process_t_file($file);
    is($sl->link_count, 1, 'link_count');
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
        }, "linktree of $file";
    #diag explain $sl;
    BEGIN { $tests += 5 }
}


# The following was not recognized even though
# t/spec/S05-metasyntax/assertions.t has this
# Now one of them is recognized the other gets 
# reported as error
{
    my $sl = Text::SmartLinks->new;
    my $file = 'eg/a/t/assertions.t';
    $sl->process_t_file($file);
    is $sl->link_count, 1, "link_count 1 for $file";
    is $sl->{invalid_link}, 2, 'invalid_link';
    is $sl->broken_link_count, 0, 'broken_link_count';
    #is_deeply $sl->{errors}, [], 'errors';
    like $sl->{errors}[0][0], qr/Legacy smartlink. Use L< instead of L<< in line 12/, 'errors';
    is_deeply $sl->{linktree}, {
     'S07' => {
       'Other Extensible metasyntax (C<< <...> >>)' => [
         [
           '"A leading C<?{> or C<!{> indicates a code:"',
           [
             'eg/a/t/assertions.t',
             '19',
             '27'
           ]
         ]
       ]
    }
    }, "linktree of $file";

#    diag explain $sl;
    BEGIN { $tests += 5 }
}

{
    my $sl = Text::SmartLinks->new;
    my $file = 'eg/a/t/undef.t';
    $sl->process_t_file($file);
    is $sl->link_count, 6, "link_count is 6 for $file";
    is $sl->broken_link_count, 0, 'broken_link_count';
    is $sl->{invalid_link}, 1, 'invalid link';
    like $sl->{errors}[0][0], qr/Legacy smartlink. Use L< instead of L<< in line 70/, 'errors';
    #diag explain $sl->{linktree};
    is_deeply $sl->{linktree}, {
         'S05' => {
       'Match objects' => [
         [
           '"they will all be undefined" closure "let keyword"',
           [
             'eg/a/t/undef.t',
             50,
             72
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
     'S19' => {
       'Something else' => [
         [
           undef,
           [
             'eg/a/t/undef.t',
             '98',
             '100'
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
    }, "linktree of $file";
    BEGIN { $tests += 5 }

    #diag explain $sl;
}

{
    my $sl = Text::SmartLinks->new;
    my $file = 'eg/a/t/many.t';
    $sl->process_t_file($file);
    is $sl->link_count, 7, "link_count is 7 for $file";
    is $sl->broken_link_count, 0, 'broken_link_count';
    is $sl->{invalid_link}, 1, 'invalid link';
    #diag explain $sl->{errors};
    like $sl->{errors}[0][0], qr/Legacy smartlink. Use L< instead of L<< in line 53/, 'errors';
#    diag explain $sl->{linktree};
    is_deeply $sl->{linktree}, {
   'Module' => {
     'Autoloading' => [
       [
         undef,
         [
           'eg/a/t/many.t',
           '66',
           70
         ]
       ]
     ],
     'Something else' => [
       [
         undef,
         [
           'eg/a/t/many.t',
           '80',
           '82'
         ]
       ]
     ]
   },
   'Module::Name' => {
     'Autoloading' => [
       [
         undef,
         [
           'eg/a/t/many.t',
           '71',
           79
         ]
       ]
     ]
   },
   'S05' => {
     'Match objects' => [
       [
         '"they will all be undefined" closure "let keyword"',
         [
           'eg/a/t/many.t',
           35,
           56
         ]
       ]
     ]
   },
   'S06' => {
     'Named parameters' => [
       [
         'Named parameters are optional',
         [
           'eg/a/t/many.t',
           '57',
           65
         ]
       ]
     ]
   },
   'S29' => {
     'Scalar' => [
       [
         '"=item undef"',
         [
           'eg/a/t/many.t',
           '11',
           24
         ]
       ]
     ]
   },
   'S32::Abstraction' => {
     'Numbers' => [
       [
         '"=item available"',
         [
           'eg/a/t/many.t',
           '25',
           34
         ]
       ]
     ]
   }
   }, "linktree of $file";

    BEGIN { $tests += 5 }
}
