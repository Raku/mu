use v6;

use Test;

plan 29;

# Signature values and pretty-printing
# L<S06/"Parameters and arguments">
{
    # let's start with valid signatures whose canonical stringy form looks
    # just like their source. I incidentally use different sigils, can't
    # throw in the complete cartesian product here...
    my @sigs =
        ( ':($x)',                'single required positional'
        , ':($x:)',               'invocant only'
        , ':(@x, $y)',            'two required positionals'
        , ':($x, %y?)',           'required and optional positionals'
        , ':($x is rw is ref is lazy is moose)', # note order matters :/
                                  'traits (including user defined)'
        , ':($x, $y, :$z)',       'positional and named'
        , ':($x, $y?, :$z)',      'optional positional and named'
        , ':(:$x)',               'required named'
        , ':(:$x?)',              'optional named'
        , ':(:short($long))',     'long named'
        , ':(:short($long)?)',    'optional long named'
        , ':($ : %x)',            'dummy invocant'
        , ':($x :($y))',          'unpacking(1)'
        , ':($x :($y: $z))',      'unpacking(2)'
        , # add more here.
        # We parse these correctly but don't pretty print them correctly yet.
        , ':($x = 42)',           'positional with default'
        , ':(@x = (1, 2))',       'positional array with default'
        , ':(%x = (1 => 2))',     'positional hash with default'
        , ':(:$x = 42)',          'named with default'
        , ':(:@x = (1, 2))',      'named array with default'
        , ':(:%x = (1 => 2))',    'named hash with default'
        , ':(:x($y) = 42)',       'longnamed with default'
        , ':(:x(@y) = (1, 2))',   'longnamed array with default'
        , ':(:x(%y) = (1 => 2))', 'longnamed hash with default'
        );
    for @sigs -> $sig, $desc {
        is eval("my \$s = $sig; qq[\$s]"), $sig, "signature stringifies - $desc";
    }

    # ("" ~ :() is just an interim hack to dispatch into pretty-newval. will be removed.)
    # canonized version is different from source
    is eval('""~:($x!)'),          ':($x)',      'required positional with hint';
    is eval('""~:($x? = 42)'),     ':($x = 42)', 'positional with default and hint';
    is eval('""~:(@y? = (1, 2))'), ':(@y = (1, 2))',
                                                'named array with default and hint';

    is eval('""~:($x is rw is ro is rw is copy is ro is rw)'),
                                ':($x is rw)',  'last repeated trait wins'; # XXX: spec
    is eval('""~:($x is moose is ref is ro is lazy)'), # 'is ro' is default thus not printed
                                ':($x is ref is lazy is moose)',
                                'interleaved traits'; # XXX spec this minor point?


    # should die
    eval_dies_ok ':($x! = 42)', "required params can't have a default";
}
