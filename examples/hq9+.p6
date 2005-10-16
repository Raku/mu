class HQ9Plus;

my subtype HQ9PlusProgram
    where /^<[hq9+]>$/;

my subtype HQ9PlusStep
    of HQ9PlusProgram
    where { .chars == 1 };

has HQ9PlusProgram $.program;
has Int            $.accumulator = 0;
has Int            $:position = 0;

has %:actions = (
    'h' => { self._hello },
    'q' => { self._quine },
    '9' => { self._nine  },
    '+' => { self._plus  },
);

method run    () {
    loop {
        .step;
        CATCH { 
            when Error::OutOfBounds { 
                return; # end of program
            } 
            default { fail }
        }
    }
}

method step   () {
    given $.program.substr($:position++, 1) {
        $:actions<$_>();
    }
}

# the current "spec" is that methods starting with _ are private.
# it used to be : instead of _. who knows what it'll be next week. (:
method _hello () { print "Hello, world!" }
method _quine () { print $.program }
method _plus  () { $.accumulator++ }
method _nine  () {
    my int $i = 99;

    while $i {
        say qq:to/END/
            $i bottles of beer on the wall
            $i bottles of beer!
            Take one down, pass it around
            END
        $i--;
        say "$i bottles of beer on the wall!"
    }
}

=head1 NAME

HQ9Plus - A HQ9+ implementation

=head1 SYNOPSIS

    HQ9Plus.new(program => $foo).run;

    my $program = HQ9Plus.new(program => $foo);

    while ... {
        $program.step;
        ...inspect the program state...
    }

=head1 DESCRIPTION

C<HQ9Plus> implements a I<HQ9+> interpreter with stepping.

=head1 SEE ALSO

http://en.wikipedia.org/wiki/HQ9+

=head1 AUTHOR

Ilmari Vacklin <ilmari.vacklin@helsinki.fi>

=head1 LICENSE

Public domain.
