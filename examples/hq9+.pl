class HQ9Plus;

# 2006-12-19 (rough) update to current spec

my subset HQ9PlusProgram
    of Str
    where /^ <[hq9+]>* $/;

my subset HQ9PlusStep
    of HQ9PlusProgram
    where { .chars == 1 };

has HQ9PlusProgram $.program;
has Int            $.accumulator = 0;
has Int            $position = 0; # twigilless are private

has %actions = (
    'h' => { self.hello },
    'q' => { self.quine },
    '9' => { self.nine  },
    '+' => { self.plus  },
);

method run () {
    # Java, anyone? Feel free to fix this.
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

method step () {
    given $.program.substr($position++, 1) {
        $actions<$_>();
    }
}

my method hello () { say "Hello, world!" }
my method quine () { say $.program }
my method plus  () { $.accumulator++ }
my method nine  () {
    my Int $i = 99;

    while $i {
        say qq:to/END/;
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
