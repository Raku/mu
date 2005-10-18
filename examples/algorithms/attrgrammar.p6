#!/usr/bin/perl

# This is just a hand-written lazy attribute grammar to help
# me decide if I want to port L::AG to Perl 6 (i.e. whether
# pugs is going to give me too much bullshit like it did
# last time I tried to port an object-oriented module).
# So far, it looks pretty good.

class Thunk {
    method call () {
        my $thunk = $.code;
        if $thunk {
            undefine $.code;
            return $.value = $thunk();
        }
        else {
            return $.value;
        }
    }
    
    has $.code;
    has $.value;
}

sub *thunk(&code) {
    return Thunk.new(code => &code);
}

class Leaf {
    method visit ($parent) {
        my $compute = {
            min     => thunk { $.value },
            result  => thunk { Leaf.new(value => $compute<gmin>.call) },
            gmin    => thunk { $parent<gmin>.call },
        };
        return $compute;
    }

    has $.value;
}

class Branch {
    method visit ($parent) {
        my ($lvis, $rvis);
        my $compute = {
            min     => thunk { min($lvis<min>.call, $rvis<min>.call) },
            result  => thunk { Branch.new(left => $lvis<result>.call, right => $rvis<result>.call) },
            gmin    => thunk { $parent<gmin>.call },
        };
        ($lvis, $rvis) = ($.left.visit($compute), $.right.visit($compute));
        return $compute;
    }

    has $.left;
    has $.right;
}

class Root {
    method visit ($parent) {
        my $tvis;
        my $compute = {
            result => thunk { $tvis<result>.call },
            gmin   => thunk { $tvis<min>.call },
        };
        $tvis = $.tree.visit($compute);
        $compute;
    }

    has $.tree;
}

my $tree = Root.new(
    tree => Branch.new(
        left  => Leaf.new(value => 1),
        right => Branch.new(
            left  => Branch.new(
                left  => Leaf.new(value => 2),
                right => Leaf.new(value => 3),
            ),
            right => Leaf.new(value => -3),
        ),
    ),
);

say $tree.perl;
say $tree.visit( {} ).<result>.call.perl;
