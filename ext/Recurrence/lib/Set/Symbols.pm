
use v6;

role Set::Symbols;

=for LATER

# parsefail :(

sub ∅ {
    set();
}

=cut

# unicode intersection
multi sub infix:<∩> ($one, $two) is export {
    $one.intersection($two);
}

# unicode union
multi sub infix:<∪> ($one, $two) is export {
    $one.union($two);
}

# addition is union
multi sub infix:<+> ($one, $two) is export {
    $one.union($two);
}

# subtraction is difference
multi sub infix:<-> ($one, $two) is export {
    $one.difference($two);
}

# unicode difference operator
#  note the difference - ∖ vs \ (backslash)
multi sub infix:<∖> ($one, $two) is export {
    $one.difference($two);
}

# multiplication is intersection
multi sub infix:<*> ($one, $two) is export {
    $one.intersection($two);
}

# modulus is symmetric difference
multi sub infix:<%> ($one, $two) is export {
    $one.symmetric_difference($two);
}

# comparison is subset/superset
multi sub infix:<==> ($one, $two) is export {
    $one.equal($two);
}
multi sub infix:<!=> ($one, $two) is export {
    $one.not_equal($two);
}
multi sub infix:<≠> ($one, $two) is export {
    $one.not_equal($two);
}

# what will be used for stringify?
method prefix:<~> ($self) returns Str is export {
    ./stringify
}

# removed - spans can be numerically compared
# multi sub infix:«<» ($one, $two) is export {
#    $one.proper_subset($two);
#}
#multi sub infix:«>» ($one, $two) is export {
#    $one.proper_superset($two);
#}
#multi sub infix:«<=» ($one, $two) is export {
#    $one.subset($two);
#}
#multi sub infix:«>=» ($one, $two) is export {
#    $one.superset($two);
#}

# look at all these great unicode operators!  :D
multi sub infix:«⊂» ($one, $two) is export {
    $one.proper_subset($two);
}
multi sub infix:«⊃» ($one, $two) is export {
    $one.proper_superset($two);
}
multi sub infix:«⊆» ($one, $two) is export {
    $one.subset($two);
}
multi sub infix:«⊇» ($one, $two) is export {
    $one.superset($two);
}
multi sub infix:«⊄» ($one, $two) is export {
    !$one.proper_subset($two);
}
multi sub infix:«⊅» ($one, $two) is export {
    !$one.proper_superset($two);
}
multi sub infix:«⊈» ($one, $two) is export {
    !$one.subset($two);
}
multi sub infix:«⊉» ($one, $two) is export {
    !$one.superset($two);
}
multi sub infix:«⊊» ($one, $two) is export {
    $one.proper_subset($two);
}
multi sub infix:«⊋» ($one, $two) is export {
    $one.proper_superset($two);
}

# several unicode operators for includes!
multi sub infix:<∋> ($one, $member) returns Bool is export {
    $one.includes($member);
}
multi sub infix:<∈> ($member, $set) returns Bool is export {
    $set.includes($member);
}
multi sub infix:<∍> ($one, $member) returns Bool is export {
    $one.includes($member);
}
multi sub infix:<∊> ($member, $set) returns Bool is export {
    $set.includes($member);
}
multi sub infix:<∌> ($one, $member) returns Bool is export {
    !$one.includes($member);
}
multi sub infix:<∉> ($member, $set) returns Bool is export {
    !$set.includes($member);
}

# these methods are for overloaded operations with non-sets
multi sub infix:<+> ($one, *@args) is export {
    $one.union(@args);
}
multi sub infix:<-> ($one, *@args) is export {
    $one.difference(@args);
}
multi sub infix:<*> ($one, *@args) is export {
    $one.intersection(@args);
}
multi sub infix:<%> ($one, *@args) is export {
    $one.symmetric_difference(@args);
}
multi sub infix:<~~> ($one, $member) returns Bool is export {
    $one.includes($member);
}
# XXX -- IIRC, there's a "is commutative" or such, so duplicating shouldn't be
# necessary.
multi sub infix:<~~> ($member, $one) returns Bool is export {
    $one.includes($member);
}

# Subs to make operations on arrays
# E.g. [1,2,3] +# [2,5]  ==>  [1,2,3,5]
# (Similar to Ruby)
## multi sub infix:<+#> (@a, @b) returns Array is export { set(@a).union(@b).members }
## multi sub infix:<-#> (@a, @b) returns Array is export { set(@a).difference(@b).members }
## multi sub infix:<*#> (@a, @b) returns Array is export { set(@a).intersection(@b).members }
## multi sub infix:<%#> (@a, $b) returns Array is export { set(@a).symmetric_difference(@b).members }

=head1 NAME

Set::Symbols - A Role of unicode symbols for "set" operations

=head1 AUTHORS

Organized by Flavio S. Glock; the unicode methods were extracted from
Set.pm, written by Sam "mugwump" Vilain

=cut
