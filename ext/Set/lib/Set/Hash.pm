
use v6;

class Set::Hash;

sub set (*@contents) returns Set::Hash is export {
    my $set = Set::Hash.new;
    $set.insert(@contents);
    return $set;
}

=for LATER

# parsefail :(

sub ∅ returns Set is export {
    set();
}

=cut

# the Set is represented as a hash of (v => v)
has %:members;

method members() returns List {
    # XXX - this is nessecary because the hash 
    # does not seem to get initialized properly
    # and calling members before any values are
    # set results in a error
    %:members ?? %:members.values !! ();
}

# NOTE:
# this is a hack to make this work well
# with objects. Eventually it should be 
# removed (IIRC hash keys are not supposed
# to stringify).
method _stringify ($item) returns Str {
    my $str_item = ~$item;
    $str_item ~= $item.id()
        if $str_item ~~ rx:perl5/^\<obj\:/; #/#cperl-mode--
    return $str_item;
}

method insert($self: *@items) returns Int {
    my $pre_size = $self.size;

    for @items -> $x { %:members{$self._stringify($x)} = $x }

    return ($self.size - $pre_size);
}

method remove($self: *@items) returns Int {
    my Int $pre_size = $self.size;
    for @items -> $x {
        %:members.delete($self._stringify($x));
    }
    return $pre_size - $self.size;
}

method includes($self: *@items) returns Bool {
    ?(%:members.exists(all(@items.map:{ $self._stringify($_) })));
}

method member($self: $item) returns Object {
    return %:members{$self._stringify($item)}
}

method size() returns int {
    +%:members.keys;
}

method invert($self: *@items) returns int {
    my int $rv;
    for @items -> $item {
        if ( $self.includes($item) ) {
            $self.remove($item);
            $rv++;
        } else {
            $self.insert($item);
        }
    }
    return $rv;
}

method clear() {
    undefine %:members;
}

method clone ($self:) returns Set::Hash {
    my $set = Set::Hash.new;
    $set.insert($self.members);
    return $set;
}

method equal($self: Set $other) returns Bool {
    return (($self.size == $other.size) &&
            ($self.includes($other.members)));
}

method not_equal($self: Set $other) returns Bool {
    return !$self.equal($other);
}

method subset($self: Set $other) returns Bool {
    return ($self.size <= $other.size && $other.includes($self.members));
}
method proper_subset($self: Set $other) returns Bool {
    return ($self.size < $other.size && $other.includes($self.members));
}
method superset($self: Set $other) returns Bool {
    return ($other.subset($self));
}
method proper_superset($self: Set $other) returns Bool {
    return ($other.proper_subset($self));
}

method stringify() returns Str {
    return("set(" ~ $?SELF.members.join(" ") ~ ")");
}


method union($self: Set $other) returns Set {
    set($self.members, $other.members);
}
method intersection($self: Set $other) returns Set {
    set($self.members.grep:{ $other.includes($_) });
}
method difference($self: Set $other) returns Set {
    set($self.members.grep:{ !$other.includes($_) });
}

method symmetric_difference($self: Set $other) returns Set {
    $self.difference($other).union($other.difference($self));
}

our &Set::Hash::count ::= &Set::Hash::size;
our &Set::Hash::has   ::= &Set::Hash::includes;

# unicode intersection
multi sub *infix:<∩> (Set $one, Set $two) returns Set {
    $one.intersection($two);
}

# unicode union
multi sub *infix:<∪> (Set $one, Set $two) returns Set {
    $one.union($two);
}

# addition is union
multi sub *infix:<+> (Set $one, Set $two) returns Set {
    $one.union($two);
}

# subtraction is difference
#method *infix:<-> (Set $one, Set $two) returns Set {
#    $one.difference($two);
#}

# unicode set difference operator
#  note the difference - ∖ vs \ (backslash)
multi sub *infix:<∖> (Set $one, Set $two) returns Set {
    $one.difference($two);
}

# multiplication is intersection
multi sub *infix:<*> (Set $one, Set $two) returns Set {
    $one.intersection($two);
}

# modulus is symmetric difference
multi sub *infix:<%> (Set $one, Set $two) returns Set {
    $one.symmetric_difference($two);
}

# comparison is subset/superset
multi sub *infix:<==> (Set $one, Set $two) returns Set {
    $one.equal($two);
}
multi sub *infix:<!=> (Set $one, Set $two) returns Set {
    $one.not_equal($two);
}
multi sub *infix:<≠> (Set $one, Set $two) returns Set {
    $one.not_equal($two);
}

# what will be used for stringify?
#method prefix:<~> (Set $self) returns Str {
#    ./stringify
#}

multi sub *infix:«<» (Set $one, Set $two) returns Set {
    $one.proper_subset($two);
}
multi sub *infix:«>» (Set $one, Set $two) returns Set {
    $one.proper_superset($two);
}
multi sub *infix:«<=» (Set $one, Set $two) returns Set {
    $one.subset($two);
}
multi sub *infix:«>=» (Set $one, Set $two) returns Set {
    $one.superset($two);
}

# look at all these great unicode operators!  :D
multi sub *infix:«⊂» (Set $one, Set $two) returns Set {
    $one.proper_subset($two);
}
multi sub *infix:«⊃» (Set $one, Set $two) returns Set {
    $one.proper_superset($two);
}
multi sub *infix:«⊆» (Set $one, Set $two) returns Set {
    $one.subset($two);
}
multi sub *infix:«⊇» (Set $one, Set $two) returns Set {
    $one.superset($two);
}
multi sub *infix:«⊄» (Set $one, Set $two) returns Set {
    !$one.proper_subset($two);
}
multi sub *infix:«⊅» (Set $one, Set $two) returns Set {
    !$one.proper_superset($two);
}
multi sub *infix:«⊈» (Set $one, Set $two) returns Set {
    !$one.subset($two);
}
multi sub *infix:«⊉» (Set $one, Set $two) returns Set {
    !$one.superset($two);
}
multi sub *infix:«⊊» (Set $one, Set $two) returns Set {
    $one.proper_subset($two);
}
multi sub *infix:«⊋» (Set $one, Set $two) returns Set {
    $one.proper_superset($two);
}

# several unicode operators for includes!
multi sub *infix:<∋> (Set $one, $member) returns Bool {
    $one.includes($member);
}
multi sub *infix:<∈> ($member, Set $set) returns Bool {
    $set.includes($member);
}
multi sub *infix:<∍> (Set $one, $member) returns Bool {
    $one.includes($member);
}
multi sub *infix:<∊> ($member, Set $set) returns Bool {
    $set.includes($member);
}
multi sub *infix:<∌> (Set $one, $member) returns Bool {
    !$one.includes($member);
}
multi sub *infix:<∉> ($member, Set $set) returns Bool {
    !$set.includes($member);
}

# these methods are for overloaded operations with non-sets
multi sub *infix:<+> (Set $one, *@args) returns Set {
    $one.union(set(@args));
}
#method *infix:<-> (Set $one, *@args) returns Set {
    #$one.difference(set(@args));
#}
multi sub *infix:<*> (Set $one, *@args) returns Set {
    $one.intersection(set(@args));
}
multi sub *infix:<%> (Set $one, *@args) returns Set {
    $one.symmetric_difference(set(@args));
}
multi sub *infix:<~~> (Set $one, $member) returns Bool {
    $one.includes($member);
}
# XXX -- IIRC, there's a "is commutative" or such, so duplicating shouldn't be
# necessary.
multi sub *infix:<~~> ($member, Set $one) returns Bool {
    $one.includes($member);
}

# Subs to make set operations on arrays
# E.g. [1,2,3] +# [2,5]  ==>  [1,2,3,5]
# (Similar to Ruby)
multi sub *infix:<+#> (@a, @b) returns Array { set(@a).union(set @b).members }
multi sub *infix:<-#> (@a, @b) returns Array { set(@a).difference(set @b).members }
multi sub *infix:<*#> (@a, @b) returns Array { set(@a).intersection(set @b).members }
multi sub *infix:<%#> (@a, $b) returns Array { set(@a).symmetric_difference(set @b).members }

=head1 NAME

Set - Sets for Perl 6

=head1 SYNOPSIS

  use Set;

  my $set = set 23, 42, $some_object;

  say "42 is in the set" if $set.includes(42);
  say "The set contains {$set.size} items";

  $set.insert(13);
  $set.remove(23);

  my @members = $set.members;

  # Set arithmetic with arrays
  say ~([1,2,3] +# [1,2,6])    # 1 2 3 6  (in no particular order)
  say ~([1,2,3] -# [1,2,6])    # 3        (in no particular order)
  say ~([1,2,3] *# [1,2,6])    # 1 2      (in no particular order)
  say ~([1,2,3] %# [1,2,6])    # 3 6      (in no particular order)

=head1 CONSTRUCTORS

=head2 C<set(...)>

Returns a new set containing all parameters.

=head2 C<Set.new()>

Returns a new, empty set.

=head1 METHODS

=head2 C<$set.insert(...)>

Inserts the specifiend items into the set. Returns the number of items inserted.

It is not fatal to insert an item which is already inserted.

=head2 C<$set.remove(...)>

Removes the specified items from the set. Returns the number of items removed.

It is not fatal to remove an item which is not in the set.

=head2 C<$set.includes(...)>, C<$set.has(...)>

Returns true if all given items are in the set. C<has> is an alias for C<includes>.

=head2 C<$set.member($item)>

Returns the specified item if it's in the set.

=head2 C<$set.size()>, C<$set.count()>

Returns the number of elements in the set. C<count> is an alias for C<size>.

=head2 C<$set.invert(...)>

Removes the given items if they are already in the set, or inserts the items if
they're not in the set.

Returns the number of items removed.

=head2 C<$set.clear()>

Clears the set.

=head1 COMPARISION METHODS

=head2 C<$set1.equal($set2)>

Returns true if C<$set1> equals C<$set2>, i.e. if C<$set1> contains all the
items of C<$set2> and C<$set1> and C<$set2> have the same size.

=head2 C<$set1.not_equal($set2)>

Returns true if C<$set1> does not equal C<$set2>.

=head2 C<$set1.subset($set2)>

Returns true if C<$set1> is a subset of C<$set2>.

=head2 C<$set1.superset($set2)>

Returns true if C<$set1> is a superset of C<$set2>.

=head2 C<$set1.proper_subset($set2)>, C<$set1.proper_superset($set2)>

Returns true if C<$set1> is a proper subset (superset) of C<$set2>, i.e. if
C<$set1> has at least one element less (more) than C<$set2>.

=head2 C<$set1.union($set2)>

Returns a new set containing all the elements of C<$set1> and C<$set2>

=head2 C<$set1.intersection($set2)>

Returns a new set containing all the elements of C<$set1> which are in C<$set2>, too.

=head2 C<$set1.difference($set2)>

Returns a new set containing all the elements of C<$set1> which are not in C<$set2>.

=head2 C<$set1.symmetric_difference($set2)>

XXX

=head1 BUGS

Currently, no operators are overloaded. This will change as soon Pugs supports overload operators.

=head1 AUTHORS

Sam "mugwump" Vilain (Code)

Ingo "iblech" Blechschmidt (Documentation)

Stevan "stevan" Little (misc. ugly hacks to make things work for now)

=head1 SEE ALSO

You might want to read the tests of Set.

=cut

