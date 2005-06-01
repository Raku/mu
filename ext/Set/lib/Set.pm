
use v6;

class Set;

our $DEBUG = 0;

# FIXME - refactor out into a Set role, and a Set::Hash and
# Set::Junction module.

sub set (*@contents) returns Set is export {
    my $set = Set.new;
    say "Making a set of ({@contents.join(',')})" if $DEBUG;
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
has Junction $:members;

method members() returns List {
    $:members.values;
}

method insert($self: *@items) returns Int {
    my $pre_size = 0;
    if ( $:members.defined ) {
	$pre_size = $self.size;
	$:members = any($:members, @items);
    } else {
	$:members = any(@items);
    }
    return ($self.size - $pre_size);
}

method remove($self: *@items) returns Int {
    my $pre_size = 0;
    if ( $:members.defined ) {
	$pre_size = $self.size;
	my $to_remove = none(@items);
	$:members = any($:members.values.grep:{ $_ =:= none($to_remove) });
    } else {
	$:members = any();
    }
    return ($pre_size - $self.size);
}

method includes($self: *@items) returns Bool {
    my $answer = (all(@items) =:= any($:members));
    return $answer;
}

method member($self: $item) returns Object {
    return $item if $self.includes($item);
}

method size() returns Int {
    +$:members.values;
}

method invert($self: *@items) returns Int {
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
    $:members = any();
}

method clone ($self:) returns Set {
    my $set = Set.new;
    $set.insert($self.members);
    return $set;
}

method equal($self: Set $other) returns Bool {

    # for some reason, directly returning this expression does not
    # yield the correct return value.

    say "Comparing: "~$self.stringify~" vs "~$other.stringify if $DEBUG;
    my $rv =
	(($self.size == $other.size) &&
	      ($self.includes($other.members)));
    return $rv;
}

method not_equal($self: Set $other) returns Bool {
    my $rv = !$self.equal($other);
    return $rv;
}

method subset($self: Set $other) returns Bool {
    my $rv = ($self.size <= $other.size && $other.includes($self.members));
    return $rv;
}
method proper_subset($self: Set $other) returns Bool {
    my $rv = ($self.size < $other.size && $other.includes($self.members));
    return $rv;
}
method superset($self: Set $other) returns Bool {
    my $rv = $other.subset($self);
    return $rv;
}
method proper_superset($self: Set $other) returns Bool {
    my $rv = ($other.proper_subset($self));
    return $rv;
}

method stringify() returns Str {
    return("set(" ~ $?SELF.members.join(" ") ~ ")");
}


method union($self: Set $other) returns Set {
    say "Self is "~$self.stringify~", other is "~$other.stringify if $DEBUG;
    my $set = set($self.members, $other.members);
    say "Returning union of "~$set.stringify if $DEBUG;
    return $set;
}
method intersection($self: Set $other) returns Set {
    my $rv = set($self.members.grep:{ $other.includes($_) });
    return $rv;
}
method difference($self: Set $other) returns Set {
    my $rv = set($self.members.grep:{ !$other.includes($_) });
    return $rv;
}

method symmetric_difference($self: Set $other) returns Set {

    # this one was particularly disturbing..
    say "Symmetric difference: "~$self.stringify~" % "~$other.stringify if $DEBUG;
    my $one_diff = $self.difference($other);
    say "One diff: "~$one_diff.stringify if $DEBUG;
    my $two_diff = $other.difference($self);
    say "Two diff: "~$two_diff.stringify if $DEBUG;
    my $symm_diff = $one_diff.union($two_diff);
    say "Symm. diff: "~$symm_diff.stringify if $DEBUG;
    return $symm_diff;
}

# FIXME ... $?PACKAGE ?
our &Set::count ::= &Set::size;
our &Set::has   ::= &Set::includes;

# Hash/Array compatibility
our &Set::elems ::= &Set::size;
#our &Set::values ::= &Set::members;

# unicode intersection
multi sub infix:<∩> (Set $one, Set $two) returns Set {
    $one.intersection($two);
}

# unicode union
multi sub infix:<∪> (Set $one, Set $two) returns Set {
    $one.union($two);
}

# addition is union
multi sub infix:<+> (Set $one, Set $two) returns Set {
    $one.union($two);
}

# subtraction is difference
#multi sub infix:<-> (Set $one, Set $two) returns Set {
#    $one.difference($two);
#}

# unicode set difference operator
#  note the difference - ∖ vs \ (backslash)
multi sub infix:<∖> (Set $one, Set $two) returns Set {
    $one.difference($two);
}

# multiplication is intersection
multi sub infix:<*> (Set $one, Set $two) returns Set {
    $one.intersection($two);
}

# modulus is symmetric difference
multi sub infix:<%> (Set $one, Set $two) returns Set {
    $one.symmetric_difference($two);
}

# comparison is subset/superset
multi sub infix:<==> (Set $one, Set $two) returns Set {
    $one.equal($two);
}
multi sub infix:<!=> (Set $one, Set $two) returns Set {
    $one.not_equal($two);
}
multi sub infix:<≠> (Set $one, Set $two) returns Set {
    $one.not_equal($two);
}

# what will be used for stringify?
method prefix:<~> (Set $self) returns Str {
    ./stringify
}

multi sub infix:«<» (Set $one, Set $two) returns Set {
    $one.proper_subset($two);
}
multi sub infix:«>» (Set $one, Set $two) returns Set {
    $one.proper_superset($two);
}
multi sub infix:«<=» (Set $one, Set $two) returns Set {
    $one.subset($two);
}
multi sub infix:«>=» (Set $one, Set $two) returns Set {
    $one.superset($two);
}

# look at all these great unicode operators!  :D
multi sub infix:«⊂» (Set $one, Set $two) returns Set {
    $one.proper_subset($two);
}
multi sub infix:«⊃» (Set $one, Set $two) returns Set {
    $one.proper_superset($two);
}
multi sub infix:«⊆» (Set $one, Set $two) returns Set {
    $one.subset($two);
}
multi sub infix:«⊇» (Set $one, Set $two) returns Set {
    $one.superset($two);
}
multi sub infix:«⊄» (Set $one, Set $two) returns Set {
    !$one.proper_subset($two);
}
multi sub infix:«⊅» (Set $one, Set $two) returns Set {
    !$one.proper_superset($two);
}
multi sub infix:«⊈» (Set $one, Set $two) returns Set {
    !$one.subset($two);
}
multi sub infix:«⊉» (Set $one, Set $two) returns Set {
    !$one.superset($two);
}
multi sub infix:«⊊» (Set $one, Set $two) returns Set {
    $one.proper_subset($two);
}
multi sub infix:«⊋» (Set $one, Set $two) returns Set {
    $one.proper_superset($two);
}

# several unicode operators for includes!
multi sub infix:<∋> (Set $one, $member) returns Bool {
    $one.includes($member);
}
multi sub infix:<∈> ($member, Set $set) returns Bool {
    $set.includes($member);
}
multi sub infix:<∍> (Set $one, $member) returns Bool {
    $one.includes($member);
}
multi sub infix:<∊> ($member, Set $set) returns Bool {
    $set.includes($member);
}
multi sub infix:<∌> (Set $one, $member) returns Bool {
    !$one.includes($member);
}
multi sub infix:<∉> ($member, Set $set) returns Bool {
    !$set.includes($member);
}

# these methods are for overloaded operations with non-sets
multi sub infix:<+> (Set $one, *@args) returns Set {
    $one.union(set(@args));
}
#multi sub infix:<-> (Set $one, *@args) returns Set {
#    $one.difference(set(@args));
#}
multi sub infix:<*> (Set $one, *@args) returns Set {
    $one.intersection(set(@args));
}
multi sub infix:<%> (Set $one, *@args) returns Set {
    $one.symmetric_difference(set(@args));
}
multi sub infix:<~~> (Set $one, $member) returns Bool {
    $one.includes($member);
}
# XXX -- IIRC, there's a "is commutative" or such, so duplicating shouldn't be
# necessary.
multi sub infix:<~~> ($member, Set $one) returns Bool {
    $one.includes($member);
}

# Subs to make set operations on arrays
# E.g. [1,2,3] +# [2,5]  ==>  [1,2,3,5]
# (Similar to Ruby)
sub infix:<+#> (@a, @b) returns Array { set(@a).union(set @b).members }
sub infix:<-#> (@a, @b) returns Array { set(@a).difference(set @b).members }
sub infix:<*#> (@a, @b) returns Array { set(@a).intersection(set @b).members }
sub infix:<%#> (@a, $b) returns Array { set(@a).symmetric_difference(set @b).members }

=head1 NAME

Set - Sets for Perl 6

=head1 SYNOPSIS

  # WARNING - this manual page contains substantial use of Unicode
  # characters.  They may not be rendered correctly.  ASCII versions
  # are supplied nearby.
  use Set;

  my $set = set 23, 42, $some_object;

  say "42 is in the set" if $set.includes(42);
  say "The set contains {$set.size} items";

  $set.insert(13);
  $set.remove(23);

  my @members = $set.members;

  # various common set operations
  my $set2 = set(1..10);

  # various set operations
  my $union        = $set ∪ $set2;  # or "+"
  my $intersection = $set ∩ $set2;  # or "*"
  my $difference   = $set ∖ $set2;  # or "-"

  # symmetric difference doesn't have a maths operator
  my $sym_difference = $set % $set2;

  # same as
  $sym_difference = ( $set ∖ $set2 ) ∪ ( $set2 ∖ $set );
  $sym_difference = ( $set - $set2 ) + ( $set2 - $set );

  # sets support subset, superset, etc operators
  my $is_proper_superset = $set ⊃ $set2;  # or ">"
  my $is_proper_subset   = $set ⊂ $set2;  # or "<"
  my $is_superset        = $set ⊇ $set2;  # or ">="
  my $is_subset          = $set ⊆ $set2;  # or "<="

  # other comparison operators available: ⊄, ⊅, ⊈, ⊉
  # ⊊ is the same as ⊂, and ⊋ is the same as ⊃

  # to test membership
  my $contains_7  = $set2 ∋ 7;  # or  7 ∈ $set2
  my $contains_11 = $set2 ∌ 11; # or 11 ∉ $set2

  # smartmatch may also be used
  $contains_7     = $set2 ~~ 7;

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

=head2 C<∅>

Also returns a new, empty set.

=head1 METHODS

=head2 C<$set.insert(...)>

Inserts the specifiend items into the set. Returns the number of items
inserted.

It is not fatal to insert an item which is already inserted.  In fact,
this is commonly used for directed graph traversal;

  my @to_do = ($object);
  my $seen = set(@to_do);

  while my $item = @to_do.shift {

      # ... do something with $item ...

      if ($item.isa(Container)) {
          @to_do.push($item.members.grep:{ $seen.insert($_) });
      }
  }

You can also use the `+=' operator to add new values into a set;

  $set += (1,2,3,4);

Note that in this instance, the information about how many members
were actually new is discarded.

=head2 C<$set.remove(...)>

Removes the specified items from the set. Returns the number of items
removed.

It is not fatal to remove an item which is not in the set.

=head2 C<$set.includes(...)>, C<$set.has(...)>

Returns true if all given items are in the set. C<has> is an alias for
C<includes>.

=head2 C<$set ~~ $item>, C<$item ~~ $set>

Operator version of C<.includes()>.

=head2 C<$set ∋ $item>, C<$item ∈ $set>

Unicode versions of C<.includes()>.

=head2 C<$set ∌ $item>, C<$item ∉ $set>

Complemented unicode versions of C<.includes()>.

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

=head2 C<$set1 == $set2>

Returns true if C<$set1> equals C<$set2>, i.e. if C<$set1> contains all the
items of C<$set2> and C<$set1> and C<$set2> have the same size.

=head2 C<$set1.not_equal($set2)>

=head2 C<$set1 != $set2>

=head2 C<$set1 ≠ $set2>

Returns true if C<$set1> does not equal C<$set2>.

=head2 C<$set1.subset($set2)>

=head2 C<$set1 E<lt>= $set2)>

=head2 C<$set1 ⊆ $set2)>

Returns true if C<$set1> is a subset of C<$set2>.

=head2 C<$set1.superset($set2)>

=head2 C<$set1 E<gt>= $set2)>

=head2 C<$set1 ⊇ $set2>

Returns true if C<$set1> is a superset of C<$set2>.

=head2 C<$set1.proper_subset($set2)>

=head2 C<$set1 ⊂ $set2>

=head2 C<$set1 ⊊ $set2>

=head2 C<$set1 E<lt> $set2)>

=head2 C<$set1.proper_superset($set2)>

=head2 C<$set1 ⊃ $set2>

=head2 C<$set1 ⊋ $set2>

=head2 C<$set1 E<gt> $set2)>

Returns true if C<$set1> is a proper subset (superset) of C<$set2>, i.e. if
C<$set1> has at least one element less (more) than C<$set2>.

=head2 C<$set1.union($set2)>

=head2 C<$set1 ∪ $set2>

=head2 C<$set1 + $set2>

Returns a new set containing all the elements of C<$set1> and C<$set2>

=head2 C<$set1.intersection($set2)>

=head2 C<$set1 ∩ $set2>

=head2 C<$set1 * $set2>

Returns a new set containing all the elements of C<$set1> which are in C<$set2>, too.

=head2 C<$set1.difference($set2)>

=head2 C<$set1 ∖ $set2>

=head2 C<$set1 * $set2>

Returns a new set containing all the elements of C<$set1> which are
not in C<$set2>.  Note that "∖" (\x{2216} - set minus) is not the same
character as "\" (\x{005C} - backslash).

=head2 C<$set1.symmetric_difference($set2)>

=head2 C<$set1 % $set2>

Returns all items that are only in one of the two sets.  This is
equivalent to any of the below:

   ( $set1 ∪ $set2 ) ∖ ( $set1 ∩ $set2 )
   ( $set1 ∖ $set2 ) ∪ ( $set2 ∖ $set1 )

   ( $set1 + $set2 ) - ( $set1 * $set2 )
   ( $set1 - $set2 ) + ( $set2 - $set1 )

=head1 BUGS

Currently, no operators are overloaded. This will change as soon Pugs supports overload operators.

=head1 AUTHORS

Sam "mugwump" Vilain (Code)

Ingo "iblech" Blechschmidt (Documentation)

Stevan "stevan" Little (misc. ugly hacks to make things work for now)

=head1 SEE ALSO

You might want to read the tests of Set.

=cut

