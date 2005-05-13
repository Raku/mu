
use v6;

class Set;

sub set (*@contents) returns Set is export {
    my $set = Set.new;
    $set.insert(@contents);
    return $set;
}

# the Set is represented as a hash of (v => v)
has %:members;

method members() returns List {
    %:members.values;
}

method insert(*@items) returns Int {
    my Int $inserted = 0;
    for @items -> $item {
	unless ( %:members.exists($item) ) {
	    $inserted++;
	    %:members{$item} = $item;
	}
    }
    return $inserted;
}

method remove(*@items) returns Int {
    my Int $removed = 0;
    for @items -> $item {
	if ( %:members.delete($item) ) {
	    $removed++;
	}
    }
    return $removed;
}

method includes(*@items) returns Bool {
    return %:members.exists(all(@items));
}

method member($item) returns Object {
    return %:members{$item}
}

method size() returns int {
    +%:members.keys;
}

method invert(*@items) returns int {
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
    undef %:members;
}

method clone ($self:) returns Set {
    my $set = Set.new;
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

method union($self: Set $other) returns Set {
    set($self.members, $other.members);
}
method intersection($self: Set $other) returns Set {
    set(grep:{ $other.includes($_) } $self.members);
}
method difference($self: Set $other) returns Set {
    set(grep:{ !$other.includes($_) } $self.members);
}

method symmetric_difference($self: Set $other) returns Set {
    $self.difference($other).union($other.difference($self));
}

#our &Set::count ::= &Set::size;
#our &Set::has   ::= &Set::includes;

=for a later time, when 'overloading' works...

# what will be used for stringify?
method prefix:<~> (Set $one, *@args) returns Set {
    $one.symmetric_difference(set(@args));
}

# addition is union
method infix:<+> (Set $one, Set $two) returns Set {
    $one.union($two);
}
method infix:<+> (Set $one, *@args) returns Set {
    $one.union(set(@args));
}

# subtraction is difference
method infix:<-> (Set $one, Set $two) returns Set {
    $one.difference($two);
}
method infix:<-> (Set $one, *@args) returns Set {
    $one.difference(set(@args));
}

# multiplication is intersection
method infix:<*> (Set $one, Set $two) returns Set {
    $one.intersection($two);
}
method infix:<*> (Set $one, *@args) returns Set {
    $one.intersection(set(@args));
}

# modulus is symmetric difference
# subtraction is difference
method infix:<%> (Set $one, Set $two) returns Set {
    $one.symmetric_difference($two);
}
method infix:<%> (Set $one, *@args) returns Set {
    $one.symmetric_difference(set(@args));
}

# comparison is subset/superset
method infix:"==" (Set $one, Set $two) returns Set {
    $one.equal($two);
}
method infix:"!=" (Set $one, Set $two) returns Set {
    $one.not_equal($two);
}
method infix:"<" (Set $one, Set $two) returns Set {
    $one.proper_subset($two);
}
method infix:">" (Set $one, *@args) returns Set {
    $one.proper_superset(set(@args));
}
method infix:"<=" (Set $one, Set $two) returns Set {
    $one.subset($two);
}
method infix:">=" (Set $one, *@args) returns Set {
    $one.superset(set(@args));
}



=cut
