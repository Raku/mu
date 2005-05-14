
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
    # XXX - this is nessecary because the hash 
    # does not seem to get initialized properly
    # and calling members before any values are
    # set results in a error
    %:members ?? %:members.values :: ();
}

# NOTE:
# this is a hack to make this work well
# with objects. Eventually it should be 
# removed (IIRC hash keys are not supposed
# to stringify).
method _stringify ($item) returns Str {
    my $str_item = ~$item;
    $str_item ~= $item.id() if $str_item ~~ rx:perl5/^\<obj\:/;
    return $str_item;
}

method insert($self: *@items) returns Int {
    my Int $inserted = 0;
    for @items -> $item {
        my $key = $self._stringify($item);
    	unless ( %:members.exists($key) ) {
	        $inserted++;
    	    %:members{$key} = $item;
    	}
    }
    return $inserted;
}

method remove($self: *@items) returns Int {
    my Int $removed = 0;
    for @items -> $item {
	    if ( %:members.delete($self._stringify($item)) ) {
	        $removed++;
	    }
    }
    return $removed;
}

method includes($self: *@items) returns Bool {
    return %:members.exists(all(@items.map:{ $self._stringify($_) }));
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
    set($self.members.grep:{ $other.includes($_) });
}
method difference($self: Set $other) returns Set {
    set($self.members.grep:{ !$other.includes($_) });
}

method symmetric_difference($self: Set $other) returns Set {
    $self.difference($other).union($other.difference($self));
}

our &Set::count ::= &Set::size;
our &Set::has   ::= &Set::includes;

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
