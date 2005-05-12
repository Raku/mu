
use v6;

class Set;

sub set (*@contents) returns Set is export {
    my $set = Set.new;
    $set.insert(@contents);
    return $set;
}

# the Set is represented as a hash of (v => v)
has Hash %:members;

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

our &Set::count ::= &Set::size;
our &Set::has   ::= &Set::includes;
