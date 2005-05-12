
use v6;

class Set;

sub set(*@contents) is export returns Set {
    return Set.new(*@contents);
};

# the Set is represented as a hash of (v => v)
has Hash %:members;

sub new($self: *@items) {
    $self.insert(@items);
};

sub members($self:) returns List {
    %:members.values;
};

sub insert($self: *@items) returns int {
    my int $inserted = 0;
    for @items -> $item {
	if ( !%:members.exists($item) ) {
	    $inserted++;
	    %:members<$item> = $item;
	}
    }
    return $inserted;
};

sub remove($self: *@items) returns int {
    my int $removed = 0;
    for @items -> $item {
	if ( %:members.delete($item) ) {
	    $removed++;
	}
    }
    return $removed;
};

sub includes($self: *@items) returns Bool {
    return %:members.exists(all(@items));
};

sub member($self: $item) returns Object {
    return %:members<$item>;
};

sub size($self:) returns int {
    %:members.size;
};

sub invert($self: *@items) returns int {
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
};

sub clear($self:) {
    %:members=();
};

&element  ::= &member;
&has      ::= &includes;
&contains ::= &includes;
&count    ::= &size;
&delete   ::= &remove;


