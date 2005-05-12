
use v6;

Val ::= Object;     # within the scope of this file?
# but does "==";

# Val.can("==");

Role Set {

    method new(Val @items) {
	.bless();
	.insert(@items);
    };

    # we could probably get the mimimum implementation requirements
    # down to just insert_one and members ;)
    method members(Val $item) returns Array of Val { ... };
    method insert(Val @items) returns Int  { ... };
    method remove(Val @items) returns Int  { ... };

    method includes(Val @items) returns Bool {
	all(@items) == any(.members)
    };

    method member(Val $item) returns Val {
	any(.members) == $item
    };

    method size returns Int {
	.members
    };

    method invert(Val @items) returns Int {
	my int $rv;
	for @items -> $item {
	    if ( .includes($item) ) {
		.remove($item);
		$rv++;
	    } else {
		.insert($item);
	    }
	}
	$rv;
    };

    method clear {
	.remove(.members);
    };

    &element  ::= &member;
    &has      ::= &includes;
    &contains ::= &includes;
    &count    ::= &size;
    &delete   ::= &remove;
}

# an implementation of the Set class using junctions
class Set does Set {

    has junction $:junc;

    method set(@anything) is export {
	.bless();
	$:junc=any(@anything);
    };

    &new ::= &set;

    method insert(@items) {
	my $success;
	for @item -> $item {
	    if ( $item != $:junc ) {
		$success++;
		$:junc = any($:junc, $item);
	    }
	}
	$success;
    }
    method remove(@items) {
	my $success;
	for @item -> $item {
	    if ( $item == $:junc ) {
		$success++;
		# ENOTSPECCED
		$:junc = any($:junc) - $item;
	    }
	}
	return $success;
    }

    method includes(@items) {
	( all(@items) == .junc );
    }

    method member($item) {
	# alternate precedence should make it return correct thing
	( $:junc == $item )
    }

    method members {
	return $:junc.members;  # ENOTSPECCED
    }

    method clear {
	$:junc = any();
    }

}

