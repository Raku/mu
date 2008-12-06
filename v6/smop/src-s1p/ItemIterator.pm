knowhow ItemIterator {
    has $.input;
    has @.prefetch;
    
    # this is also a knowhow
    method new($proto: :$input) {
	my $ret = $proto.clone;
	$ret.input = $input;
	return $ret;
    }
    
    method clone {
	return self.^clone;
    }
    
    method prefix:<=> {
	if (@.prefetch.elems) {
	    return @.prefetch.shift;
	} else {
	    @.prefetch.push(|=$.input);
	}
    }
    
}
