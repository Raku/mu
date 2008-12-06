module Map {

    use ItemIterator;
    use LazyList;

    my knowhow MapIterator {
	has $.code;
	has $.input;
	# we need to implement new here because this is not a class, but a
	# knowhow.

	method new($proto: :$code, :$input) {
	    my $ret = $proto.clone;
	    $ret.code = $code;
	    $ret.input = $input;
	    return $ret;
	}

	method clone {
	    return self.^clone;
	}

	method prefix:<=> {
	    if ($.code.signature.arity > 1) {
		my @inputitems;
		{
		    for 1..$.code.signature.arity {
			@inputitems.push(=$.input);
		    }
		    CATCH {
			when OutOfItemsException {
			    if (! @inputitems.elems) {
				.rethrow;
			    }
			}
		    }
		}
		return \$.code(|@inputitems);
	    } else {
		return \$.code(=$.input)
	    }
	}

    }
    
    sub map($code, *@items) is export {
	my $item_iterator = ItemIterator.new( input =>  @items.Iterator() );
	return LazyList.new( input => MapIterator.new( input => $item_iterator,
						       code  => $code ) );
    }

}
