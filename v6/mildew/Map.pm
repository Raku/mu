knowhow MapIterator {
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

knowhow ArrayIterator {
    has @.input;
    has $.count;


    # this is also a knowhow
    method new($proto: :$input) {
        my $ret = $proto.clone;
        $ret.input = $input;
        $ret.count = 0;
        return $ret;
    }

    method clone {
        return self.^clone;
    }

    method prefix:<=> {
        if (@.input.exists($.count)) {
            return \@.input[$.count++];
        } else {
            fail OutOfItemsException;
        }
    }

}

knowhow LazyList {
    has $.input;
    has @.evaluated;
    has $.intact;
    has $.exhausted;

    # this is also a knowhow
    method new($proto: :$input) {
        my $ret = $proto.clone;
        $ret.input = $input;
        return $ret;
    }

    method clone {
        return self.^clone;
    }

    method Iterator {
        if ($.intact) {
            return $.input.clone;
        } elsif ($.exhausted) {
            return @.evaluated.Iterator;
        } else {
            return ArrayIterator.new( input => self );
        }
    }

    method postcircumfix:<[ ]>($index) {
        if ($.exhausted ||
            @.evaluated.elems > $index) {
            return @.evaluated[$index];
        } else {
            $.intact = 0;
            {
                @.evaluated.push(|=$.input);
                CATCH {
                    when OutOfItemsException {
                        $.exhausted = 1;
                    }
                }
            }
            return self[$index];
        }
    }

    method exists($index) {
        self[$index];
        return @.evaluated.exists($index);
    }

    method elems {
        self[+Inf];
        return @.evaluated.elems;
    }
}

my sub map($code, *@items) {
    my $item_iterator = ItemIterator.new( input =>  @items.Iterator() );
    return LazyList.new( input => MapIterator.new( input => $item_iterator,
                                                   code  => $code ) );
}
