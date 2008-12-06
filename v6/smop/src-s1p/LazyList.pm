knowhow LazyList {
    has $.input;
    has @.evaluated;
    has $.intact;
    has $.exhausted;

    use ArrayIterator;

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
