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
