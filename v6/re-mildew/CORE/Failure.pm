role Failure {
    has $.handled;
    has $.exception;
    method true {
        #$!handled = True;
        ::False;
    }
    method defined {
        #$!handled = True;
        ::False;
    }
    method handled {
        say "handled";
        ::False;
        #$!handled;
    }
    method UNKNOWN_METHOD() {
        ::Exception.new.throw;
    }
}
my sub fail {
    my $failure = Failure.new;
    return $failure;
    #$failure.exception = ::Exception.new;
}
$LexicalPrelude.{'Failure'} := ::Failure;
$LexicalPrelude.{'&fail'} := &fail;
