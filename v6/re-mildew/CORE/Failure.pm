role Failure {
    has $.handled;
    has $.exception;
    method true() {
        $.handled = True;
        ::False;
    }
    method defined() {
        $.handled = True;
        ::False;
    }
    method FETCH() {
        self;
    }
    method throw() {
        $.exception.throw;
    }
    method UNKNOWN_METHOD($identifier) {
        say "UNKNOWN METHOD: $identifer";
        $.exception.throw;
    }
}
role DollarBang {
    has @.failures;
    method cleanup() {
        my sub throw($failure) {
            if ($failure.handled) {
            } else {
                $failure.throw;
            }
        }
        map(&throw,self.failures);
    }
}
my sub fail {
    my $failure = Failure.new;
    $failure.exception = ::Exception.new;
    $failure;
}
$LexicalPrelude.{'Failure'} := ::Failure;
$LexicalPrelude.{'DollarBang'} := ::DollarBang;
$LexicalPrelude.{'&fail'} := &fail;
