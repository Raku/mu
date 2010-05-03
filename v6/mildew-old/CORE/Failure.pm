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
        $.exception.throw;
    }
}
role DollarBang {
    has @.failures;
    method cleanup() {
        map(sub ($failure) {
            if ($failure.handled) {
            } else {
                $failure.throw;
            }
        },self.failures);
    }
}
my sub fail {
    my $failure = Failure.new;
    $failure.exception = ::Exception.new;
    $failure;
    my $e = ::ControlExceptionReturn.new();
    $e.capture = $failure;
    $e.routine = CALLER::<&?ROUTINE>;
    $e.throw;
}
$LexicalPrelude{'Failure'} := ::Failure;
$LexicalPrelude{'DollarBang'} := ::DollarBang;
$LexicalPrelude{'&fail'} := &fail;
