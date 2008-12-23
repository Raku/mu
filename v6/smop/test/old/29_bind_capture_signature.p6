
$*OUT.FETCH.print("1..1\n");

# Here we create the outer lexicalscopes
$*outer_scope = $SMOP__S1P__LexicalScope.new();

# On this test, we want to make sure this value is not found
$*outer_scope.FETCH.entries.{"$_"} = $SMOP__S1P__Scalar.new("not ok 1\n");

# when the outer block is executed, the inner scope for the code
# object is created and linked to the outer scope.
$*inner_scope = $SMOP__S1P__LexicalScope.new();
$*inner_scope.FETCH.outer = $*outer_scope.FETCH;

# The signature is bound at .() time, and at this point the value should be
# saved in the inner scope.
$SMOP__S1P__BindCaptureSignature.BIND($SMOP__S1P__Capturize.capturize("ok 1\n"), $*inner_scope.FETCH);

# Now we make a lookup in the inner scope, to make sure it takes the correct value.
$*OUT.FETCH.print($*inner_scope.FETCH.lookup("$_").FETCH.FETCH.positional(0));
