
$*OUT.FETCH.print("1..2\n");

# Here we create the outer lexicalscopes
$*outer_scope.STORE($SMOP__S1P__LexicalScope.new());

# Here's the value that should be fetched, because it was there at closure
# creation time.
$*outer_scope.FETCH.entries.{"$_"}.STORE("ok 1\n");

# when the outer block is executed, the inner scope for the code
# object is created and linked to the outer scope.
$*inner_scope.STORE($SMOP__S1P__LexicalScope.new());
$*inner_scope.FETCH.outer.STORE($*outer_scope.FETCH);

# The signature is bound at .() time, and at this point the value should be
# saved in the inner scope.
$SMOP__S1P__DefaultBlockSignature.BIND($SMOP__S1P__Capturize.capturize(), $*inner_scope.FETCH);

# To make sure the bind worked as expected, let's change the value in the outer
# scope. The inner scope should be a different variable, so the lookup should get its
# own value
$*outer_scope.FETCH.entries.{"$_"}.STORE("not ok 1\n");

# Now we make a lookup in the inner scope, to make sure it takes the correct value.
$*OUT.FETCH.print($*inner_scope.FETCH.lookup("$_").FETCH);

# And now a last test which is to send a value in the capture, which should be set in
# the inner scope.

$SMOP__S1P__DefaultBlockSignature.BIND($SMOP__S1P__Capturize.capturize("ok 2\n"), $*inner_scope.FETCH);

# And now we make the lookup.
$*OUT.FETCH.print($*inner_scope.FETCH.lookup("$_").FETCH);
