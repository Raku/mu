$*OUT.FETCH.print("1..2\n");
my $outer_scope = $SMOP__S1P__LexicalScope.new();
my $inner_scope = $SMOP__S1p__LexicalScope.new();
$inner_scope.outer.STORE($outer_scope);
$outer_scope.items.{"$_"}.STORE("ok 1");
$SMOP__S1P__DefaultBlockSignature.BIND($SMOP__NATIVE__capture.new(), $inner_scope);
$*OUT.FETCH.print($inner_scope.lookup("$_"));
$SMOP__S1P__DefaultBlockSignature.BIND($SMOP__NATIVE__capture.new("ok 2"), $inner_scope);
$*OUT.FETCH.print($inner_scope.lookup("$_");
