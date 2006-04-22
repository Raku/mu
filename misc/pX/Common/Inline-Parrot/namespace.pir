.namespace ["InlineParrot"]
.sub main :main
	$P0 = new .PerlInt
	$P0 = 5
	store_global "x", $P0
	.local pmc ns
	ns = split "::","parrot::InlineParrot"
	$P3 = get_namespace ns
	$P1 = find_global $P3,"x"
	say $P1
.end
 #parrot_ns = VTABLE_get_pmc_keyed_int(interpreter,
 #             interpreter->HLL_namespace 0);
 #symbol = VTABLE_set_pmc_keyed_str(interpreter, parrot_ns, const_string(interpreter, "Foo"));
