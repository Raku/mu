#!pugs
use v6;

require Test-0.0.2;

plan( 41 );

######################################################################
# First ensure the modules to test will compile, are correct versions:

@*INC.unshift( 't/lib' ); # Note: I did "use lib 't/lib';" in Perl 5.
require t_SRT_Verbose;
require t_SRT_Terse;
require t_SRT_Abstract;
require SQL::Routine-0.0.2;
require SQL::Routine::L::en-0.0.2;

######################################################################
# Here are some utility methods:

sub message {
	my ($detail) = @_;
	print "-- $detail\n";
}

sub error_to_string( $message ) {
	my $translator = Locale::KeyedText.new_translator( ['SQL::Routine::L::'], ['en'] );
	my Str $user_text = $translator.translate_message( $message );
	unless( $user_text ) {
		return ref($message) ?? "internal error: can't find user text for a message: "~
			$message.as_string()~" "~$translator.as_string() :: $message;
	}
	return $user_text;
}

######################################################################
# Now perform the actual tests:

message( "START TESTING SQL::Routine - Circular Ref Prevention" );
message( "  Test that circular reference creation can be blocked." );

######################################################################

try {
	my $model = SQL::Routine.new_container();
	$model.auto_set_node_ids( 1 );

	my $vw1 = $model.build_node( 'view', 'foo' );
	my $vw2 = $vw1.build_child_node( 'view', 'bar' );
	my $vw3 = $vw2.build_child_node( 'view', 'bz' );

	my $test1_passed = 0;
	try {
		$vw2.set_primary_parent_attribute( $vw3 );
		CATCH {
			my $exception = $!;
			if( ref($exception) and UNIVERSAL::isa( $exception, 'Locale::KeyedText::Message' ) ) {
				if( $exception.get_message_key() eq 'SRT_N_SET_PP_AT_CIRC_REF' ) {
					$test1_passed = 1;
				}
			}
			$test1_passed or throw $exception;
		}
	};
	ok( $test1_passed, "prevent creation of circular refs - parent is child" );

	my $test2_passed = 0;
	try {
		$vw2.set_primary_parent_attribute( $vw2 );
		CATCH {
			my $exception = $!;
			if( ref($exception) and UNIVERSAL::isa( $exception, 'Locale::KeyedText::Message' ) ) {
				if( $exception.get_message_key() eq 'SRT_N_SET_PP_AT_CIRC_REF' ) {
					$test2_passed = 1;
				}
			}
			$test2_passed or throw $exception;
		}
	}
	ok( $test2_passed, "prevent creation of circular refs - parent is self" );

	$model.destroy();

	CATCH {
		ok( 0, "TESTS ABORTED: "~error_to_string( $! ) );
	}
}

######################################################################

message( "DONE TESTING SQL::Routine - Circular Ref Prevention" );
message( "START TESTING SQL::Routine - t_SRT_Verbose" );
message( "  Test model construction using verbose standard interface." );

######################################################################

try {
	message( "First create the Container object that will be populated ..." );

	my $model = SQL::Routine.new_container();
	isa_ok( $model, 'SQL::Routine::Container', "creation of Container object" );

	message( "Now create a set of Nodes in the Container ..." );

	t_SRT_Verbose.populate_model( $model );
	ok( 1, "creation of Node objects" );

	message( "Now see if deferrable constraints are valid ..." );

	$model.assert_deferrable_constraints();
	ok( 1, "assert all deferrable constraints" );

	message( "Now see if the NID-based output is correct ..." );

	my $expected_output = t_SRT_Verbose.expected_model_nid_xml_output();
	my $actual_output = $model.get_all_properties_as_xml_str();
	is( $actual_output, $expected_output, "verify serialization of objects (NID)" );

	message( "Now see if the SID-long-based output is correct ..." );

	my $expected_output2 = t_SRT_Verbose.expected_model_sid_long_xml_output();
	my $actual_output2 = $model.get_all_properties_as_xml_str( 1 );
	is( $actual_output2, $expected_output2, "verify serialization of objects (SID long)" );

	message( "Now see if the SID-short-based output is correct ..." );

	my $expected_output3 = t_SRT_Verbose.expected_model_sid_short_xml_output();
	my $actual_output3 = $model.get_all_properties_as_xml_str( 1, 1 );
	is( $actual_output3, $expected_output3, "verify serialization of objects (SID short)" );

	message( "Now destroy the objects ..." );

	$model.destroy();
	is( (keys %{$model}), '0', "destruction of all objects" );

	CATCH {
		ok( 0, "TESTS ABORTED: "~error_to_string( $! ) );
	}
}

######################################################################

message( "DONE TESTING SQL::Routine - t_SRT_Verbose" );
message( "START TESTING SQL::Routine - t_SRT_Terse" );
message( "  Test model construction using terse wrapper interface." );

######################################################################

try {
	message( "First create the Container object that will be populated ..." );

	my $model = SQL::Routine.new_container();
	isa_ok( $model, 'SQL::Routine::Container', "creation of Container object" );

	message( "Now create a set of Nodes in the Container ..." );

	$model.auto_assert_deferrable_constraints( 1 ); # also done here to help with debugging
	t_SRT_Terse.populate_model( $model );
	ok( 1, "creation of Node objects" );

	message( "Now see if deferrable constraints are valid ..." );

	$model.assert_deferrable_constraints();
	ok( 1, "assert all deferrable constraints" );

	message( "Now see if the NID-based output is correct ..." );

	my $expected_output = t_SRT_Terse.expected_model_nid_xml_output();
	my $actual_output = $model.get_all_properties_as_xml_str();
	is( $actual_output, $expected_output, "verify serialization of objects (NID)" );

	message( "Now see if the SID-long-based output is correct ..." );

	my $expected_output2 = t_SRT_Terse.expected_model_sid_long_xml_output();
	my $actual_output2 = $model.get_all_properties_as_xml_str( 1 );
	is( $actual_output2, $expected_output2, "verify serialization of objects (SID long)" );

	message( "Now see if the SID-short-based output is correct ..." );

	my $expected_output3 = t_SRT_Terse.expected_model_sid_short_xml_output();
	my $actual_output3 = $model.get_all_properties_as_xml_str( 1, 1 );
	is( $actual_output3, $expected_output3, "verify serialization of objects (SID short)" );

	message( "Now destroy the objects ..." );

	$model.destroy();
	is( (keys %{$model}), '0', "destruction of all objects" );

	CATCH {
		ok( 0, "TESTS ABORTED: "~error_to_string( $! ) );
	}
}

######################################################################

message( "DONE TESTING SQL::Routine - t_SRT_Terse" );
message( "START TESTING SQL::Routine - t_SRT_Abstract" );
message( "  Test model construction using abstract wrapper interface." );

######################################################################

try {
	message( "First create the Container object that will be populated ..." );

	my $model = SQL::Routine.new_container();
	isa_ok( $model, 'SQL::Routine::Container', "creation of Container object" );

	message( "Now create a set of Nodes in the Container ..." );

	$model.auto_assert_deferrable_constraints( 1 ); # also done here to help with debugging
	$model.auto_set_node_ids( 1 );
	$model.may_match_surrogate_node_ids( 1 );
	t_SRT_Abstract.populate_model( $model );
	ok( 1, "creation of Node objects" );

	message( "Now see if deferrable constraints are valid ..." );

	$model.assert_deferrable_constraints();
	ok( 1, "assert all deferrable constraints" );

	message( "Now see if the NID-based output is correct ..." );

	my $expected_output = t_SRT_Abstract.expected_model_nid_xml_output();
	my $actual_output = $model.get_all_properties_as_xml_str();
	is( $actual_output, $expected_output, "verify serialization of objects (NID)" );

	message( "Now see if the SID-long-based output is correct ..." );

	my $expected_output2 = t_SRT_Abstract.expected_model_sid_long_xml_output();
	my $actual_output2 = $model.get_all_properties_as_xml_str( 1 );
	is( $actual_output2, $expected_output2, "verify serialization of objects (SID long)" );

	message( "Now see if the SID-short-based output is correct ..." );

	my $expected_output3 = t_SRT_Abstract.expected_model_sid_short_xml_output();
	my $actual_output3 = $model.get_all_properties_as_xml_str( 1, 1 );
	is( $actual_output3, $expected_output3, "verify serialization of objects (SID short)" );

	message( "Now destroy the objects ..." );

	$model.destroy();
	is( (keys %{$model}), '0', "destruction of all objects" );

	CATCH {
		ok( 0, "TESTS ABORTED: "~error_to_string( $! ) );
	}
}

######################################################################

message( "DONE TESTING SQL::Routine - t_SRT_Abstract" );
message( "START TESTING SQL::Routine - Model Cloning" );
message( "  Test that SRT models can be easily cloned." );

######################################################################

try {
	my $model = SQL::Routine.new_container();
	t_SRT_Terse.populate_model( $model );
	my $props_nid = $model.get_all_properties();
	my $props_sidL = $model.get_all_properties( 1 );
	my $props_sidS = $model.get_all_properties( 1, 1 );
	$model.destroy();

	message( "NID: First build populated Container object from an original model's NID dump..." );

	my $nid_model = SQL::Routine.build_container( $props_nid, 1 );
	isa_ok( $nid_model, 'SQL::Routine::Container', "building Container and Nodes" );

	message( "NID: Now see if deferrable constraints are valid ..." );

	$nid_model.assert_deferrable_constraints();
	ok( 1, "assert all deferrable constraints" );

	message( "NID: Now see if the NID-based output is correct ..." );

	my $nid_expected_output = t_SRT_Terse.expected_model_nid_xml_output();
	my $nid_actual_output = $nid_model.get_all_properties_as_xml_str();
	is( $nid_actual_output, $nid_expected_output, "verify serialization of objects (NID)" );

	message( "NID: Now see if the SID-long-based output is correct ..." );

	my $nid_expected_output2 = t_SRT_Terse.expected_model_sid_long_xml_output();
	my $nid_actual_output2 = $nid_model.get_all_properties_as_xml_str( 1 );
	is( $nid_actual_output2, $nid_expected_output2, "verify serialization of objects (SID long)" );

	message( "NID: Now see if the SID-short-based output is correct ..." );

	my $nid_expected_output3 = t_SRT_Terse.expected_model_sid_short_xml_output();
	my $nid_actual_output3 = $nid_model.get_all_properties_as_xml_str( 1, 1 );
	is( $nid_actual_output3, $nid_expected_output3, "verify serialization of objects (SID short)" );

	message( "NID: Now destroy the Container and its Nodes ..." );

	$nid_model.destroy();
	ok( (keys %{$nid_model}) eq '0', "destruction of all objects" );

	message( "SID-L: First build populated Container object from an original model's SID-long dump..." );

	my $sidL_model = SQL::Routine.build_container( $props_sidL, 1, undef, 1 );
	isa_ok( $sidL_model, 'SQL::Routine::Container', "building Container and Nodes" );

	message( "SID-L: Now see if deferrable constraints are valid ..." );

	$sidL_model.assert_deferrable_constraints();
	ok( 1, "assert all deferrable constraints" );

	message( "SID-L: Now see if the NID-based output is correct ..." );

	my $sidL_expected_output = t_SRT_Terse.expected_model_nid_xml_output();
	my $sidL_actual_output = $sidL_model.get_all_properties_as_xml_str();
	is( $sidL_actual_output, $sidL_expected_output, "verify serialization of objects (NID)" );

	message( "SID-L: Now see if the SID-long-based output is correct ..." );

	my $sidL_expected_output2 = t_SRT_Terse.expected_model_sid_long_xml_output();
	my $sidL_actual_output2 = $sidL_model.get_all_properties_as_xml_str( 1 );
	is( $sidL_actual_output2, $sidL_expected_output2, "verify serialization of objects (SID long)" );

	message( "SID-L: Now see if the SID-short-based output is correct ..." );

	my $sidL_expected_output3 = t_SRT_Terse.expected_model_sid_short_xml_output();
	my $sidL_actual_output3 = $sidL_model.get_all_properties_as_xml_str( 1, 1 );
	is( $sidL_actual_output3, $sidL_expected_output3, "verify serialization of objects (SID short)" );

	message( "SID-L: Now destroy the Container and its Nodes ..." );

	$sidL_model.destroy();
	ok( (keys %{$sidL_model}) eq '0', "destruction of all objects" );

	message( "SID-S: First build populated Container object from an original model's SID-long dump..." );

	my $sidS_model = SQL::Routine.build_container( $props_sidS, 1, undef, 1 );
	isa_ok( $sidS_model, 'SQL::Routine::Container', "building Container and Nodes" );

	message( "SID-S: Now see if deferrable constraints are valid ..." );

	$sidS_model.assert_deferrable_constraints();
	ok( 1, "assert all deferrable constraints" );

	message( "SID-S: Now see if the NID-based output is correct ..." );

	my $sidS_expected_output = t_SRT_Terse.expected_model_nid_xml_output();
	my $sidS_actual_output = $sidS_model.get_all_properties_as_xml_str();
	is( $sidS_actual_output, $sidS_expected_output, "verify serialization of objects (NID)" );

	message( "SID-S: Now see if the SID-long-based output is correct ..." );

	my $sidS_expected_output2 = t_SRT_Terse.expected_model_sid_long_xml_output();
	my $sidS_actual_output2 = $sidS_model.get_all_properties_as_xml_str( 1 );
	is( $sidS_actual_output2, $sidS_expected_output2, "verify serialization of objects (SID long)" );

	message( "SID-S: Now see if the SID-short-based output is correct ..." );

	my $sidS_expected_output3 = t_SRT_Terse.expected_model_sid_short_xml_output();
	my $sidS_actual_output3 = $sidS_model.get_all_properties_as_xml_str( 1, 1 );
	is( $sidS_actual_output3, $sidS_expected_output3, "verify serialization of objects (SID short)" );

	message( "SID-S: Now destroy the Container and its Nodes ..." );

	$sidS_model.destroy();
	ok( (keys %{$sidS_model}) eq '0', "destruction of all objects" );

	CATCH {
		ok( 0, "TESTS ABORTED: "~error_to_string( $! ) );
	}
}

######################################################################

message( "DONE TESTING SQL::Routine - Model Cloning" );

######################################################################

1;
