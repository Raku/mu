#!pugs
use v6;

module t_SRT_Terse;

# This module is used when testing SQL::Routine.
# These tests check that a model can be built using the terse wrapper 
# interface without errors, and serializes to the correct output.
# This module contains sample input and output data which is used to test 
# SQL::Routine, and possibly other modules that are derived from it.

######################################################################

sub populate_model( SQL::Routine::Container $model ) {

	##### NEXT SET CATALOG ELEMENT-TYPE DETAILS #####

	$model.build_child_node_trees( [ map { { 'NODE_TYPE' => 'scalar_data_type', 'ATTRS' => $_ } } (
		{ 'id' =>  1, 'si_name' => 'bin1k' , 'base_type' => 'STR_BIT', 'max_octets' =>  1_000, },
		{ 'id' =>  2, 'si_name' => 'bin32k', 'base_type' => 'STR_BIT', 'max_octets' => 32_000, },
		{ 'id' =>  3, 'si_name' => 'str4'  , 'base_type' => 'STR_CHAR', 'max_chars' =>     4, 'store_fixed' => 1, 
			'char_enc' => 'ASCII', 'trim_white' => 1, 'uc_latin' => 1, 
			'pad_char' => ' ', 'trim_pad' => 1, },
		{ 'id' =>  4, 'si_name' => 'str10' , 'base_type' => 'STR_CHAR', 'max_chars' =>    10, 'store_fixed' => 1, 
			'char_enc' => 'ASCII', 'trim_white' => 1, 
			'pad_char' => ' ', 'trim_pad' => 1, },
		{ 'id' =>  5, 'si_name' => 'str30' , 'base_type' => 'STR_CHAR', 'max_chars' =>    30, 
			'char_enc' => 'ASCII', 'trim_white' => 1, },
		{ 'id' =>  6, 'si_name' => 'str2k' , 'base_type' => 'STR_CHAR', 'max_chars' => 2_000, 'char_enc' => 'UTF8', },
		{ 'id' =>  7, 'si_name' => 'byte' , 'base_type' => 'NUM_INT', 'num_precision' =>  3, },
		{ 'id' =>  8, 'si_name' => 'short', 'base_type' => 'NUM_INT', 'num_precision' =>  5, },
		{ 'id' =>  9, 'si_name' => 'int'  , 'base_type' => 'NUM_INT', 'num_precision' => 10, },
		{ 'id' => 10, 'si_name' => 'long' , 'base_type' => 'NUM_INT', 'num_precision' => 19, },
		{ 'id' => 11, 'si_name' => 'ubyte' , 'base_type' => 'NUM_INT', 'num_precision' =>  3, 'num_unsigned' => 1, },
		{ 'id' => 12, 'si_name' => 'ushort', 'base_type' => 'NUM_INT', 'num_precision' =>  5, 'num_unsigned' => 1, },
		{ 'id' => 13, 'si_name' => 'uint'  , 'base_type' => 'NUM_INT', 'num_precision' => 10, 'num_unsigned' => 1, },
		{ 'id' => 14, 'si_name' => 'ulong' , 'base_type' => 'NUM_INT', 'num_precision' => 19, 'num_unsigned' => 1, },
		{ 'id' => 15, 'si_name' => 'float' , 'base_type' => 'NUM_APR', 'num_octets' => 4, },
		{ 'id' => 16, 'si_name' => 'double', 'base_type' => 'NUM_APR', 'num_octets' => 8, },
		{ 'id' => 17, 'si_name' => 'dec10p2', 'base_type' => 'NUM_EXA', 'num_precision' =>  10, 'num_scale' => 2, },
		{ 'id' => 18, 'si_name' => 'dec255' , 'base_type' => 'NUM_EXA', 'num_precision' => 255, },
		{ 'id' => 19, 'si_name' => 'boolean', 'base_type' => 'BOOLEAN', },
		{ 'id' => 20, 'si_name' => 'datetime', 'base_type' => 'DATM_FULL', 'calendar' => 'ABS', },
		{ 'id' => 21, 'si_name' => 'dtchines', 'base_type' => 'DATM_FULL', 'calendar' => 'CHI', },
		{ 'id' => 22, 'si_name' => 'sex'   , 'base_type' => 'STR_CHAR', 'max_chars' =>     1, 'char_enc' => 'ASCII', },
		{ 'id' => 23, 'si_name' => 'str20' , 'base_type' => 'STR_CHAR', 'max_chars' =>    20, 'char_enc' => 'ASCII', },
		{ 'id' => 24, 'si_name' => 'str100', 'base_type' => 'STR_CHAR', 'max_chars' =>   100, 'char_enc' => 'ASCII', },
		{ 'id' => 25, 'si_name' => 'str250', 'base_type' => 'STR_CHAR', 'max_chars' =>   250, 'char_enc' => 'ASCII', },
		{ 'id' => 26, 'si_name' => 'entitynm', 'base_type' => 'STR_CHAR', 'max_chars' =>  30, 'char_enc' => 'ASCII', },
		{ 'id' => 27, 'si_name' => 'generic' , 'base_type' => 'STR_CHAR', 'max_chars' => 250, 'char_enc' => 'ASCII', },
	) ] );

	my $sex = $model.find_node_by_id( '22' );
	$sex.build_child_node_trees( [ map { { 'NODE_TYPE' => 'scalar_data_type_opt', 'ATTRS' => $_ } } (
		{ 'id' => 28, 'si_value' => 'M', },
		{ 'id' => 29, 'si_value' => 'F', },
	) ] );

	$model.build_child_node_tree( { 'NODE_TYPE' => 'row_data_type', 
			'ATTRS' => { 'id' => 304, 'si_name' => 'person', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'row_data_type_field', 'ATTRS' => $_ } } (
			{ 'id' => 220, 'si_name' => 'person_id'   , 'scalar_data_type' =>  9, },
			{ 'id' => 221, 'si_name' => 'alternate_id', 'scalar_data_type' => 23, },
			{ 'id' => 222, 'si_name' => 'name'        , 'scalar_data_type' => 24, },
			{ 'id' => 223, 'si_name' => 'sex'         , 'scalar_data_type' => 22, },
			{ 'id' => 224, 'si_name' => 'father_id'   , 'scalar_data_type' =>  9, },
			{ 'id' => 225, 'si_name' => 'mother_id'   , 'scalar_data_type' =>  9, },
		) ),
	] } );

	$model.build_child_node_tree( { 'NODE_TYPE' => 'row_data_type', 
			'ATTRS' => { 'id' => 312, 'si_name' => 'person_with_parents', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'row_data_type_field', 'ATTRS' => $_ } } (
			{ 'id' => 116, 'si_name' => 'self_id'    , 'scalar_data_type' =>  9, },
			{ 'id' => 117, 'si_name' => 'self_name'  , 'scalar_data_type' => 24, },
			{ 'id' => 118, 'si_name' => 'father_id'  , 'scalar_data_type' =>  9, },
			{ 'id' => 119, 'si_name' => 'father_name', 'scalar_data_type' => 24, },
			{ 'id' => 120, 'si_name' => 'mother_id'  , 'scalar_data_type' =>  9, },
			{ 'id' => 121, 'si_name' => 'mother_name', 'scalar_data_type' => 24, },
		) ),
	] } );

	$model.build_child_node_tree( { 'NODE_TYPE' => 'row_data_type', 
			'ATTRS' => { 'id' => 301, 'si_name' => 'user_auth', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'row_data_type_field', 'ATTRS' => $_ } } (
			{ 'id' => 201, 'si_name' => 'user_id'      , 'scalar_data_type' =>  9, },
			{ 'id' => 202, 'si_name' => 'login_name'   , 'scalar_data_type' => 23, },
			{ 'id' => 203, 'si_name' => 'login_pass'   , 'scalar_data_type' => 23, },
			{ 'id' => 204, 'si_name' => 'private_name' , 'scalar_data_type' => 24, },
			{ 'id' => 205, 'si_name' => 'private_email', 'scalar_data_type' => 24, },
			{ 'id' => 206, 'si_name' => 'may_login'    , 'scalar_data_type' => 19, },
			{ 'id' => 207, 'si_name' => 'max_sessions' , 'scalar_data_type' =>  7, },
		) ),
	] } );

	$model.build_child_node_tree( { 'NODE_TYPE' => 'row_data_type', 
			'ATTRS' => { 'id' => 302, 'si_name' => 'user_profile', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'row_data_type_field', 'ATTRS' => $_ } } (
			{ 'id' => 208, 'si_name' => 'user_id'     , 'scalar_data_type' =>  9, },
			{ 'id' => 209, 'si_name' => 'public_name' , 'scalar_data_type' => 25, },
			{ 'id' => 210, 'si_name' => 'public_email', 'scalar_data_type' => 25, },
			{ 'id' => 211, 'si_name' => 'web_url'     , 'scalar_data_type' => 25, },
			{ 'id' => 212, 'si_name' => 'contact_net' , 'scalar_data_type' => 25, },
			{ 'id' => 213, 'si_name' => 'contact_phy' , 'scalar_data_type' => 25, },
			{ 'id' => 214, 'si_name' => 'bio'         , 'scalar_data_type' => 25, },
			{ 'id' => 215, 'si_name' => 'plan'        , 'scalar_data_type' => 25, },
			{ 'id' => 216, 'si_name' => 'comments'    , 'scalar_data_type' => 25, },
		) ),
	] } );

	$model.build_child_node_tree( { 'NODE_TYPE' => 'row_data_type', 
			'ATTRS' => { 'id' => 311, 'si_name' => 'user', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'row_data_type_field', 'ATTRS' => $_ } } (
			{ 'id' => 101, 'si_name' => 'user_id'      , 'scalar_data_type' =>  9, },
			{ 'id' => 102, 'si_name' => 'login_name'   , 'scalar_data_type' => 23, },
			{ 'id' => 103, 'si_name' => 'login_pass'   , 'scalar_data_type' => 23, },
			{ 'id' => 104, 'si_name' => 'private_name' , 'scalar_data_type' => 24, },
			{ 'id' => 105, 'si_name' => 'private_email', 'scalar_data_type' => 24, },
			{ 'id' => 106, 'si_name' => 'may_login'    , 'scalar_data_type' => 19, },
			{ 'id' => 107, 'si_name' => 'max_sessions' , 'scalar_data_type' =>  7, },
			{ 'id' => 108, 'si_name' => 'public_name'  , 'scalar_data_type' => 25, },
			{ 'id' => 109, 'si_name' => 'public_email' , 'scalar_data_type' => 25, },
			{ 'id' => 110, 'si_name' => 'web_url'      , 'scalar_data_type' => 25, },
			{ 'id' => 111, 'si_name' => 'contact_net'  , 'scalar_data_type' => 25, },
			{ 'id' => 112, 'si_name' => 'contact_phy'  , 'scalar_data_type' => 25, },
			{ 'id' => 113, 'si_name' => 'bio'          , 'scalar_data_type' => 25, },
			{ 'id' => 114, 'si_name' => 'plan'         , 'scalar_data_type' => 25, },
			{ 'id' => 115, 'si_name' => 'comments'     , 'scalar_data_type' => 25, },
		) ),
	] } );

	$model.build_child_node_tree( { 'NODE_TYPE' => 'row_data_type', 
			'ATTRS' => { 'id' => 303, 'si_name' => 'user_pref', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'row_data_type_field', 'ATTRS' => $_ } } (
			{ 'id' => 217, 'si_name' => 'user_id'   , 'scalar_data_type' =>  9, },
			{ 'id' => 218, 'si_name' => 'pref_name' , 'scalar_data_type' => 26, },
			{ 'id' => 219, 'si_name' => 'pref_value', 'scalar_data_type' => 27, },
		) ),
	] } );

	##### NEXT SET APPLICATION ELEMENT-TYPE DETAILS #####

	$model.build_child_node_tree( { 'NODE_TYPE' => 'row_data_type', 
			'ATTRS' => { 'id' => 313, 'si_name' => 'user_theme',  }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'row_data_type_field', 'ATTRS' => $_ } } (
			{ 'id' => 122, 'si_name' => 'theme_name' , 'scalar_data_type' => 27, },
			{ 'id' => 123, 'si_name' => 'theme_count', 'scalar_data_type' =>  9, },
		) ),
	] } );

	##### NEXT SET CATALOG BLUEPRINT-TYPE DETAILS #####

	my $catalog = $model.build_child_node_tree( 
		{ 'NODE_TYPE' => 'catalog', 'ATTRS' => { 'id' => 31, 'si_name' => 'The Catalog Blueprint' }, 
		'CHILDREN' => [ { 'NODE_TYPE' => 'owner', 'ATTRS' => { 'id' =>  32,  'si_name' => 'Gene\'s Owner', } } ] } ); 

	my $schema = $catalog.build_child_node_tree( { 'NODE_TYPE' => 'schema', 
		'ATTRS' => { 'id' => 33, 'si_name' => 'gene', 'owner' => 32, } } ); 

	$schema.build_child_node_tree( { 'NODE_TYPE' => 'table', 
			'ATTRS' => { 'id' => 44, 'si_name' => 'person', 'row_data_type' => 304, }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'table_field', 'ATTRS' => $_ } } (
			{ 'id' => 520, 'si_row_field' => 220, 'mandatory' => 1, 'default_val' => 1, 'auto_inc' => 1, },
			{ 'id' => 522, 'si_row_field' => 222, 'mandatory' => 1, },
		) ),
		( map { { 'NODE_TYPE' => 'table_index', 'ATTRS' => $_[0], 
				'CHILDREN' => { 'NODE_TYPE' => 'table_index_field', 'ATTRS' => $_[1] } } } (
			[ { 'id' => 701, 'si_name' => 'primary'        , 'index_type' => 'UNIQUE', }, { 'id' => 702, 'si_field' => 220, }, ], 
			[ { 'id' => 703, 'si_name' => 'ak_alternate_id', 'index_type' => 'UNIQUE', }, { 'id' => 704, 'si_field' => 221, }, ], 
			[ { 'id' => 705, 'si_name' => 'fk_father', 'index_type' => 'FOREIGN', 'f_table' => 44, }, 
				{ 'id' => 706, 'si_field' => 224, 'f_field' => 220 }, ], 
			[ { 'id' => 707, 'si_name' => 'fk_mother', 'index_type' => 'FOREIGN', 'f_table' => 44, }, 
				{ 'id' => 708, 'si_field' => 225, 'f_field' => 220 }, ], 
		) ),
	] } );

	$schema.build_child_node_tree( { 'NODE_TYPE' => 'view', 'ATTRS' => { 'id' => 52, 
			'si_name' => 'person_with_parents', 'view_type' => 'JOINED', 'row_data_type' => 312, }, 'CHILDREN' => [ 
		{ 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'id' => 73, 'si_name' => 'self'  , 
				'match' => 44, }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 417, 'si_match_field' => 220, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 418, 'si_match_field' => 222, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 425, 'si_match_field' => 224, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 426, 'si_match_field' => 225, }, },
		] },
		{ 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'id' => 74, 'si_name' => 'father', 
				'match' => 44, }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 419, 'si_match_field' => 220, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 420, 'si_match_field' => 222, }, },
		] },
		{ 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'id' => 75, 'si_name' => 'mother', 
				'match' => 44, }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 421, 'si_match_field' => 220, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 422, 'si_match_field' => 222, }, },
		] },
		( map { { 'NODE_TYPE' => 'view_field', 'ATTRS' => $_ } } (
			{ 'id' => 616, 'si_row_field' => 116, 'src_field' => 417, },
			{ 'id' => 617, 'si_row_field' => 117, 'src_field' => 418, },
			{ 'id' => 618, 'si_row_field' => 118, 'src_field' => 419, },
			{ 'id' => 619, 'si_row_field' => 119, 'src_field' => 420, },
			{ 'id' => 620, 'si_row_field' => 120, 'src_field' => 421, },
			{ 'id' => 621, 'si_row_field' => 121, 'src_field' => 422, },
		) ),
		{ 'NODE_TYPE' => 'view_join', 'ATTRS' => { 'id' => 731, 'lhs_src' => 73, 
				'rhs_src' => 74, 'join_op' => 'LEFT', }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_join_field', 'ATTRS' => { 'id' => 732, 'lhs_src_field' => 425, 'rhs_src_field' => 419, } },
		] },
		{ 'NODE_TYPE' => 'view_join', 'ATTRS' => { 'id' => 733, 'lhs_src' => 73, 
				'rhs_src' => 75, 'join_op' => 'LEFT', }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_join_field', 'ATTRS' => { 'id' => 734, 'lhs_src_field' => 426, 'rhs_src_field' => 421, } },
		] },
	] } );

	$schema.build_child_node_tree( { 'NODE_TYPE' => 'table', 
			'ATTRS' => { 'id' => 41, 'si_name' => 'user_auth', 'row_data_type' => 301, }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'table_field', 'ATTRS' => $_ } } (
			{ 'id' => 501, 'si_row_field' => 201, 'mandatory' => 1, 'default_val' => 1, 'auto_inc' => 1, },
			{ 'id' => 502, 'si_row_field' => 202, 'mandatory' => 1, },
			{ 'id' => 503, 'si_row_field' => 203, 'mandatory' => 1, },
			{ 'id' => 504, 'si_row_field' => 204, 'mandatory' => 1, },
			{ 'id' => 505, 'si_row_field' => 205, 'mandatory' => 1, },
			{ 'id' => 506, 'si_row_field' => 206, 'mandatory' => 1, },
			{ 'id' => 507, 'si_row_field' => 207, 'mandatory' => 1, 'default_val' => 3, },
		) ),
		( map { { 'NODE_TYPE' => 'table_index', 'ATTRS' => $_[0], 
				'CHILDREN' => { 'NODE_TYPE' => 'table_index_field', 'ATTRS' => $_[1] } } } (
			[ { 'id' => 709, 'si_name' => 'primary'         , 'index_type' => 'UNIQUE', }, { 'id' => 710, 'si_field' => 201, }, ], 
			[ { 'id' => 711, 'si_name' => 'ak_login_name'   , 'index_type' => 'UNIQUE', }, { 'id' => 712, 'si_field' => 202, }, ], 
			[ { 'id' => 713, 'si_name' => 'ak_private_email', 'index_type' => 'UNIQUE', }, { 'id' => 714, 'si_field' => 205, }, ], 
		) ),
	] } );

	$schema.build_child_node_tree( { 'NODE_TYPE' => 'table', 
			'ATTRS' => { 'id' => 42, 'si_name' => 'user_profile', 'row_data_type' => 302, }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'table_field', 'ATTRS' => $_ } } (
			{ 'id' => 508, 'si_row_field' => 208, 'mandatory' => 1, },
			{ 'id' => 509, 'si_row_field' => 209, 'mandatory' => 1, },
		) ),
		( map { { 'NODE_TYPE' => 'table_index', 'ATTRS' => $_[0], 
				'CHILDREN' => { 'NODE_TYPE' => 'table_index_field', 'ATTRS' => $_[1] } } } (
			[ { 'id' => 715, 'si_name' => 'primary'       , 'index_type' => 'UNIQUE', }, { 'id' => 716, 'si_field' => 208, }, ], 
			[ { 'id' => 717, 'si_name' => 'ak_public_name', 'index_type' => 'UNIQUE', }, { 'id' => 718, 'si_field' => 209, }, ], 
			[ { 'id' => 719, 'si_name' => 'fk_user'       , 'index_type' => 'FOREIGN', 'f_table' => 41, }, 
				{ 'id' => 720, 'si_field' => 208, 'f_field' => 201 }, ], 
		) ),
	] } );

	$schema.build_child_node_tree( { 'NODE_TYPE' => 'view', 'ATTRS' => { 'id' => 51, 
			'si_name' => 'user', 'view_type' => 'JOINED', 'row_data_type' => 311, }, 'CHILDREN' => [ 
		{ 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'id' => 71, 'si_name' => 'user_auth', 
				'match' => 41, }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 401, 'si_match_field' => 201, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 402, 'si_match_field' => 202, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 403, 'si_match_field' => 203, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 404, 'si_match_field' => 204, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 405, 'si_match_field' => 205, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 406, 'si_match_field' => 206, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 407, 'si_match_field' => 207, }, },
		] },
		{ 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'id' => 72, 'si_name' => 'user_profile', 
				'match' => 42, }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 408, 'si_match_field' => 208, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 409, 'si_match_field' => 209, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 410, 'si_match_field' => 210, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 411, 'si_match_field' => 211, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 412, 'si_match_field' => 212, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 413, 'si_match_field' => 213, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 414, 'si_match_field' => 214, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 415, 'si_match_field' => 215, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 416, 'si_match_field' => 216, }, },
		] },
		( map { { 'NODE_TYPE' => 'view_field', 'ATTRS' => $_ } } (
			{ 'id' => 601, 'si_row_field' => 101, 'src_field' => 401, },
			{ 'id' => 602, 'si_row_field' => 102, 'src_field' => 402, },
			{ 'id' => 603, 'si_row_field' => 103, 'src_field' => 403, },
			{ 'id' => 604, 'si_row_field' => 104, 'src_field' => 404, },
			{ 'id' => 605, 'si_row_field' => 105, 'src_field' => 405, },
			{ 'id' => 606, 'si_row_field' => 106, 'src_field' => 406, },
			{ 'id' => 607, 'si_row_field' => 107, 'src_field' => 407, },
			{ 'id' => 608, 'si_row_field' => 108, 'src_field' => 409, },
			{ 'id' => 609, 'si_row_field' => 109, 'src_field' => 410, },
			{ 'id' => 610, 'si_row_field' => 110, 'src_field' => 411, },
			{ 'id' => 611, 'si_row_field' => 111, 'src_field' => 412, },
			{ 'id' => 612, 'si_row_field' => 112, 'src_field' => 413, },
			{ 'id' => 613, 'si_row_field' => 113, 'src_field' => 414, },
			{ 'id' => 614, 'si_row_field' => 114, 'src_field' => 415, },
			{ 'id' => 615, 'si_row_field' => 115, 'src_field' => 416, },
		) ),
		{ 'NODE_TYPE' => 'view_join', 'ATTRS' => { 'id' => 735, 'lhs_src' => 71, 
				'rhs_src' => 72, 'join_op' => 'LEFT', }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_join_field', 'ATTRS' => { 'id' => 736, 'lhs_src_field' => 401, 'rhs_src_field' => 408, } },
		] },
	] } );

	$schema.build_child_node_tree( { 'NODE_TYPE' => 'table', 
			'ATTRS' => { 'id' => 43, 'si_name' => 'user_pref', 'row_data_type' => 303, }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'table_field', 'ATTRS' => $_ } } (
			{ 'id' => 517, 'si_row_field' => 217, 'mandatory' => 1, },
			{ 'id' => 518, 'si_row_field' => 218, 'mandatory' => 1, },
		) ),
		( map { { 'NODE_TYPE' => 'table_index', 'ATTRS' => $_[0], 'CHILDREN' => [ 
				map { { 'NODE_TYPE' => 'table_index_field', 'ATTRS' => $_ } } @{$_[1]}
				] } } (
			[ { 'id' => 721, 'si_name' => 'primary', 'index_type' => 'UNIQUE', }, 
				[ { 'id' => 722, 'si_field' => 217, }, { 'id' => 723, 'si_field' => 218, }, ], ], 
			[ { 'id' => 724, 'si_name' => 'fk_user', 'index_type' => 'FOREIGN', 'f_table' => 41, }, 
				[ { 'id' => 725, 'si_field' => 217, 'f_field' => 201 }, ], ], 
		) ),
	] } );

	##### NEXT SET APPLICATION BLUEPRINT-TYPE DETAILS #####

	my $application = $model.build_child_node_tree( { 'NODE_TYPE' => 'application', 
		'ATTRS' => { 'id' => 34, 'si_name' => 'My App', }, } ); 

	$application.build_child_node_tree( { 'NODE_TYPE' => 'view', 'ATTRS' => { 'id' => 53, 
			'si_name' => 'user_theme', 'view_type' => 'JOINED', 'row_data_type' => 313, }, 'CHILDREN' => [ 
		{ 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'id' => 76, 'si_name' => 'user_pref', 'match' => 43, }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 423, 'si_match_field' => 218, }, },
			{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 424, 'si_match_field' => 219, }, },
		] },
		{ 'NODE_TYPE' => 'view_field', 'ATTRS' => { 'id' => 842, 'si_row_field' => 122, 'src_field' => 424, }, },
		{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'RESULT', 
				'id' => 843, 'set_result_field' => 123, 'cont_type' => 'SCALAR', 'valf_call_sroutine' => 'COUNT', }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
				'id' => 844, 'cont_type' => 'SCALAR', 'valf_src_field' => 424, }, },
		] },
		{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'WHERE', 
				'id' => 811, 'cont_type' => 'SCALAR', 'valf_call_sroutine' => 'EQ', }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
				'id' => 812, 'cont_type' => 'SCALAR', 'valf_src_field' => 423, }, },
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
				'id' => 813, 'cont_type' => 'SCALAR', 'scalar_data_type' => 5, 'valf_literal' => 'theme', }, },
		] },
		{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'GROUP', 
			'id' => 814, 'cont_type' => 'SCALAR', 'valf_src_field' => 424, }, },
		{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'HAVING', 
				'id' => 815, 'cont_type' => 'SCALAR', 'valf_call_sroutine' => 'GT', }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
				'id' => 816, 'cont_type' => 'SCALAR', 'valf_call_sroutine' => 'COUNT', }, },
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
				'id' => 817, 'cont_type' => 'SCALAR', 'scalar_data_type' => 9, 'valf_literal' => '1', }, },
		] },
		{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'ORDER', 
			'id' => 855, 'cont_type' => 'SCALAR', 'valf_result_field' => 123, }, },
		{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'ORDER', 
			'id' => 856, 'cont_type' => 'SCALAR', 'valf_result_field' => 122, }, },
	] } );

	$application.build_child_node_tree( { 'NODE_TYPE' => 'routine', 
			'ATTRS' => { 'id' => 61, 'routine_type' => 'FUNCTION', 'si_name' => 'get_user', 
			'return_cont_type' => 'CURSOR', }, 'CHILDREN' => [ 
		{ 'NODE_TYPE' => 'routine_arg', 'ATTRS' => { 'id' => 91, 'si_name' => 'curr_uid', 
			'cont_type' => 'SCALAR', 'scalar_data_type' => 9, }, },
		{ 'NODE_TYPE' => 'view', 'ATTRS' => { 'id' => 55, 'si_name' => 'get_user', 
				'view_type' => 'JOINED', 'row_data_type' => 311, }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'id' => 78, 'si_name' => 'm', 'match' => 51, }, 
					'CHILDREN' => [ 
				{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 430, 'si_match_field' => 101, }, },
				{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 431, 'si_match_field' => 102, }, },
			] },
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'WHERE', 
					'id' => 801, 'cont_type' => 'SCALAR', 'valf_call_sroutine' => 'EQ', }, 'CHILDREN' => [ 
				{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
					'id' => 802, 'cont_type' => 'SCALAR', 'valf_src_field' => 430, }, },
				{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
					'id' => 803, 'cont_type' => 'SCALAR', 'valf_p_routine_item' => 91, }, },
			] },
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'ORDER', 
				'id' => 851, 'cont_type' => 'SCALAR', 'valf_src_field' => 431, }, },
		] },
		{ 'NODE_TYPE' => 'routine_stmt', 'ATTRS' => { 'id' => 94, 'call_sroutine' => 'CURSOR_OPEN' }, },
	] } );

	$application.build_child_node_tree( { 'NODE_TYPE' => 'routine', 
			'ATTRS' => { 'id' => 62, 'routine_type' => 'FUNCTION', 'si_name' => 'get_pwp', 
			'return_cont_type' => 'CURSOR', }, 'CHILDREN' => [ 
		{ 'NODE_TYPE' => 'routine_arg', 'ATTRS' => { 'id' => 92, 'si_name' => 'srchw_fa', 'cont_type' => 'SCALAR', 'scalar_data_type' => 5, }, },
		{ 'NODE_TYPE' => 'routine_arg', 'ATTRS' => { 'id' => 93, 'si_name' => 'srchw_mo', 'cont_type' => 'SCALAR', 'scalar_data_type' => 5, }, },
		{ 'NODE_TYPE' => 'view', 'ATTRS' => { 'id' => 56, 'si_name' => 'get_pwp', 
				'view_type' => 'JOINED', 'row_data_type' => 312, }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'id' => 79, 'si_name' => 'm', 'match' => 52, }, 
					'CHILDREN' => [ 
				{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 427, 'si_match_field' => 117, }, },
				{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 428, 'si_match_field' => 119, }, },
				{ 'NODE_TYPE' => 'view_src_field', 'ATTRS' => { 'id' => 429, 'si_match_field' => 121, }, },
			] },
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'WHERE', 
					'id' => 804, 'cont_type' => 'SCALAR', 'valf_call_sroutine' => 'AND', }, 'CHILDREN' => [ 
				{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
						'id' => 805, 'cont_type' => 'SCALAR', 'valf_call_sroutine' => 'LIKE', }, 'CHILDREN' => [ 
					{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
						'id' => 806, 'cont_type' => 'SCALAR', 'valf_src_field' => 428, }, },
					{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
						'id' => 807, 'cont_type' => 'SCALAR', 'valf_p_routine_item' => 92, }, },
				] },
				{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
						'id' => 808, 'cont_type' => 'SCALAR', 'valf_call_sroutine' => 'LIKE', }, 'CHILDREN' => [ 
					{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
						'id' => 809, 'cont_type' => 'SCALAR', 'valf_src_field' => 429, }, },
					{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
						'id' => 810, 'cont_type' => 'SCALAR', 'valf_p_routine_item' => 93, }, },
				] },
			] },
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'ORDER', 
				'id' => 852, 'cont_type' => 'SCALAR', 'valf_src_field' => 427, }, },
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'ORDER', 
				'id' => 853, 'cont_type' => 'SCALAR', 'valf_src_field' => 428, }, },
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'ORDER', 
				'id' => 854, 'cont_type' => 'SCALAR', 'valf_src_field' => 429, }, },
		] },
		{ 'NODE_TYPE' => 'routine_stmt', 'ATTRS' => { 'id' => 95, 'call_sroutine' => 'CURSOR_OPEN' }, },
	] } );

	$application.build_child_node_tree( { 'NODE_TYPE' => 'routine', 
			'ATTRS' => { 'id' => 63, 'routine_type' => 'FUNCTION', 'si_name' => 'get_theme', 
			'return_cont_type' => 'CURSOR', }, 'CHILDREN' => [ 
		{ 'NODE_TYPE' => 'view', 'ATTRS' => { 'id' => 57, 'si_name' => 'get_theme', 
				'view_type' => 'ALIAS', 'row_data_type' => 313, }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'id' => 69, 'si_name' => 'm', 'match' => 53, }, },
		] },
		{ 'NODE_TYPE' => 'routine_stmt', 'ATTRS' => { 'id' => 96, 'call_sroutine' => 'CURSOR_OPEN' }, },
	] } );

	$application.build_child_node_tree( { 'NODE_TYPE' => 'routine', 
			'ATTRS' => { 'id' => 64, 'routine_type' => 'FUNCTION', 'si_name' => 'get_person', 
			'return_cont_type' => 'CURSOR', }, 'CHILDREN' => [ 
		{ 'NODE_TYPE' => 'view', 'ATTRS' => { 'id' => 54, 'si_name' => 'get_person', 
				'view_type' => 'ALIAS', 'row_data_type' => 304, }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'id' => 77, 'si_name' => 'person', 'match' => 44, }, },
		] },
		{ 'NODE_TYPE' => 'routine_stmt', 'ATTRS' => { 'id' => 97, 'call_sroutine' => 'CURSOR_OPEN' }, },
	] } );

	##### NEXT SET PRODUCT-TYPE DETAILS #####

	# ... TODO ...

	##### NEXT SET INSTANCE-TYPE DETAILS #####

	my $app_inst = $model.build_child_node_tree( { 'NODE_TYPE' => 'application_instance', 
		'ATTRS' => { 'id' => 35, 'si_name' => 'My App Instance', 'blueprint' => 34, }, } ); 

	##### END OF DETAILS SETTING #####
}

######################################################################

sub expected_model_nid_xml_output() returns Str {
	return
'<?xml version="1.0" encoding="UTF-8"?>
<root>
	<elements>
		<scalar_data_type id="1" si_name="bin1k" base_type="STR_BIT" max_octets="1000" />
		<scalar_data_type id="2" si_name="bin32k" base_type="STR_BIT" max_octets="32000" />
		<scalar_data_type id="3" si_name="str4" base_type="STR_CHAR" max_chars="4" store_fixed="1" char_enc="ASCII" trim_white="1" uc_latin="1" pad_char=" " trim_pad="1" />
		<scalar_data_type id="4" si_name="str10" base_type="STR_CHAR" max_chars="10" store_fixed="1" char_enc="ASCII" trim_white="1" pad_char=" " trim_pad="1" />
		<scalar_data_type id="5" si_name="str30" base_type="STR_CHAR" max_chars="30" char_enc="ASCII" trim_white="1" />
		<scalar_data_type id="6" si_name="str2k" base_type="STR_CHAR" max_chars="2000" char_enc="UTF8" />
		<scalar_data_type id="7" si_name="byte" base_type="NUM_INT" num_precision="3" />
		<scalar_data_type id="8" si_name="short" base_type="NUM_INT" num_precision="5" />
		<scalar_data_type id="9" si_name="int" base_type="NUM_INT" num_precision="10" />
		<scalar_data_type id="10" si_name="long" base_type="NUM_INT" num_precision="19" />
		<scalar_data_type id="11" si_name="ubyte" base_type="NUM_INT" num_precision="3" num_unsigned="1" />
		<scalar_data_type id="12" si_name="ushort" base_type="NUM_INT" num_precision="5" num_unsigned="1" />
		<scalar_data_type id="13" si_name="uint" base_type="NUM_INT" num_precision="10" num_unsigned="1" />
		<scalar_data_type id="14" si_name="ulong" base_type="NUM_INT" num_precision="19" num_unsigned="1" />
		<scalar_data_type id="15" si_name="float" base_type="NUM_APR" num_octets="4" />
		<scalar_data_type id="16" si_name="double" base_type="NUM_APR" num_octets="8" />
		<scalar_data_type id="17" si_name="dec10p2" base_type="NUM_EXA" num_precision="10" num_scale="2" />
		<scalar_data_type id="18" si_name="dec255" base_type="NUM_EXA" num_precision="255" />
		<scalar_data_type id="19" si_name="boolean" base_type="BOOLEAN" />
		<scalar_data_type id="20" si_name="datetime" base_type="DATM_FULL" calendar="ABS" />
		<scalar_data_type id="21" si_name="dtchines" base_type="DATM_FULL" calendar="CHI" />
		<scalar_data_type id="22" si_name="sex" base_type="STR_CHAR" max_chars="1" char_enc="ASCII">
			<scalar_data_type_opt id="28" si_value="M" />
			<scalar_data_type_opt id="29" si_value="F" />
		</scalar_data_type>
		<scalar_data_type id="23" si_name="str20" base_type="STR_CHAR" max_chars="20" char_enc="ASCII" />
		<scalar_data_type id="24" si_name="str100" base_type="STR_CHAR" max_chars="100" char_enc="ASCII" />
		<scalar_data_type id="25" si_name="str250" base_type="STR_CHAR" max_chars="250" char_enc="ASCII" />
		<scalar_data_type id="26" si_name="entitynm" base_type="STR_CHAR" max_chars="30" char_enc="ASCII" />
		<scalar_data_type id="27" si_name="generic" base_type="STR_CHAR" max_chars="250" char_enc="ASCII" />
		<row_data_type id="304" si_name="person">
			<row_data_type_field id="220" si_name="person_id" scalar_data_type="9" />
			<row_data_type_field id="221" si_name="alternate_id" scalar_data_type="23" />
			<row_data_type_field id="222" si_name="name" scalar_data_type="24" />
			<row_data_type_field id="223" si_name="sex" scalar_data_type="22" />
			<row_data_type_field id="224" si_name="father_id" scalar_data_type="9" />
			<row_data_type_field id="225" si_name="mother_id" scalar_data_type="9" />
		</row_data_type>
		<row_data_type id="312" si_name="person_with_parents">
			<row_data_type_field id="116" si_name="self_id" scalar_data_type="9" />
			<row_data_type_field id="117" si_name="self_name" scalar_data_type="24" />
			<row_data_type_field id="118" si_name="father_id" scalar_data_type="9" />
			<row_data_type_field id="119" si_name="father_name" scalar_data_type="24" />
			<row_data_type_field id="120" si_name="mother_id" scalar_data_type="9" />
			<row_data_type_field id="121" si_name="mother_name" scalar_data_type="24" />
		</row_data_type>
		<row_data_type id="301" si_name="user_auth">
			<row_data_type_field id="201" si_name="user_id" scalar_data_type="9" />
			<row_data_type_field id="202" si_name="login_name" scalar_data_type="23" />
			<row_data_type_field id="203" si_name="login_pass" scalar_data_type="23" />
			<row_data_type_field id="204" si_name="private_name" scalar_data_type="24" />
			<row_data_type_field id="205" si_name="private_email" scalar_data_type="24" />
			<row_data_type_field id="206" si_name="may_login" scalar_data_type="19" />
			<row_data_type_field id="207" si_name="max_sessions" scalar_data_type="7" />
		</row_data_type>
		<row_data_type id="302" si_name="user_profile">
			<row_data_type_field id="208" si_name="user_id" scalar_data_type="9" />
			<row_data_type_field id="209" si_name="public_name" scalar_data_type="25" />
			<row_data_type_field id="210" si_name="public_email" scalar_data_type="25" />
			<row_data_type_field id="211" si_name="web_url" scalar_data_type="25" />
			<row_data_type_field id="212" si_name="contact_net" scalar_data_type="25" />
			<row_data_type_field id="213" si_name="contact_phy" scalar_data_type="25" />
			<row_data_type_field id="214" si_name="bio" scalar_data_type="25" />
			<row_data_type_field id="215" si_name="plan" scalar_data_type="25" />
			<row_data_type_field id="216" si_name="comments" scalar_data_type="25" />
		</row_data_type>
		<row_data_type id="311" si_name="user">
			<row_data_type_field id="101" si_name="user_id" scalar_data_type="9" />
			<row_data_type_field id="102" si_name="login_name" scalar_data_type="23" />
			<row_data_type_field id="103" si_name="login_pass" scalar_data_type="23" />
			<row_data_type_field id="104" si_name="private_name" scalar_data_type="24" />
			<row_data_type_field id="105" si_name="private_email" scalar_data_type="24" />
			<row_data_type_field id="106" si_name="may_login" scalar_data_type="19" />
			<row_data_type_field id="107" si_name="max_sessions" scalar_data_type="7" />
			<row_data_type_field id="108" si_name="public_name" scalar_data_type="25" />
			<row_data_type_field id="109" si_name="public_email" scalar_data_type="25" />
			<row_data_type_field id="110" si_name="web_url" scalar_data_type="25" />
			<row_data_type_field id="111" si_name="contact_net" scalar_data_type="25" />
			<row_data_type_field id="112" si_name="contact_phy" scalar_data_type="25" />
			<row_data_type_field id="113" si_name="bio" scalar_data_type="25" />
			<row_data_type_field id="114" si_name="plan" scalar_data_type="25" />
			<row_data_type_field id="115" si_name="comments" scalar_data_type="25" />
		</row_data_type>
		<row_data_type id="303" si_name="user_pref">
			<row_data_type_field id="217" si_name="user_id" scalar_data_type="9" />
			<row_data_type_field id="218" si_name="pref_name" scalar_data_type="26" />
			<row_data_type_field id="219" si_name="pref_value" scalar_data_type="27" />
		</row_data_type>
		<row_data_type id="313" si_name="user_theme">
			<row_data_type_field id="122" si_name="theme_name" scalar_data_type="27" />
			<row_data_type_field id="123" si_name="theme_count" scalar_data_type="9" />
		</row_data_type>
	</elements>
	<blueprints>
		<catalog id="31" si_name="The Catalog Blueprint">
			<owner id="32" si_name="Gene\'s Owner" />
			<schema id="33" si_name="gene" owner="32">
				<table id="44" si_name="person" row_data_type="304">
					<table_field id="520" si_row_field="220" mandatory="1" default_val="1" auto_inc="1" />
					<table_field id="522" si_row_field="222" mandatory="1" />
					<table_index id="701" si_name="primary" index_type="UNIQUE">
						<table_index_field id="702" si_field="220" />
					</table_index>
					<table_index id="703" si_name="ak_alternate_id" index_type="UNIQUE">
						<table_index_field id="704" si_field="221" />
					</table_index>
					<table_index id="705" si_name="fk_father" index_type="FOREIGN" f_table="44">
						<table_index_field id="706" si_field="224" f_field="220" />
					</table_index>
					<table_index id="707" si_name="fk_mother" index_type="FOREIGN" f_table="44">
						<table_index_field id="708" si_field="225" f_field="220" />
					</table_index>
				</table>
				<view id="52" si_name="person_with_parents" view_type="JOINED" row_data_type="312">
					<view_src id="73" si_name="self" match="44">
						<view_src_field id="417" si_match_field="220" />
						<view_src_field id="418" si_match_field="222" />
						<view_src_field id="425" si_match_field="224" />
						<view_src_field id="426" si_match_field="225" />
					</view_src>
					<view_src id="74" si_name="father" match="44">
						<view_src_field id="419" si_match_field="220" />
						<view_src_field id="420" si_match_field="222" />
					</view_src>
					<view_src id="75" si_name="mother" match="44">
						<view_src_field id="421" si_match_field="220" />
						<view_src_field id="422" si_match_field="222" />
					</view_src>
					<view_field id="616" si_row_field="116" src_field="417" />
					<view_field id="617" si_row_field="117" src_field="418" />
					<view_field id="618" si_row_field="118" src_field="419" />
					<view_field id="619" si_row_field="119" src_field="420" />
					<view_field id="620" si_row_field="120" src_field="421" />
					<view_field id="621" si_row_field="121" src_field="422" />
					<view_join id="731" lhs_src="73" rhs_src="74" join_op="LEFT">
						<view_join_field id="732" lhs_src_field="425" rhs_src_field="419" />
					</view_join>
					<view_join id="733" lhs_src="73" rhs_src="75" join_op="LEFT">
						<view_join_field id="734" lhs_src_field="426" rhs_src_field="421" />
					</view_join>
				</view>
				<table id="41" si_name="user_auth" row_data_type="301">
					<table_field id="501" si_row_field="201" mandatory="1" default_val="1" auto_inc="1" />
					<table_field id="502" si_row_field="202" mandatory="1" />
					<table_field id="503" si_row_field="203" mandatory="1" />
					<table_field id="504" si_row_field="204" mandatory="1" />
					<table_field id="505" si_row_field="205" mandatory="1" />
					<table_field id="506" si_row_field="206" mandatory="1" />
					<table_field id="507" si_row_field="207" mandatory="1" default_val="3" />
					<table_index id="709" si_name="primary" index_type="UNIQUE">
						<table_index_field id="710" si_field="201" />
					</table_index>
					<table_index id="711" si_name="ak_login_name" index_type="UNIQUE">
						<table_index_field id="712" si_field="202" />
					</table_index>
					<table_index id="713" si_name="ak_private_email" index_type="UNIQUE">
						<table_index_field id="714" si_field="205" />
					</table_index>
				</table>
				<table id="42" si_name="user_profile" row_data_type="302">
					<table_field id="508" si_row_field="208" mandatory="1" />
					<table_field id="509" si_row_field="209" mandatory="1" />
					<table_index id="715" si_name="primary" index_type="UNIQUE">
						<table_index_field id="716" si_field="208" />
					</table_index>
					<table_index id="717" si_name="ak_public_name" index_type="UNIQUE">
						<table_index_field id="718" si_field="209" />
					</table_index>
					<table_index id="719" si_name="fk_user" index_type="FOREIGN" f_table="41">
						<table_index_field id="720" si_field="208" f_field="201" />
					</table_index>
				</table>
				<view id="51" si_name="user" view_type="JOINED" row_data_type="311">
					<view_src id="71" si_name="user_auth" match="41">
						<view_src_field id="401" si_match_field="201" />
						<view_src_field id="402" si_match_field="202" />
						<view_src_field id="403" si_match_field="203" />
						<view_src_field id="404" si_match_field="204" />
						<view_src_field id="405" si_match_field="205" />
						<view_src_field id="406" si_match_field="206" />
						<view_src_field id="407" si_match_field="207" />
					</view_src>
					<view_src id="72" si_name="user_profile" match="42">
						<view_src_field id="408" si_match_field="208" />
						<view_src_field id="409" si_match_field="209" />
						<view_src_field id="410" si_match_field="210" />
						<view_src_field id="411" si_match_field="211" />
						<view_src_field id="412" si_match_field="212" />
						<view_src_field id="413" si_match_field="213" />
						<view_src_field id="414" si_match_field="214" />
						<view_src_field id="415" si_match_field="215" />
						<view_src_field id="416" si_match_field="216" />
					</view_src>
					<view_field id="601" si_row_field="101" src_field="401" />
					<view_field id="602" si_row_field="102" src_field="402" />
					<view_field id="603" si_row_field="103" src_field="403" />
					<view_field id="604" si_row_field="104" src_field="404" />
					<view_field id="605" si_row_field="105" src_field="405" />
					<view_field id="606" si_row_field="106" src_field="406" />
					<view_field id="607" si_row_field="107" src_field="407" />
					<view_field id="608" si_row_field="108" src_field="409" />
					<view_field id="609" si_row_field="109" src_field="410" />
					<view_field id="610" si_row_field="110" src_field="411" />
					<view_field id="611" si_row_field="111" src_field="412" />
					<view_field id="612" si_row_field="112" src_field="413" />
					<view_field id="613" si_row_field="113" src_field="414" />
					<view_field id="614" si_row_field="114" src_field="415" />
					<view_field id="615" si_row_field="115" src_field="416" />
					<view_join id="735" lhs_src="71" rhs_src="72" join_op="LEFT">
						<view_join_field id="736" lhs_src_field="401" rhs_src_field="408" />
					</view_join>
				</view>
				<table id="43" si_name="user_pref" row_data_type="303">
					<table_field id="517" si_row_field="217" mandatory="1" />
					<table_field id="518" si_row_field="218" mandatory="1" />
					<table_index id="721" si_name="primary" index_type="UNIQUE">
						<table_index_field id="722" si_field="217" />
						<table_index_field id="723" si_field="218" />
					</table_index>
					<table_index id="724" si_name="fk_user" index_type="FOREIGN" f_table="41">
						<table_index_field id="725" si_field="217" f_field="201" />
					</table_index>
				</table>
			</schema>
		</catalog>
		<application id="34" si_name="My App">
			<view id="53" si_name="user_theme" view_type="JOINED" row_data_type="313">
				<view_src id="76" si_name="user_pref" match="43">
					<view_src_field id="423" si_match_field="218" />
					<view_src_field id="424" si_match_field="219" />
				</view_src>
				<view_field id="842" si_row_field="122" src_field="424" />
				<view_expr id="843" view_part="RESULT" set_result_field="123" cont_type="SCALAR" valf_call_sroutine="COUNT">
					<view_expr id="844" cont_type="SCALAR" valf_src_field="424" />
				</view_expr>
				<view_expr id="811" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="EQ">
					<view_expr id="812" cont_type="SCALAR" valf_src_field="423" />
					<view_expr id="813" cont_type="SCALAR" valf_literal="theme" scalar_data_type="5" />
				</view_expr>
				<view_expr id="814" view_part="GROUP" cont_type="SCALAR" valf_src_field="424" />
				<view_expr id="815" view_part="HAVING" cont_type="SCALAR" valf_call_sroutine="GT">
					<view_expr id="816" cont_type="SCALAR" valf_call_sroutine="COUNT" />
					<view_expr id="817" cont_type="SCALAR" valf_literal="1" scalar_data_type="9" />
				</view_expr>
				<view_expr id="855" view_part="ORDER" cont_type="SCALAR" valf_result_field="123" />
				<view_expr id="856" view_part="ORDER" cont_type="SCALAR" valf_result_field="122" />
			</view>
			<routine id="61" si_name="get_user" routine_type="FUNCTION" return_cont_type="CURSOR">
				<routine_arg id="91" si_name="curr_uid" cont_type="SCALAR" scalar_data_type="9" />
				<view id="55" si_name="get_user" view_type="JOINED" row_data_type="311">
					<view_src id="78" si_name="m" match="51">
						<view_src_field id="430" si_match_field="101" />
						<view_src_field id="431" si_match_field="102" />
					</view_src>
					<view_expr id="801" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="EQ">
						<view_expr id="802" cont_type="SCALAR" valf_src_field="430" />
						<view_expr id="803" cont_type="SCALAR" valf_p_routine_item="91" />
					</view_expr>
					<view_expr id="851" view_part="ORDER" cont_type="SCALAR" valf_src_field="431" />
				</view>
				<routine_stmt id="94" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="62" si_name="get_pwp" routine_type="FUNCTION" return_cont_type="CURSOR">
				<routine_arg id="92" si_name="srchw_fa" cont_type="SCALAR" scalar_data_type="5" />
				<routine_arg id="93" si_name="srchw_mo" cont_type="SCALAR" scalar_data_type="5" />
				<view id="56" si_name="get_pwp" view_type="JOINED" row_data_type="312">
					<view_src id="79" si_name="m" match="52">
						<view_src_field id="427" si_match_field="117" />
						<view_src_field id="428" si_match_field="119" />
						<view_src_field id="429" si_match_field="121" />
					</view_src>
					<view_expr id="804" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="AND">
						<view_expr id="805" cont_type="SCALAR" valf_call_sroutine="LIKE">
							<view_expr id="806" cont_type="SCALAR" valf_src_field="428" />
							<view_expr id="807" cont_type="SCALAR" valf_p_routine_item="92" />
						</view_expr>
						<view_expr id="808" cont_type="SCALAR" valf_call_sroutine="LIKE">
							<view_expr id="809" cont_type="SCALAR" valf_src_field="429" />
							<view_expr id="810" cont_type="SCALAR" valf_p_routine_item="93" />
						</view_expr>
					</view_expr>
					<view_expr id="852" view_part="ORDER" cont_type="SCALAR" valf_src_field="427" />
					<view_expr id="853" view_part="ORDER" cont_type="SCALAR" valf_src_field="428" />
					<view_expr id="854" view_part="ORDER" cont_type="SCALAR" valf_src_field="429" />
				</view>
				<routine_stmt id="95" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="63" si_name="get_theme" routine_type="FUNCTION" return_cont_type="CURSOR">
				<view id="57" si_name="get_theme" view_type="ALIAS" row_data_type="313">
					<view_src id="69" si_name="m" match="53" />
				</view>
				<routine_stmt id="96" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="64" si_name="get_person" routine_type="FUNCTION" return_cont_type="CURSOR">
				<view id="54" si_name="get_person" view_type="ALIAS" row_data_type="304">
					<view_src id="77" si_name="person" match="44" />
				</view>
				<routine_stmt id="97" call_sroutine="CURSOR_OPEN" />
			</routine>
		</application>
	</blueprints>
	<tools />
	<sites>
		<application_instance id="35" si_name="My App Instance" blueprint="34" />
	</sites>
	<circumventions />
</root>
'
	;
}

######################################################################

sub expected_model_sid_long_xml_output() returns Str {
	return
'<?xml version="1.0" encoding="UTF-8"?>
<root>
	<elements>
		<scalar_data_type id="1" si_name="bin1k" base_type="STR_BIT" max_octets="1000" />
		<scalar_data_type id="2" si_name="bin32k" base_type="STR_BIT" max_octets="32000" />
		<scalar_data_type id="3" si_name="str4" base_type="STR_CHAR" max_chars="4" store_fixed="1" char_enc="ASCII" trim_white="1" uc_latin="1" pad_char=" " trim_pad="1" />
		<scalar_data_type id="4" si_name="str10" base_type="STR_CHAR" max_chars="10" store_fixed="1" char_enc="ASCII" trim_white="1" pad_char=" " trim_pad="1" />
		<scalar_data_type id="5" si_name="str30" base_type="STR_CHAR" max_chars="30" char_enc="ASCII" trim_white="1" />
		<scalar_data_type id="6" si_name="str2k" base_type="STR_CHAR" max_chars="2000" char_enc="UTF8" />
		<scalar_data_type id="7" si_name="byte" base_type="NUM_INT" num_precision="3" />
		<scalar_data_type id="8" si_name="short" base_type="NUM_INT" num_precision="5" />
		<scalar_data_type id="9" si_name="int" base_type="NUM_INT" num_precision="10" />
		<scalar_data_type id="10" si_name="long" base_type="NUM_INT" num_precision="19" />
		<scalar_data_type id="11" si_name="ubyte" base_type="NUM_INT" num_precision="3" num_unsigned="1" />
		<scalar_data_type id="12" si_name="ushort" base_type="NUM_INT" num_precision="5" num_unsigned="1" />
		<scalar_data_type id="13" si_name="uint" base_type="NUM_INT" num_precision="10" num_unsigned="1" />
		<scalar_data_type id="14" si_name="ulong" base_type="NUM_INT" num_precision="19" num_unsigned="1" />
		<scalar_data_type id="15" si_name="float" base_type="NUM_APR" num_octets="4" />
		<scalar_data_type id="16" si_name="double" base_type="NUM_APR" num_octets="8" />
		<scalar_data_type id="17" si_name="dec10p2" base_type="NUM_EXA" num_precision="10" num_scale="2" />
		<scalar_data_type id="18" si_name="dec255" base_type="NUM_EXA" num_precision="255" />
		<scalar_data_type id="19" si_name="boolean" base_type="BOOLEAN" />
		<scalar_data_type id="20" si_name="datetime" base_type="DATM_FULL" calendar="ABS" />
		<scalar_data_type id="21" si_name="dtchines" base_type="DATM_FULL" calendar="CHI" />
		<scalar_data_type id="22" si_name="sex" base_type="STR_CHAR" max_chars="1" char_enc="ASCII">
			<scalar_data_type_opt id="28" si_value="M" />
			<scalar_data_type_opt id="29" si_value="F" />
		</scalar_data_type>
		<scalar_data_type id="23" si_name="str20" base_type="STR_CHAR" max_chars="20" char_enc="ASCII" />
		<scalar_data_type id="24" si_name="str100" base_type="STR_CHAR" max_chars="100" char_enc="ASCII" />
		<scalar_data_type id="25" si_name="str250" base_type="STR_CHAR" max_chars="250" char_enc="ASCII" />
		<scalar_data_type id="26" si_name="entitynm" base_type="STR_CHAR" max_chars="30" char_enc="ASCII" />
		<scalar_data_type id="27" si_name="generic" base_type="STR_CHAR" max_chars="250" char_enc="ASCII" />
		<row_data_type id="304" si_name="person">
			<row_data_type_field id="220" si_name="person_id" scalar_data_type="int" />
			<row_data_type_field id="221" si_name="alternate_id" scalar_data_type="str20" />
			<row_data_type_field id="222" si_name="name" scalar_data_type="str100" />
			<row_data_type_field id="223" si_name="sex" scalar_data_type="sex" />
			<row_data_type_field id="224" si_name="father_id" scalar_data_type="int" />
			<row_data_type_field id="225" si_name="mother_id" scalar_data_type="int" />
		</row_data_type>
		<row_data_type id="312" si_name="person_with_parents">
			<row_data_type_field id="116" si_name="self_id" scalar_data_type="int" />
			<row_data_type_field id="117" si_name="self_name" scalar_data_type="str100" />
			<row_data_type_field id="118" si_name="father_id" scalar_data_type="int" />
			<row_data_type_field id="119" si_name="father_name" scalar_data_type="str100" />
			<row_data_type_field id="120" si_name="mother_id" scalar_data_type="int" />
			<row_data_type_field id="121" si_name="mother_name" scalar_data_type="str100" />
		</row_data_type>
		<row_data_type id="301" si_name="user_auth">
			<row_data_type_field id="201" si_name="user_id" scalar_data_type="int" />
			<row_data_type_field id="202" si_name="login_name" scalar_data_type="str20" />
			<row_data_type_field id="203" si_name="login_pass" scalar_data_type="str20" />
			<row_data_type_field id="204" si_name="private_name" scalar_data_type="str100" />
			<row_data_type_field id="205" si_name="private_email" scalar_data_type="str100" />
			<row_data_type_field id="206" si_name="may_login" scalar_data_type="boolean" />
			<row_data_type_field id="207" si_name="max_sessions" scalar_data_type="byte" />
		</row_data_type>
		<row_data_type id="302" si_name="user_profile">
			<row_data_type_field id="208" si_name="user_id" scalar_data_type="int" />
			<row_data_type_field id="209" si_name="public_name" scalar_data_type="str250" />
			<row_data_type_field id="210" si_name="public_email" scalar_data_type="str250" />
			<row_data_type_field id="211" si_name="web_url" scalar_data_type="str250" />
			<row_data_type_field id="212" si_name="contact_net" scalar_data_type="str250" />
			<row_data_type_field id="213" si_name="contact_phy" scalar_data_type="str250" />
			<row_data_type_field id="214" si_name="bio" scalar_data_type="str250" />
			<row_data_type_field id="215" si_name="plan" scalar_data_type="str250" />
			<row_data_type_field id="216" si_name="comments" scalar_data_type="str250" />
		</row_data_type>
		<row_data_type id="311" si_name="user">
			<row_data_type_field id="101" si_name="user_id" scalar_data_type="int" />
			<row_data_type_field id="102" si_name="login_name" scalar_data_type="str20" />
			<row_data_type_field id="103" si_name="login_pass" scalar_data_type="str20" />
			<row_data_type_field id="104" si_name="private_name" scalar_data_type="str100" />
			<row_data_type_field id="105" si_name="private_email" scalar_data_type="str100" />
			<row_data_type_field id="106" si_name="may_login" scalar_data_type="boolean" />
			<row_data_type_field id="107" si_name="max_sessions" scalar_data_type="byte" />
			<row_data_type_field id="108" si_name="public_name" scalar_data_type="str250" />
			<row_data_type_field id="109" si_name="public_email" scalar_data_type="str250" />
			<row_data_type_field id="110" si_name="web_url" scalar_data_type="str250" />
			<row_data_type_field id="111" si_name="contact_net" scalar_data_type="str250" />
			<row_data_type_field id="112" si_name="contact_phy" scalar_data_type="str250" />
			<row_data_type_field id="113" si_name="bio" scalar_data_type="str250" />
			<row_data_type_field id="114" si_name="plan" scalar_data_type="str250" />
			<row_data_type_field id="115" si_name="comments" scalar_data_type="str250" />
		</row_data_type>
		<row_data_type id="303" si_name="user_pref">
			<row_data_type_field id="217" si_name="user_id" scalar_data_type="int" />
			<row_data_type_field id="218" si_name="pref_name" scalar_data_type="entitynm" />
			<row_data_type_field id="219" si_name="pref_value" scalar_data_type="generic" />
		</row_data_type>
		<row_data_type id="313" si_name="user_theme">
			<row_data_type_field id="122" si_name="theme_name" scalar_data_type="generic" />
			<row_data_type_field id="123" si_name="theme_count" scalar_data_type="int" />
		</row_data_type>
	</elements>
	<blueprints>
		<catalog id="31" si_name="The Catalog Blueprint">
			<owner id="32" si_name="Gene\'s Owner" />
			<schema id="33" si_name="gene" owner="Gene\'s Owner">
				<table id="44" si_name="person" row_data_type="person">
					<table_field id="520" si_row_field="person_id" mandatory="1" default_val="1" auto_inc="1" />
					<table_field id="522" si_row_field="name" mandatory="1" />
					<table_index id="701" si_name="primary" index_type="UNIQUE">
						<table_index_field id="702" si_field="person_id" />
					</table_index>
					<table_index id="703" si_name="ak_alternate_id" index_type="UNIQUE">
						<table_index_field id="704" si_field="alternate_id" />
					</table_index>
					<table_index id="705" si_name="fk_father" index_type="FOREIGN" f_table="person">
						<table_index_field id="706" si_field="father_id" f_field="person_id" />
					</table_index>
					<table_index id="707" si_name="fk_mother" index_type="FOREIGN" f_table="person">
						<table_index_field id="708" si_field="mother_id" f_field="person_id" />
					</table_index>
				</table>
				<view id="52" si_name="person_with_parents" view_type="JOINED" row_data_type="person_with_parents">
					<view_src id="73" si_name="self" match="person">
						<view_src_field id="417" si_match_field="person_id" />
						<view_src_field id="418" si_match_field="name" />
						<view_src_field id="425" si_match_field="father_id" />
						<view_src_field id="426" si_match_field="mother_id" />
					</view_src>
					<view_src id="74" si_name="father" match="person">
						<view_src_field id="419" si_match_field="person_id" />
						<view_src_field id="420" si_match_field="name" />
					</view_src>
					<view_src id="75" si_name="mother" match="person">
						<view_src_field id="421" si_match_field="person_id" />
						<view_src_field id="422" si_match_field="name" />
					</view_src>
					<view_field id="616" si_row_field="self_id" src_field="[person_id,self]" />
					<view_field id="617" si_row_field="self_name" src_field="[name,self]" />
					<view_field id="618" si_row_field="father_id" src_field="[person_id,father]" />
					<view_field id="619" si_row_field="father_name" src_field="[name,father]" />
					<view_field id="620" si_row_field="mother_id" src_field="[person_id,mother]" />
					<view_field id="621" si_row_field="mother_name" src_field="[name,mother]" />
					<view_join id="731" lhs_src="self" rhs_src="father" join_op="LEFT">
						<view_join_field id="732" lhs_src_field="father_id" rhs_src_field="person_id" />
					</view_join>
					<view_join id="733" lhs_src="self" rhs_src="mother" join_op="LEFT">
						<view_join_field id="734" lhs_src_field="mother_id" rhs_src_field="person_id" />
					</view_join>
				</view>
				<table id="41" si_name="user_auth" row_data_type="user_auth">
					<table_field id="501" si_row_field="user_id" mandatory="1" default_val="1" auto_inc="1" />
					<table_field id="502" si_row_field="login_name" mandatory="1" />
					<table_field id="503" si_row_field="login_pass" mandatory="1" />
					<table_field id="504" si_row_field="private_name" mandatory="1" />
					<table_field id="505" si_row_field="private_email" mandatory="1" />
					<table_field id="506" si_row_field="may_login" mandatory="1" />
					<table_field id="507" si_row_field="max_sessions" mandatory="1" default_val="3" />
					<table_index id="709" si_name="primary" index_type="UNIQUE">
						<table_index_field id="710" si_field="user_id" />
					</table_index>
					<table_index id="711" si_name="ak_login_name" index_type="UNIQUE">
						<table_index_field id="712" si_field="login_name" />
					</table_index>
					<table_index id="713" si_name="ak_private_email" index_type="UNIQUE">
						<table_index_field id="714" si_field="private_email" />
					</table_index>
				</table>
				<table id="42" si_name="user_profile" row_data_type="user_profile">
					<table_field id="508" si_row_field="user_id" mandatory="1" />
					<table_field id="509" si_row_field="public_name" mandatory="1" />
					<table_index id="715" si_name="primary" index_type="UNIQUE">
						<table_index_field id="716" si_field="user_id" />
					</table_index>
					<table_index id="717" si_name="ak_public_name" index_type="UNIQUE">
						<table_index_field id="718" si_field="public_name" />
					</table_index>
					<table_index id="719" si_name="fk_user" index_type="FOREIGN" f_table="user_auth">
						<table_index_field id="720" si_field="user_id" f_field="user_id" />
					</table_index>
				</table>
				<view id="51" si_name="user" view_type="JOINED" row_data_type="user">
					<view_src id="71" si_name="user_auth" match="user_auth">
						<view_src_field id="401" si_match_field="user_id" />
						<view_src_field id="402" si_match_field="login_name" />
						<view_src_field id="403" si_match_field="login_pass" />
						<view_src_field id="404" si_match_field="private_name" />
						<view_src_field id="405" si_match_field="private_email" />
						<view_src_field id="406" si_match_field="may_login" />
						<view_src_field id="407" si_match_field="max_sessions" />
					</view_src>
					<view_src id="72" si_name="user_profile" match="user_profile">
						<view_src_field id="408" si_match_field="user_id" />
						<view_src_field id="409" si_match_field="public_name" />
						<view_src_field id="410" si_match_field="public_email" />
						<view_src_field id="411" si_match_field="web_url" />
						<view_src_field id="412" si_match_field="contact_net" />
						<view_src_field id="413" si_match_field="contact_phy" />
						<view_src_field id="414" si_match_field="bio" />
						<view_src_field id="415" si_match_field="plan" />
						<view_src_field id="416" si_match_field="comments" />
					</view_src>
					<view_field id="601" si_row_field="user_id" src_field="[user_id,user_auth]" />
					<view_field id="602" si_row_field="login_name" src_field="[login_name,user_auth]" />
					<view_field id="603" si_row_field="login_pass" src_field="[login_pass,user_auth]" />
					<view_field id="604" si_row_field="private_name" src_field="[private_name,user_auth]" />
					<view_field id="605" si_row_field="private_email" src_field="[private_email,user_auth]" />
					<view_field id="606" si_row_field="may_login" src_field="[may_login,user_auth]" />
					<view_field id="607" si_row_field="max_sessions" src_field="[max_sessions,user_auth]" />
					<view_field id="608" si_row_field="public_name" src_field="[public_name,user_profile]" />
					<view_field id="609" si_row_field="public_email" src_field="[public_email,user_profile]" />
					<view_field id="610" si_row_field="web_url" src_field="[web_url,user_profile]" />
					<view_field id="611" si_row_field="contact_net" src_field="[contact_net,user_profile]" />
					<view_field id="612" si_row_field="contact_phy" src_field="[contact_phy,user_profile]" />
					<view_field id="613" si_row_field="bio" src_field="[bio,user_profile]" />
					<view_field id="614" si_row_field="plan" src_field="[plan,user_profile]" />
					<view_field id="615" si_row_field="comments" src_field="[comments,user_profile]" />
					<view_join id="735" lhs_src="user_auth" rhs_src="user_profile" join_op="LEFT">
						<view_join_field id="736" lhs_src_field="user_id" rhs_src_field="user_id" />
					</view_join>
				</view>
				<table id="43" si_name="user_pref" row_data_type="user_pref">
					<table_field id="517" si_row_field="user_id" mandatory="1" />
					<table_field id="518" si_row_field="pref_name" mandatory="1" />
					<table_index id="721" si_name="primary" index_type="UNIQUE">
						<table_index_field id="722" si_field="user_id" />
						<table_index_field id="723" si_field="pref_name" />
					</table_index>
					<table_index id="724" si_name="fk_user" index_type="FOREIGN" f_table="user_auth">
						<table_index_field id="725" si_field="user_id" f_field="user_id" />
					</table_index>
				</table>
			</schema>
		</catalog>
		<application id="34" si_name="My App">
			<view id="53" si_name="user_theme" view_type="JOINED" row_data_type="user_theme">
				<view_src id="76" si_name="user_pref" match="[user_pref,gene,The Catalog Blueprint]">
					<view_src_field id="423" si_match_field="pref_name" />
					<view_src_field id="424" si_match_field="pref_value" />
				</view_src>
				<view_field id="842" si_row_field="theme_name" src_field="[pref_value,user_pref]" />
				<view_expr id="843" view_part="RESULT" set_result_field="theme_count" cont_type="SCALAR" valf_call_sroutine="COUNT">
					<view_expr id="844" cont_type="SCALAR" valf_src_field="[pref_value,user_pref]" />
				</view_expr>
				<view_expr id="811" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="EQ">
					<view_expr id="812" cont_type="SCALAR" valf_src_field="[pref_name,user_pref]" />
					<view_expr id="813" cont_type="SCALAR" valf_literal="theme" scalar_data_type="str30" />
				</view_expr>
				<view_expr id="814" view_part="GROUP" cont_type="SCALAR" valf_src_field="[pref_value,user_pref]" />
				<view_expr id="815" view_part="HAVING" cont_type="SCALAR" valf_call_sroutine="GT">
					<view_expr id="816" cont_type="SCALAR" valf_call_sroutine="COUNT" />
					<view_expr id="817" cont_type="SCALAR" valf_literal="1" scalar_data_type="int" />
				</view_expr>
				<view_expr id="855" view_part="ORDER" cont_type="SCALAR" valf_result_field="theme_count" />
				<view_expr id="856" view_part="ORDER" cont_type="SCALAR" valf_result_field="theme_name" />
			</view>
			<routine id="61" si_name="get_user" routine_type="FUNCTION" return_cont_type="CURSOR">
				<routine_arg id="91" si_name="curr_uid" cont_type="SCALAR" scalar_data_type="int" />
				<view id="55" si_name="get_user" view_type="JOINED" row_data_type="user">
					<view_src id="78" si_name="m" match="[user,gene,The Catalog Blueprint]">
						<view_src_field id="430" si_match_field="user_id" />
						<view_src_field id="431" si_match_field="login_name" />
					</view_src>
					<view_expr id="801" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="EQ">
						<view_expr id="802" cont_type="SCALAR" valf_src_field="[user_id,m]" />
						<view_expr id="803" cont_type="SCALAR" valf_p_routine_item="curr_uid" />
					</view_expr>
					<view_expr id="851" view_part="ORDER" cont_type="SCALAR" valf_src_field="[login_name,m]" />
				</view>
				<routine_stmt id="94" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="62" si_name="get_pwp" routine_type="FUNCTION" return_cont_type="CURSOR">
				<routine_arg id="92" si_name="srchw_fa" cont_type="SCALAR" scalar_data_type="str30" />
				<routine_arg id="93" si_name="srchw_mo" cont_type="SCALAR" scalar_data_type="str30" />
				<view id="56" si_name="get_pwp" view_type="JOINED" row_data_type="person_with_parents">
					<view_src id="79" si_name="m" match="[person_with_parents,gene,The Catalog Blueprint]">
						<view_src_field id="427" si_match_field="self_name" />
						<view_src_field id="428" si_match_field="father_name" />
						<view_src_field id="429" si_match_field="mother_name" />
					</view_src>
					<view_expr id="804" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="AND">
						<view_expr id="805" cont_type="SCALAR" valf_call_sroutine="LIKE">
							<view_expr id="806" cont_type="SCALAR" valf_src_field="[father_name,m]" />
							<view_expr id="807" cont_type="SCALAR" valf_p_routine_item="srchw_fa" />
						</view_expr>
						<view_expr id="808" cont_type="SCALAR" valf_call_sroutine="LIKE">
							<view_expr id="809" cont_type="SCALAR" valf_src_field="[mother_name,m]" />
							<view_expr id="810" cont_type="SCALAR" valf_p_routine_item="srchw_mo" />
						</view_expr>
					</view_expr>
					<view_expr id="852" view_part="ORDER" cont_type="SCALAR" valf_src_field="[self_name,m]" />
					<view_expr id="853" view_part="ORDER" cont_type="SCALAR" valf_src_field="[father_name,m]" />
					<view_expr id="854" view_part="ORDER" cont_type="SCALAR" valf_src_field="[mother_name,m]" />
				</view>
				<routine_stmt id="95" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="63" si_name="get_theme" routine_type="FUNCTION" return_cont_type="CURSOR">
				<view id="57" si_name="get_theme" view_type="ALIAS" row_data_type="user_theme">
					<view_src id="69" si_name="m" match="user_theme" />
				</view>
				<routine_stmt id="96" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="64" si_name="get_person" routine_type="FUNCTION" return_cont_type="CURSOR">
				<view id="54" si_name="get_person" view_type="ALIAS" row_data_type="person">
					<view_src id="77" si_name="person" match="[person,gene,The Catalog Blueprint]" />
				</view>
				<routine_stmt id="97" call_sroutine="CURSOR_OPEN" />
			</routine>
		</application>
	</blueprints>
	<tools />
	<sites>
		<application_instance id="35" si_name="My App Instance" blueprint="My App" />
	</sites>
	<circumventions />
</root>
'
	;
}

######################################################################

sub expected_model_sid_short_xml_output() returns Str {
	return
'<?xml version="1.0" encoding="UTF-8"?>
<root>
	<elements>
		<scalar_data_type id="1" si_name="bin1k" base_type="STR_BIT" max_octets="1000" />
		<scalar_data_type id="2" si_name="bin32k" base_type="STR_BIT" max_octets="32000" />
		<scalar_data_type id="3" si_name="str4" base_type="STR_CHAR" max_chars="4" store_fixed="1" char_enc="ASCII" trim_white="1" uc_latin="1" pad_char=" " trim_pad="1" />
		<scalar_data_type id="4" si_name="str10" base_type="STR_CHAR" max_chars="10" store_fixed="1" char_enc="ASCII" trim_white="1" pad_char=" " trim_pad="1" />
		<scalar_data_type id="5" si_name="str30" base_type="STR_CHAR" max_chars="30" char_enc="ASCII" trim_white="1" />
		<scalar_data_type id="6" si_name="str2k" base_type="STR_CHAR" max_chars="2000" char_enc="UTF8" />
		<scalar_data_type id="7" si_name="byte" base_type="NUM_INT" num_precision="3" />
		<scalar_data_type id="8" si_name="short" base_type="NUM_INT" num_precision="5" />
		<scalar_data_type id="9" si_name="int" base_type="NUM_INT" num_precision="10" />
		<scalar_data_type id="10" si_name="long" base_type="NUM_INT" num_precision="19" />
		<scalar_data_type id="11" si_name="ubyte" base_type="NUM_INT" num_precision="3" num_unsigned="1" />
		<scalar_data_type id="12" si_name="ushort" base_type="NUM_INT" num_precision="5" num_unsigned="1" />
		<scalar_data_type id="13" si_name="uint" base_type="NUM_INT" num_precision="10" num_unsigned="1" />
		<scalar_data_type id="14" si_name="ulong" base_type="NUM_INT" num_precision="19" num_unsigned="1" />
		<scalar_data_type id="15" si_name="float" base_type="NUM_APR" num_octets="4" />
		<scalar_data_type id="16" si_name="double" base_type="NUM_APR" num_octets="8" />
		<scalar_data_type id="17" si_name="dec10p2" base_type="NUM_EXA" num_precision="10" num_scale="2" />
		<scalar_data_type id="18" si_name="dec255" base_type="NUM_EXA" num_precision="255" />
		<scalar_data_type id="19" si_name="boolean" base_type="BOOLEAN" />
		<scalar_data_type id="20" si_name="datetime" base_type="DATM_FULL" calendar="ABS" />
		<scalar_data_type id="21" si_name="dtchines" base_type="DATM_FULL" calendar="CHI" />
		<scalar_data_type id="22" si_name="sex" base_type="STR_CHAR" max_chars="1" char_enc="ASCII">
			<scalar_data_type_opt id="28" si_value="M" />
			<scalar_data_type_opt id="29" si_value="F" />
		</scalar_data_type>
		<scalar_data_type id="23" si_name="str20" base_type="STR_CHAR" max_chars="20" char_enc="ASCII" />
		<scalar_data_type id="24" si_name="str100" base_type="STR_CHAR" max_chars="100" char_enc="ASCII" />
		<scalar_data_type id="25" si_name="str250" base_type="STR_CHAR" max_chars="250" char_enc="ASCII" />
		<scalar_data_type id="26" si_name="entitynm" base_type="STR_CHAR" max_chars="30" char_enc="ASCII" />
		<scalar_data_type id="27" si_name="generic" base_type="STR_CHAR" max_chars="250" char_enc="ASCII" />
		<row_data_type id="304" si_name="person">
			<row_data_type_field id="220" si_name="person_id" scalar_data_type="int" />
			<row_data_type_field id="221" si_name="alternate_id" scalar_data_type="str20" />
			<row_data_type_field id="222" si_name="name" scalar_data_type="str100" />
			<row_data_type_field id="223" si_name="sex" scalar_data_type="sex" />
			<row_data_type_field id="224" si_name="father_id" scalar_data_type="int" />
			<row_data_type_field id="225" si_name="mother_id" scalar_data_type="int" />
		</row_data_type>
		<row_data_type id="312" si_name="person_with_parents">
			<row_data_type_field id="116" si_name="self_id" scalar_data_type="int" />
			<row_data_type_field id="117" si_name="self_name" scalar_data_type="str100" />
			<row_data_type_field id="118" si_name="father_id" scalar_data_type="int" />
			<row_data_type_field id="119" si_name="father_name" scalar_data_type="str100" />
			<row_data_type_field id="120" si_name="mother_id" scalar_data_type="int" />
			<row_data_type_field id="121" si_name="mother_name" scalar_data_type="str100" />
		</row_data_type>
		<row_data_type id="301" si_name="user_auth">
			<row_data_type_field id="201" si_name="user_id" scalar_data_type="int" />
			<row_data_type_field id="202" si_name="login_name" scalar_data_type="str20" />
			<row_data_type_field id="203" si_name="login_pass" scalar_data_type="str20" />
			<row_data_type_field id="204" si_name="private_name" scalar_data_type="str100" />
			<row_data_type_field id="205" si_name="private_email" scalar_data_type="str100" />
			<row_data_type_field id="206" si_name="may_login" scalar_data_type="boolean" />
			<row_data_type_field id="207" si_name="max_sessions" scalar_data_type="byte" />
		</row_data_type>
		<row_data_type id="302" si_name="user_profile">
			<row_data_type_field id="208" si_name="user_id" scalar_data_type="int" />
			<row_data_type_field id="209" si_name="public_name" scalar_data_type="str250" />
			<row_data_type_field id="210" si_name="public_email" scalar_data_type="str250" />
			<row_data_type_field id="211" si_name="web_url" scalar_data_type="str250" />
			<row_data_type_field id="212" si_name="contact_net" scalar_data_type="str250" />
			<row_data_type_field id="213" si_name="contact_phy" scalar_data_type="str250" />
			<row_data_type_field id="214" si_name="bio" scalar_data_type="str250" />
			<row_data_type_field id="215" si_name="plan" scalar_data_type="str250" />
			<row_data_type_field id="216" si_name="comments" scalar_data_type="str250" />
		</row_data_type>
		<row_data_type id="311" si_name="user">
			<row_data_type_field id="101" si_name="user_id" scalar_data_type="int" />
			<row_data_type_field id="102" si_name="login_name" scalar_data_type="str20" />
			<row_data_type_field id="103" si_name="login_pass" scalar_data_type="str20" />
			<row_data_type_field id="104" si_name="private_name" scalar_data_type="str100" />
			<row_data_type_field id="105" si_name="private_email" scalar_data_type="str100" />
			<row_data_type_field id="106" si_name="may_login" scalar_data_type="boolean" />
			<row_data_type_field id="107" si_name="max_sessions" scalar_data_type="byte" />
			<row_data_type_field id="108" si_name="public_name" scalar_data_type="str250" />
			<row_data_type_field id="109" si_name="public_email" scalar_data_type="str250" />
			<row_data_type_field id="110" si_name="web_url" scalar_data_type="str250" />
			<row_data_type_field id="111" si_name="contact_net" scalar_data_type="str250" />
			<row_data_type_field id="112" si_name="contact_phy" scalar_data_type="str250" />
			<row_data_type_field id="113" si_name="bio" scalar_data_type="str250" />
			<row_data_type_field id="114" si_name="plan" scalar_data_type="str250" />
			<row_data_type_field id="115" si_name="comments" scalar_data_type="str250" />
		</row_data_type>
		<row_data_type id="303" si_name="user_pref">
			<row_data_type_field id="217" si_name="user_id" scalar_data_type="int" />
			<row_data_type_field id="218" si_name="pref_name" scalar_data_type="entitynm" />
			<row_data_type_field id="219" si_name="pref_value" scalar_data_type="generic" />
		</row_data_type>
		<row_data_type id="313" si_name="user_theme">
			<row_data_type_field id="122" si_name="theme_name" scalar_data_type="generic" />
			<row_data_type_field id="123" si_name="theme_count" scalar_data_type="int" />
		</row_data_type>
	</elements>
	<blueprints>
		<catalog id="31" si_name="The Catalog Blueprint">
			<owner id="32" si_name="Gene\'s Owner" />
			<schema id="33" si_name="gene" owner="Gene\'s Owner">
				<table id="44" si_name="person" row_data_type="person">
					<table_field id="520" si_row_field="person_id" mandatory="1" default_val="1" auto_inc="1" />
					<table_field id="522" si_row_field="name" mandatory="1" />
					<table_index id="701" si_name="primary" index_type="UNIQUE">
						<table_index_field id="702" si_field="person_id" />
					</table_index>
					<table_index id="703" si_name="ak_alternate_id" index_type="UNIQUE">
						<table_index_field id="704" si_field="alternate_id" />
					</table_index>
					<table_index id="705" si_name="fk_father" index_type="FOREIGN" f_table="person">
						<table_index_field id="706" si_field="father_id" f_field="person_id" />
					</table_index>
					<table_index id="707" si_name="fk_mother" index_type="FOREIGN" f_table="person">
						<table_index_field id="708" si_field="mother_id" f_field="person_id" />
					</table_index>
				</table>
				<view id="52" si_name="person_with_parents" view_type="JOINED" row_data_type="person_with_parents">
					<view_src id="73" si_name="self" match="person">
						<view_src_field id="417" si_match_field="person_id" />
						<view_src_field id="418" si_match_field="name" />
						<view_src_field id="425" si_match_field="father_id" />
						<view_src_field id="426" si_match_field="mother_id" />
					</view_src>
					<view_src id="74" si_name="father" match="person">
						<view_src_field id="419" si_match_field="person_id" />
						<view_src_field id="420" si_match_field="name" />
					</view_src>
					<view_src id="75" si_name="mother" match="person">
						<view_src_field id="421" si_match_field="person_id" />
						<view_src_field id="422" si_match_field="name" />
					</view_src>
					<view_field id="616" si_row_field="self_id" src_field="[person_id,self]" />
					<view_field id="617" si_row_field="self_name" src_field="[name,self]" />
					<view_field id="618" si_row_field="father_id" src_field="[person_id,father]" />
					<view_field id="619" si_row_field="father_name" src_field="[name,father]" />
					<view_field id="620" si_row_field="mother_id" src_field="[person_id,mother]" />
					<view_field id="621" si_row_field="mother_name" src_field="[name,mother]" />
					<view_join id="731" lhs_src="self" rhs_src="father" join_op="LEFT">
						<view_join_field id="732" lhs_src_field="father_id" rhs_src_field="person_id" />
					</view_join>
					<view_join id="733" lhs_src="self" rhs_src="mother" join_op="LEFT">
						<view_join_field id="734" lhs_src_field="mother_id" rhs_src_field="person_id" />
					</view_join>
				</view>
				<table id="41" si_name="user_auth" row_data_type="user_auth">
					<table_field id="501" si_row_field="user_id" mandatory="1" default_val="1" auto_inc="1" />
					<table_field id="502" si_row_field="login_name" mandatory="1" />
					<table_field id="503" si_row_field="login_pass" mandatory="1" />
					<table_field id="504" si_row_field="private_name" mandatory="1" />
					<table_field id="505" si_row_field="private_email" mandatory="1" />
					<table_field id="506" si_row_field="may_login" mandatory="1" />
					<table_field id="507" si_row_field="max_sessions" mandatory="1" default_val="3" />
					<table_index id="709" si_name="primary" index_type="UNIQUE">
						<table_index_field id="710" si_field="user_id" />
					</table_index>
					<table_index id="711" si_name="ak_login_name" index_type="UNIQUE">
						<table_index_field id="712" si_field="login_name" />
					</table_index>
					<table_index id="713" si_name="ak_private_email" index_type="UNIQUE">
						<table_index_field id="714" si_field="private_email" />
					</table_index>
				</table>
				<table id="42" si_name="user_profile" row_data_type="user_profile">
					<table_field id="508" si_row_field="user_id" mandatory="1" />
					<table_field id="509" si_row_field="public_name" mandatory="1" />
					<table_index id="715" si_name="primary" index_type="UNIQUE">
						<table_index_field id="716" si_field="user_id" />
					</table_index>
					<table_index id="717" si_name="ak_public_name" index_type="UNIQUE">
						<table_index_field id="718" si_field="public_name" />
					</table_index>
					<table_index id="719" si_name="fk_user" index_type="FOREIGN" f_table="user_auth">
						<table_index_field id="720" si_field="user_id" f_field="user_id" />
					</table_index>
				</table>
				<view id="51" si_name="user" view_type="JOINED" row_data_type="user">
					<view_src id="71" si_name="user_auth" match="user_auth">
						<view_src_field id="401" si_match_field="user_id" />
						<view_src_field id="402" si_match_field="login_name" />
						<view_src_field id="403" si_match_field="login_pass" />
						<view_src_field id="404" si_match_field="private_name" />
						<view_src_field id="405" si_match_field="private_email" />
						<view_src_field id="406" si_match_field="may_login" />
						<view_src_field id="407" si_match_field="max_sessions" />
					</view_src>
					<view_src id="72" si_name="user_profile" match="user_profile">
						<view_src_field id="408" si_match_field="user_id" />
						<view_src_field id="409" si_match_field="public_name" />
						<view_src_field id="410" si_match_field="public_email" />
						<view_src_field id="411" si_match_field="web_url" />
						<view_src_field id="412" si_match_field="contact_net" />
						<view_src_field id="413" si_match_field="contact_phy" />
						<view_src_field id="414" si_match_field="bio" />
						<view_src_field id="415" si_match_field="plan" />
						<view_src_field id="416" si_match_field="comments" />
					</view_src>
					<view_field id="601" si_row_field="user_id" src_field="[user_id,user_auth]" />
					<view_field id="602" si_row_field="login_name" src_field="login_name" />
					<view_field id="603" si_row_field="login_pass" src_field="login_pass" />
					<view_field id="604" si_row_field="private_name" src_field="private_name" />
					<view_field id="605" si_row_field="private_email" src_field="private_email" />
					<view_field id="606" si_row_field="may_login" src_field="may_login" />
					<view_field id="607" si_row_field="max_sessions" src_field="max_sessions" />
					<view_field id="608" si_row_field="public_name" src_field="public_name" />
					<view_field id="609" si_row_field="public_email" src_field="public_email" />
					<view_field id="610" si_row_field="web_url" src_field="web_url" />
					<view_field id="611" si_row_field="contact_net" src_field="contact_net" />
					<view_field id="612" si_row_field="contact_phy" src_field="contact_phy" />
					<view_field id="613" si_row_field="bio" src_field="bio" />
					<view_field id="614" si_row_field="plan" src_field="plan" />
					<view_field id="615" si_row_field="comments" src_field="comments" />
					<view_join id="735" lhs_src="user_auth" rhs_src="user_profile" join_op="LEFT">
						<view_join_field id="736" lhs_src_field="user_id" rhs_src_field="user_id" />
					</view_join>
				</view>
				<table id="43" si_name="user_pref" row_data_type="user_pref">
					<table_field id="517" si_row_field="user_id" mandatory="1" />
					<table_field id="518" si_row_field="pref_name" mandatory="1" />
					<table_index id="721" si_name="primary" index_type="UNIQUE">
						<table_index_field id="722" si_field="user_id" />
						<table_index_field id="723" si_field="pref_name" />
					</table_index>
					<table_index id="724" si_name="fk_user" index_type="FOREIGN" f_table="user_auth">
						<table_index_field id="725" si_field="user_id" f_field="user_id" />
					</table_index>
				</table>
			</schema>
		</catalog>
		<application id="34" si_name="My App">
			<view id="53" si_name="user_theme" view_type="JOINED" row_data_type="user_theme">
				<view_src id="76" si_name="user_pref" match="user_pref">
					<view_src_field id="423" si_match_field="pref_name" />
					<view_src_field id="424" si_match_field="pref_value" />
				</view_src>
				<view_field id="842" si_row_field="theme_name" src_field="pref_value" />
				<view_expr id="843" view_part="RESULT" set_result_field="theme_count" cont_type="SCALAR" valf_call_sroutine="COUNT">
					<view_expr id="844" cont_type="SCALAR" valf_src_field="pref_value" />
				</view_expr>
				<view_expr id="811" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="EQ">
					<view_expr id="812" cont_type="SCALAR" valf_src_field="pref_name" />
					<view_expr id="813" cont_type="SCALAR" valf_literal="theme" scalar_data_type="str30" />
				</view_expr>
				<view_expr id="814" view_part="GROUP" cont_type="SCALAR" valf_src_field="pref_value" />
				<view_expr id="815" view_part="HAVING" cont_type="SCALAR" valf_call_sroutine="GT">
					<view_expr id="816" cont_type="SCALAR" valf_call_sroutine="COUNT" />
					<view_expr id="817" cont_type="SCALAR" valf_literal="1" scalar_data_type="int" />
				</view_expr>
				<view_expr id="855" view_part="ORDER" cont_type="SCALAR" valf_result_field="theme_count" />
				<view_expr id="856" view_part="ORDER" cont_type="SCALAR" valf_result_field="theme_name" />
			</view>
			<routine id="61" si_name="get_user" routine_type="FUNCTION" return_cont_type="CURSOR">
				<routine_arg id="91" si_name="curr_uid" cont_type="SCALAR" scalar_data_type="int" />
				<view id="55" si_name="get_user" view_type="JOINED" row_data_type="user">
					<view_src id="78" si_name="m" match="user">
						<view_src_field id="430" si_match_field="user_id" />
						<view_src_field id="431" si_match_field="login_name" />
					</view_src>
					<view_expr id="801" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="EQ">
						<view_expr id="802" cont_type="SCALAR" valf_src_field="user_id" />
						<view_expr id="803" cont_type="SCALAR" valf_p_routine_item="curr_uid" />
					</view_expr>
					<view_expr id="851" view_part="ORDER" cont_type="SCALAR" valf_src_field="login_name" />
				</view>
				<routine_stmt id="94" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="62" si_name="get_pwp" routine_type="FUNCTION" return_cont_type="CURSOR">
				<routine_arg id="92" si_name="srchw_fa" cont_type="SCALAR" scalar_data_type="str30" />
				<routine_arg id="93" si_name="srchw_mo" cont_type="SCALAR" scalar_data_type="str30" />
				<view id="56" si_name="get_pwp" view_type="JOINED" row_data_type="person_with_parents">
					<view_src id="79" si_name="m" match="person_with_parents">
						<view_src_field id="427" si_match_field="self_name" />
						<view_src_field id="428" si_match_field="father_name" />
						<view_src_field id="429" si_match_field="mother_name" />
					</view_src>
					<view_expr id="804" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="AND">
						<view_expr id="805" cont_type="SCALAR" valf_call_sroutine="LIKE">
							<view_expr id="806" cont_type="SCALAR" valf_src_field="father_name" />
							<view_expr id="807" cont_type="SCALAR" valf_p_routine_item="srchw_fa" />
						</view_expr>
						<view_expr id="808" cont_type="SCALAR" valf_call_sroutine="LIKE">
							<view_expr id="809" cont_type="SCALAR" valf_src_field="mother_name" />
							<view_expr id="810" cont_type="SCALAR" valf_p_routine_item="srchw_mo" />
						</view_expr>
					</view_expr>
					<view_expr id="852" view_part="ORDER" cont_type="SCALAR" valf_src_field="self_name" />
					<view_expr id="853" view_part="ORDER" cont_type="SCALAR" valf_src_field="father_name" />
					<view_expr id="854" view_part="ORDER" cont_type="SCALAR" valf_src_field="mother_name" />
				</view>
				<routine_stmt id="95" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="63" si_name="get_theme" routine_type="FUNCTION" return_cont_type="CURSOR">
				<view id="57" si_name="get_theme" view_type="ALIAS" row_data_type="user_theme">
					<view_src id="69" si_name="m" match="user_theme" />
				</view>
				<routine_stmt id="96" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="64" si_name="get_person" routine_type="FUNCTION" return_cont_type="CURSOR">
				<view id="54" si_name="get_person" view_type="ALIAS" row_data_type="person">
					<view_src id="77" si_name="person" match="person" />
				</view>
				<routine_stmt id="97" call_sroutine="CURSOR_OPEN" />
			</routine>
		</application>
	</blueprints>
	<tools />
	<sites>
		<application_instance id="35" si_name="My App Instance" blueprint="My App" />
	</sites>
	<circumventions />
</root>
'
	;
}

######################################################################

1;
