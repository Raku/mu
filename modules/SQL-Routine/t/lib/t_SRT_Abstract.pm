#!pugs
use v6;

module t_SRT_Abstract;

# This module is used when testing SQL::Routine.
# These tests check that a model can be built using the abstract wrapper 
# interface without errors, and serializes to the correct output.
# This module contains sample input and output data which is used to test 
# SQL::Routine, and possibly other modules that are derived from it.

######################################################################

sub populate_model( SQL::Routine::Container $model ) {

	##### NEXT SET CATALOG ELEMENT-TYPE DETAILS #####

	$model.build_child_node_trees( [ map { { 'NODE_TYPE' => 'scalar_data_type', 'ATTRS' => $_ } } (
		{ 'si_name' => 'bin1k' , 'base_type' => 'STR_BIT', 'max_octets' =>  1_000, },
		{ 'si_name' => 'bin32k', 'base_type' => 'STR_BIT', 'max_octets' => 32_000, },
		{ 'si_name' => 'str4'  , 'base_type' => 'STR_CHAR', 'max_chars' =>  4, 'store_fixed' => 1, 
			'char_enc' => 'ASCII', 'trim_white' => 1, 'uc_latin' => 1, 
			'pad_char' => ' ', 'trim_pad' => 1, },
		{ 'si_name' => 'str10' , 'base_type' => 'STR_CHAR', 'max_chars' => 10, 'store_fixed' => 1, 
			'char_enc' => 'ASCII', 'trim_white' => 1, 
			'pad_char' => ' ', 'trim_pad' => 1, },
		{ 'si_name' => 'str30' , 'base_type' => 'STR_CHAR', 'max_chars' =>    30, 
			'char_enc' => 'ASCII', 'trim_white' => 1, },
		{ 'si_name' => 'str2k' , 'base_type' => 'STR_CHAR', 'max_chars' => 2_000, 'char_enc' => 'UTF8', },
		{ 'si_name' => 'byte' , 'base_type' => 'NUM_INT', 'num_precision' =>  3, },
		{ 'si_name' => 'short', 'base_type' => 'NUM_INT', 'num_precision' =>  5, },
		{ 'si_name' => 'int'  , 'base_type' => 'NUM_INT', 'num_precision' => 10, },
		{ 'si_name' => 'long' , 'base_type' => 'NUM_INT', 'num_precision' => 19, },
		{ 'si_name' => 'ubyte' , 'base_type' => 'NUM_INT', 'num_precision' =>  3, 'num_unsigned' => 1, },
		{ 'si_name' => 'ushort', 'base_type' => 'NUM_INT', 'num_precision' =>  5, 'num_unsigned' => 1, },
		{ 'si_name' => 'uint'  , 'base_type' => 'NUM_INT', 'num_precision' => 10, 'num_unsigned' => 1, },
		{ 'si_name' => 'ulong' , 'base_type' => 'NUM_INT', 'num_precision' => 19, 'num_unsigned' => 1, },
		{ 'si_name' => 'float' , 'base_type' => 'NUM_APR', 'num_octets' => 4, },
		{ 'si_name' => 'double', 'base_type' => 'NUM_APR', 'num_octets' => 8, },
		{ 'si_name' => 'dec10p2', 'base_type' => 'NUM_EXA', 'num_precision' =>  10, 'num_scale' => 2, },
		{ 'si_name' => 'dec255' , 'base_type' => 'NUM_EXA', 'num_precision' => 255, },
		{ 'si_name' => 'boolean', 'base_type' => 'BOOLEAN', },
		{ 'si_name' => 'datetime', 'base_type' => 'DATM_FULL', 'calendar' => 'ABS', },
		{ 'si_name' => 'dtchines', 'base_type' => 'DATM_FULL', 'calendar' => 'CHI', },
		{ 'si_name' => 'sex'   , 'base_type' => 'STR_CHAR', 'max_chars' =>     1, 'char_enc' => 'ASCII', },
		{ 'si_name' => 'str20' , 'base_type' => 'STR_CHAR', 'max_chars' =>    20, 'char_enc' => 'ASCII', },
		{ 'si_name' => 'str100', 'base_type' => 'STR_CHAR', 'max_chars' =>   100, 'char_enc' => 'ASCII', },
		{ 'si_name' => 'str250', 'base_type' => 'STR_CHAR', 'max_chars' =>   250, 'char_enc' => 'ASCII', },
		{ 'si_name' => 'entitynm', 'base_type' => 'STR_CHAR', 'max_chars' =>  30, 'char_enc' => 'ASCII', },
		{ 'si_name' => 'generic' , 'base_type' => 'STR_CHAR', 'max_chars' => 250, 'char_enc' => 'ASCII', },
	) ] );

	my $sex = $model.find_child_node_by_surrogate_id( [undef,'root','elements','sex'] );
	$sex.build_child_node_trees( [ map { { 'NODE_TYPE' => 'scalar_data_type_opt', 'ATTRS' => $_ } } (
		{ 'si_value' => 'M', },
		{ 'si_value' => 'F', },
	) ] );

	$model.build_child_node_tree( { 'NODE_TYPE' => 'row_data_type', 
			'ATTRS' => { 'si_name' => 'person', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'row_data_type_field', 'ATTRS' => $_ } } (
			{ 'si_name' => 'person_id'   , 'scalar_data_type' => 'int'   , },
			{ 'si_name' => 'alternate_id', 'scalar_data_type' => 'str20' , },
			{ 'si_name' => 'name'        , 'scalar_data_type' => 'str100', },
			{ 'si_name' => 'sex'         , 'scalar_data_type' => 'sex'   , },
			{ 'si_name' => 'father_id'   , 'scalar_data_type' => 'int'   , },
			{ 'si_name' => 'mother_id'   , 'scalar_data_type' => 'int'   , },
		) ),
	] } );

	$model.build_child_node_tree( { 'NODE_TYPE' => 'row_data_type', 
			'ATTRS' => { 'si_name' => 'person_with_parents', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'row_data_type_field', 'ATTRS' => $_ } } (
			{ 'si_name' => 'self_id'    , 'scalar_data_type' => 'int'   , },
			{ 'si_name' => 'self_name'  , 'scalar_data_type' => 'str100', },
			{ 'si_name' => 'father_id'  , 'scalar_data_type' => 'int'   , },
			{ 'si_name' => 'father_name', 'scalar_data_type' => 'str100', },
			{ 'si_name' => 'mother_id'  , 'scalar_data_type' => 'int'   , },
			{ 'si_name' => 'mother_name', 'scalar_data_type' => 'str100', },
		) ),
	] } );

	$model.build_child_node_tree( { 'NODE_TYPE' => 'row_data_type', 
			'ATTRS' => { 'si_name' => 'user_auth', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'row_data_type_field', 'ATTRS' => $_ } } (
			{ 'si_name' => 'user_id'      , 'scalar_data_type' => 'int'    , },
			{ 'si_name' => 'login_name'   , 'scalar_data_type' => 'str20'  , },
			{ 'si_name' => 'login_pass'   , 'scalar_data_type' => 'str20'  , },
			{ 'si_name' => 'private_name' , 'scalar_data_type' => 'str100' , },
			{ 'si_name' => 'private_email', 'scalar_data_type' => 'str100' , },
			{ 'si_name' => 'may_login'    , 'scalar_data_type' => 'boolean', },
			{ 'si_name' => 'max_sessions' , 'scalar_data_type' => 'byte'   , },
		) ),
	] } );

	$model.build_child_node_tree( { 'NODE_TYPE' => 'row_data_type', 
			'ATTRS' => { 'si_name' => 'user_profile', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'row_data_type_field', 'ATTRS' => $_ } } (
			{ 'si_name' => 'user_id'     , 'scalar_data_type' => 'int'   , },
			{ 'si_name' => 'public_name' , 'scalar_data_type' => 'str250', },
			{ 'si_name' => 'public_email', 'scalar_data_type' => 'str250', },
			{ 'si_name' => 'web_url'     , 'scalar_data_type' => 'str250', },
			{ 'si_name' => 'contact_net' , 'scalar_data_type' => 'str250', },
			{ 'si_name' => 'contact_phy' , 'scalar_data_type' => 'str250', },
			{ 'si_name' => 'bio'         , 'scalar_data_type' => 'str250', },
			{ 'si_name' => 'plan'        , 'scalar_data_type' => 'str250', },
			{ 'si_name' => 'comments'    , 'scalar_data_type' => 'str250', },
		) ),
	] } );

	$model.build_child_node_tree( { 'NODE_TYPE' => 'row_data_type', 
			'ATTRS' => { 'si_name' => 'user', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'row_data_type_field', 'ATTRS' => $_ } } (
			{ 'si_name' => 'user_id'      , 'scalar_data_type' => 'int'    , },
			{ 'si_name' => 'login_name'   , 'scalar_data_type' => 'str20'  , },
			{ 'si_name' => 'login_pass'   , 'scalar_data_type' => 'str20'  , },
			{ 'si_name' => 'private_name' , 'scalar_data_type' => 'str100' , },
			{ 'si_name' => 'private_email', 'scalar_data_type' => 'str100' , },
			{ 'si_name' => 'may_login'    , 'scalar_data_type' => 'boolean', },
			{ 'si_name' => 'max_sessions' , 'scalar_data_type' => 'byte'   , },
			{ 'si_name' => 'public_name'  , 'scalar_data_type' => 'str250' , },
			{ 'si_name' => 'public_email' , 'scalar_data_type' => 'str250' , },
			{ 'si_name' => 'web_url'      , 'scalar_data_type' => 'str250' , },
			{ 'si_name' => 'contact_net'  , 'scalar_data_type' => 'str250' , },
			{ 'si_name' => 'contact_phy'  , 'scalar_data_type' => 'str250' , },
			{ 'si_name' => 'bio'          , 'scalar_data_type' => 'str250' , },
			{ 'si_name' => 'plan'         , 'scalar_data_type' => 'str250' , },
			{ 'si_name' => 'comments'     , 'scalar_data_type' => 'str250' , },
		) ),
	] } );

	$model.build_child_node_tree( { 'NODE_TYPE' => 'row_data_type', 
			'ATTRS' => { 'si_name' => 'user_pref', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'row_data_type_field', 'ATTRS' => $_ } } (
			{ 'si_name' => 'user_id'   , 'scalar_data_type' => 'int'     , },
			{ 'si_name' => 'pref_name' , 'scalar_data_type' => 'entitynm', },
			{ 'si_name' => 'pref_value', 'scalar_data_type' => 'generic' , },
		) ),
	] } );

	##### NEXT SET APPLICATION ELEMENT-TYPE DETAILS #####

	$model.build_child_node_tree( { 'NODE_TYPE' => 'row_data_type', 
			'ATTRS' => { 'si_name' => 'user_theme', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'row_data_type_field', 'ATTRS' => $_ } } (
			{ 'si_name' => 'theme_name' , 'scalar_data_type' => 'generic', },
			{ 'si_name' => 'theme_count', 'scalar_data_type' => 'int'    , },
		) ),
	] } );

	##### NEXT SET CATALOG BLUEPRINT-TYPE DETAILS #####

	my $catalog = $model.build_child_node_tree( 
		{ 'NODE_TYPE' => 'catalog', 'ATTRS' => { 'si_name' => 'The Catalog Blueprint' }, 
		'CHILDREN' => [ { 'NODE_TYPE' => 'owner', 'ATTRS' => { 'si_name' => 'Gene\'s Owner' } } ] } ); 

	my $schema = $catalog.build_child_node_tree( { 'NODE_TYPE' => 'schema', 
		'ATTRS' => { 'si_name' => 'gene', 'owner' => 'Gene\'s Owner', } } ); 

	$schema.build_child_node_tree( { 'NODE_TYPE' => 'table', 
			'ATTRS' => { 'si_name' => 'person', 'row_data_type' => 'person', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'table_field', 'ATTRS' => $_ } } (
			{ 'si_row_field' => 'person_id', 'mandatory' => 1, 'default_val' => 1, 'auto_inc' => 1, },
			{ 'si_row_field' => 'name'     , 'mandatory' => 1, },
		) ),
		( map { { 'NODE_TYPE' => 'table_index', 'ATTRS' => $_[0], 
				'CHILDREN' => { 'NODE_TYPE' => 'table_index_field', 'ATTRS' => $_[1] } } } (
			[ { 'si_name' => 'primary'        , 'index_type' => 'UNIQUE', }, 'person_id'    ], 
			[ { 'si_name' => 'ak_alternate_id', 'index_type' => 'UNIQUE', }, 'alternate_id' ], 
			[ { 'si_name' => 'fk_father', 'index_type' => 'FOREIGN', 'f_table' => 'person', }, 
				{ 'si_field' => 'father_id', 'f_field' => 'person_id' } ], 
			[ { 'si_name' => 'fk_mother', 'index_type' => 'FOREIGN', 'f_table' => 'person', }, 
				{ 'si_field' => 'mother_id', 'f_field' => 'person_id' } ], 
		) ),
	] } );

	$schema.build_child_node_tree( { 'NODE_TYPE' => 'view', 
			'ATTRS' => { 'si_name' => 'person_with_parents', 'view_type' => 'JOINED', 'row_data_type' => 'person_with_parents', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'si_name' => $_, 'match' => 'person', }, 
			'CHILDREN' => [ map { { 'NODE_TYPE' => 'view_src_field', 'ATTRS' => $_ } } qw( person_id name father_id mother_id ) ] 
		} } qw( self ) ),
		( map { { 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'si_name' => $_, 'match' => 'person', }, 
			'CHILDREN' => [ map { { 'NODE_TYPE' => 'view_src_field', 'ATTRS' => $_ } } qw( person_id name ) ] 
		} } qw( father mother ) ),
		( map { { 'NODE_TYPE' => 'view_field', 'ATTRS' => $_ } } (
			{ 'si_row_field' => 'self_id'    , 'src_field' => ['person_id','self'  ], },
			{ 'si_row_field' => 'self_name'  , 'src_field' => ['name'     ,'self'  ], },
			{ 'si_row_field' => 'father_id'  , 'src_field' => ['person_id','father'], },
			{ 'si_row_field' => 'father_name', 'src_field' => ['name'     ,'father'], },
			{ 'si_row_field' => 'mother_id'  , 'src_field' => ['person_id','mother'], },
			{ 'si_row_field' => 'mother_name', 'src_field' => ['name'     ,'mother'], },
		) ),
		{ 'NODE_TYPE' => 'view_join', 'ATTRS' => { 'lhs_src' => 'self', 
				'rhs_src' => 'father', 'join_op' => 'LEFT', }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_join_field', 'ATTRS' => { 'lhs_src_field' => 'father_id', 'rhs_src_field' => 'person_id',  } },
		] },
		{ 'NODE_TYPE' => 'view_join', 'ATTRS' => { 'lhs_src' => 'self', 
				'rhs_src' => 'mother', 'join_op' => 'LEFT', }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_join_field', 'ATTRS' => { 'lhs_src_field' => 'mother_id', 'rhs_src_field' => 'person_id',  } },
		] },
	] } );

	$schema.build_child_node_tree( { 'NODE_TYPE' => 'table', 
			'ATTRS' => { 'si_name' => 'user_auth', 'row_data_type' => 'user_auth', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'table_field', 'ATTRS' => $_ } } (
			{ 'si_row_field' => 'user_id'      , 'mandatory' => 1, 'default_val' => 1, 'auto_inc' => 1, },
			{ 'si_row_field' => 'login_name'   , 'mandatory' => 1, },
			{ 'si_row_field' => 'login_pass'   , 'mandatory' => 1, },
			{ 'si_row_field' => 'private_name' , 'mandatory' => 1, },
			{ 'si_row_field' => 'private_email', 'mandatory' => 1, },
			{ 'si_row_field' => 'may_login'    , 'mandatory' => 1, },
			{ 'si_row_field' => 'max_sessions' , 'mandatory' => 1, 'default_val' => 3, },
		) ),
		( map { { 'NODE_TYPE' => 'table_index', 'ATTRS' => $_[0], 
				'CHILDREN' => { 'NODE_TYPE' => 'table_index_field', 'ATTRS' => $_[1] } } } (
			[ { 'si_name' => 'primary'         , 'index_type' => 'UNIQUE', }, 'user_id'       ],
			[ { 'si_name' => 'ak_login_name'   , 'index_type' => 'UNIQUE', }, 'login_name'    ],
			[ { 'si_name' => 'ak_private_email', 'index_type' => 'UNIQUE', }, 'private_email' ],
		) ),
	] } );

	$schema.build_child_node_tree( { 'NODE_TYPE' => 'table', 
			'ATTRS' => { 'si_name' => 'user_profile', 'row_data_type' => 'user_profile', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'table_field', 'ATTRS' => $_ } } (
			{ 'si_row_field' => 'user_id'    , 'mandatory' => 1, },
			{ 'si_row_field' => 'public_name', 'mandatory' => 1, },
		) ),
		( map { { 'NODE_TYPE' => 'table_index', 'ATTRS' => $_[0], 
				'CHILDREN' => { 'NODE_TYPE' => 'table_index_field', 'ATTRS' => $_[1] } } } (
			[ { 'si_name' => 'primary'       , 'index_type' => 'UNIQUE', }, 'user_id'     ],
			[ { 'si_name' => 'ak_public_name', 'index_type' => 'UNIQUE', }, 'public_name' ],
			[ { 'si_name' => 'fk_user', 'index_type' => 'FOREIGN', 'f_table' => 'user_auth', }, 
				{ 'si_field' => 'user_id', 'f_field' => 'user_id' } ], 
		) ),
	] } );

	$schema.build_child_node_tree( { 'NODE_TYPE' => 'view', 
			'ATTRS' => { 'si_name' => 'user', 'view_type' => 'JOINED', 'row_data_type' => 'user', }, 'CHILDREN' => [ 
		{ 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'si_name' => 'user_auth', 
				'match' => 'user_auth', }, 'CHILDREN' => [ 
			( map { { 'NODE_TYPE' => 'view_src_field', 'ATTRS' => $_ } } qw(
				user_id login_name login_pass private_name private_email may_login max_sessions
			) ),
		] },
		{ 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'si_name' => 'user_profile', 
				'match' => 'user_profile', }, 'CHILDREN' => [ 
			( map { { 'NODE_TYPE' => 'view_src_field', 'ATTRS' => $_ } } qw(
				user_id public_name public_email web_url contact_net contact_phy bio plan comments
			) ),
		] },
		( map { { 'NODE_TYPE' => 'view_field', 'ATTRS' => $_ } } (
			{ 'si_row_field' => 'user_id'      , 'src_field' => ['user_id','user_auth'], },
			{ 'si_row_field' => 'login_name'   , 'src_field' => 'login_name'   , },
			{ 'si_row_field' => 'login_pass'   , 'src_field' => 'login_pass'   , },
			{ 'si_row_field' => 'private_name' , 'src_field' => 'private_name' , },
			{ 'si_row_field' => 'private_email', 'src_field' => 'private_email', },
			{ 'si_row_field' => 'may_login'    , 'src_field' => 'may_login'    , },
			{ 'si_row_field' => 'max_sessions' , 'src_field' => 'max_sessions' , },
			{ 'si_row_field' => 'public_name'  , 'src_field' => 'public_name'  , },
			{ 'si_row_field' => 'public_email' , 'src_field' => 'public_email' , },
			{ 'si_row_field' => 'web_url'      , 'src_field' => 'web_url'      , },
			{ 'si_row_field' => 'contact_net'  , 'src_field' => 'contact_net'  , },
			{ 'si_row_field' => 'contact_phy'  , 'src_field' => 'contact_phy'  , },
			{ 'si_row_field' => 'bio'          , 'src_field' => 'bio'          , },
			{ 'si_row_field' => 'plan'         , 'src_field' => 'plan'         , },
			{ 'si_row_field' => 'comments'     , 'src_field' => 'comments'     , },
		) ),
		{ 'NODE_TYPE' => 'view_join', 'ATTRS' => { 'lhs_src' => 'user_auth', 
				'rhs_src' => 'user_profile', 'join_op' => 'LEFT', }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_join_field', 'ATTRS' => { 'lhs_src_field' => 'user_id', 'rhs_src_field' => 'user_id',  } },
		] },
	] } );

	$schema.build_child_node_tree( { 'NODE_TYPE' => 'table', 
			'ATTRS' => { 'si_name' => 'user_pref', 'row_data_type' => 'user_pref', }, 'CHILDREN' => [ 
		( map { { 'NODE_TYPE' => 'table_field', 'ATTRS' => $_ } } (
			{ 'si_row_field' => 'user_id'  , 'mandatory' => 1, },
			{ 'si_row_field' => 'pref_name', 'mandatory' => 1, },
		) ),
		( map { { 'NODE_TYPE' => 'table_index', 'ATTRS' => $_[0], 'CHILDREN' => [ 
				map { { 'NODE_TYPE' => 'table_index_field', 'ATTRS' => $_ } } @{$_[1]}
				] } } (
			[ { 'si_name' => 'primary', 'index_type' => 'UNIQUE', }, 
				[ 'user_id', 'pref_name', ], ], 
			[ { 'si_name' => 'fk_user', 'index_type' => 'FOREIGN', 'f_table' => 'user_auth', }, 
				[ { 'si_field' => 'user_id', 'f_field' => 'user_id' }, ], ], 
		) ),
	] } );

	##### NEXT SET APPLICATION BLUEPRINT-TYPE DETAILS #####

	my $application = $model.build_child_node_tree( { 'NODE_TYPE' => 'application', 
		'ATTRS' => { 'si_name' => 'My App', }, } ); 

	$application.build_child_node_tree( { 'NODE_TYPE' => 'view', 
			'ATTRS' => { 'si_name' => 'user_theme', 'view_type' => 'JOINED', 'row_data_type' => 'user_theme', }, 'CHILDREN' => [ 
		{ 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'si_name' => 'user_pref', 'match' => 'user_pref', }, 
			'CHILDREN' => [ map { { 'NODE_TYPE' => 'view_src_field', 'ATTRS' => $_ } } qw( pref_name pref_value ) ] 
		},
		{ 'NODE_TYPE' => 'view_field', 'ATTRS' => { 'si_row_field' => 'theme_name', 'src_field' => 'pref_value', }, },
		{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'RESULT', 
				'set_result_field' => 'theme_count', 'cont_type' => 'SCALAR', 'valf_call_sroutine' => 'COUNT', }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
				'cont_type' => 'SCALAR', 'valf_src_field' => 'pref_value', }, },
		] },
		{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'WHERE', 
				'cont_type' => 'SCALAR', 'valf_call_sroutine' => 'EQ', }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
				'cont_type' => 'SCALAR', 'valf_src_field' => 'pref_name', }, },
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
				'cont_type' => 'SCALAR', 'scalar_data_type' => 'str30', 'valf_literal' => 'theme', }, },
		] },
		{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'GROUP', 
			'cont_type' => 'SCALAR', 'valf_src_field' => 'pref_value', }, },
		{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'HAVING', 
				'cont_type' => 'SCALAR', 'valf_call_sroutine' => 'GT', }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
				'cont_type' => 'SCALAR', 'valf_call_sroutine' => 'COUNT', }, },
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
				'cont_type' => 'SCALAR', 'scalar_data_type' => 'int', 'valf_literal' => '1', }, },
		] },
		{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'ORDER', 
			'cont_type' => 'SCALAR', 'valf_result_field' => 'theme_count', }, },
		{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'ORDER', 
			'cont_type' => 'SCALAR', 'valf_result_field' => 'theme_name', }, },
	] } );

	$application.build_child_node_tree( { 'NODE_TYPE' => 'routine', 
			'ATTRS' => { 'routine_type' => 'FUNCTION', 'si_name' => 'get_user', 
			'return_cont_type' => 'CURSOR', }, 'CHILDREN' => [ 
		{ 'NODE_TYPE' => 'routine_arg', 'ATTRS' => { 'si_name' => 'curr_uid', 
			'cont_type' => 'SCALAR', 'scalar_data_type' => 'int', }, },
		{ 'NODE_TYPE' => 'view', 'ATTRS' => { 'si_name' => 'get_user', 
				'view_type' => 'JOINED', 'row_data_type' => 'user', }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'si_name' => 'm', 'match' => 'user', }, 
				'CHILDREN' => [ map { { 'NODE_TYPE' => 'view_src_field', 'ATTRS' => $_ } } qw( user_id login_name ) ] 
			},
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'WHERE', 
					'cont_type' => 'SCALAR', 'valf_call_sroutine' => 'EQ', }, 'CHILDREN' => [ 
				{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
					'cont_type' => 'SCALAR', 'valf_src_field' => 'user_id', }, },
				{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
					'cont_type' => 'SCALAR', 'valf_p_routine_item' => 'curr_uid', }, },
			] },
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'ORDER', 
				'cont_type' => 'SCALAR', 'valf_src_field' => 'login_name', }, },
		] },
		{ 'NODE_TYPE' => 'routine_stmt', 'ATTRS' => { 'call_sroutine' => 'CURSOR_OPEN' }, },
	] } );

	$application.build_child_node_tree( { 'NODE_TYPE' => 'routine', 
			'ATTRS' => { 'routine_type' => 'FUNCTION', 'si_name' => 'get_pwp', 
			'return_cont_type' => 'CURSOR', }, 'CHILDREN' => [ 
		{ 'NODE_TYPE' => 'routine_arg', 'ATTRS' => { 'si_name' => 'srchw_fa', 'cont_type' => 'SCALAR', 'scalar_data_type' => 'str30', }, },
		{ 'NODE_TYPE' => 'routine_arg', 'ATTRS' => { 'si_name' => 'srchw_mo', 'cont_type' => 'SCALAR', 'scalar_data_type' => 'str30', }, },
		{ 'NODE_TYPE' => 'view', 'ATTRS' => { 'si_name' => 'get_pwp', 
				'view_type' => 'JOINED', 'row_data_type' => 'person_with_parents', }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'si_name' => 'm', 'match' => 'person_with_parents', }, 
				'CHILDREN' => [ map { { 'NODE_TYPE' => 'view_src_field', 'ATTRS' => $_ } } qw( self_name father_name mother_name ) ] 
			},
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'WHERE', 
					'cont_type' => 'SCALAR', 'valf_call_sroutine' => 'AND', }, 'CHILDREN' => [ 
				{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
						'cont_type' => 'SCALAR', 'valf_call_sroutine' => 'LIKE', }, 'CHILDREN' => [ 
					{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
						'cont_type' => 'SCALAR', 'valf_src_field' => 'father_name', }, },
					{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
						'cont_type' => 'SCALAR', 'valf_p_routine_item' => 'srchw_fa', }, },
				] },
				{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
						'cont_type' => 'SCALAR', 'valf_call_sroutine' => 'LIKE', }, 'CHILDREN' => [ 
					{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
						'cont_type' => 'SCALAR', 'valf_src_field' => 'mother_name', }, },
					{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 
						'cont_type' => 'SCALAR', 'valf_p_routine_item' => 'srchw_mo', }, },
				] },
			] },
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'ORDER', 
				'cont_type' => 'SCALAR', 'valf_src_field' => 'self_name', }, },
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'ORDER', 
				'cont_type' => 'SCALAR', 'valf_src_field' => 'father_name', }, },
			{ 'NODE_TYPE' => 'view_expr', 'ATTRS' => { 'view_part' => 'ORDER', 
				'cont_type' => 'SCALAR', 'valf_src_field' => 'mother_name', }, },
		] },
		{ 'NODE_TYPE' => 'routine_stmt', 'ATTRS' => { 'call_sroutine' => 'CURSOR_OPEN' }, },
	] } );

	$application.build_child_node_tree( { 'NODE_TYPE' => 'routine', 
			'ATTRS' => { 'routine_type' => 'FUNCTION', 'si_name' => 'get_theme', 
			'return_cont_type' => 'CURSOR', }, 'CHILDREN' => [ 
		{ 'NODE_TYPE' => 'view', 'ATTRS' => { 'si_name' => 'get_theme', 
				'view_type' => 'ALIAS', 'row_data_type' => 'user_theme', }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'si_name' => 'm', 'match' => 'user_theme', }, },
		] },
		{ 'NODE_TYPE' => 'routine_stmt', 'ATTRS' => { 'call_sroutine' => 'CURSOR_OPEN' }, },
	] } );

	$application.build_child_node_tree( { 'NODE_TYPE' => 'routine', 
			'ATTRS' => { 'routine_type' => 'FUNCTION', 'si_name' => 'get_person', 
			'return_cont_type' => 'CURSOR', }, 'CHILDREN' => [ 
		{ 'NODE_TYPE' => 'view', 'ATTRS' => { 'si_name' => 'get_person', 
				'view_type' => 'ALIAS', 'row_data_type' => 'person', }, 'CHILDREN' => [ 
			{ 'NODE_TYPE' => 'view_src', 'ATTRS' => { 'si_name' => 'person', 'match' => 'person', }, },
		] },
		{ 'NODE_TYPE' => 'routine_stmt', 'ATTRS' => { 'call_sroutine' => 'CURSOR_OPEN' }, },
	] } );

	##### NEXT SET PRODUCT-TYPE DETAILS #####

	# ... TODO ...

	##### NEXT SET INSTANCE-TYPE DETAILS #####

	my $app_inst = $model.build_child_node_tree( { 'NODE_TYPE' => 'application_instance', 
		'ATTRS' => { 'si_name' => 'My App Instance', 'blueprint' => 'My App', }, } ); 

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
		<row_data_type id="30" si_name="person">
			<row_data_type_field id="31" si_name="person_id" scalar_data_type="9" />
			<row_data_type_field id="32" si_name="alternate_id" scalar_data_type="23" />
			<row_data_type_field id="33" si_name="name" scalar_data_type="24" />
			<row_data_type_field id="34" si_name="sex" scalar_data_type="22" />
			<row_data_type_field id="35" si_name="father_id" scalar_data_type="9" />
			<row_data_type_field id="36" si_name="mother_id" scalar_data_type="9" />
		</row_data_type>
		<row_data_type id="37" si_name="person_with_parents">
			<row_data_type_field id="38" si_name="self_id" scalar_data_type="9" />
			<row_data_type_field id="39" si_name="self_name" scalar_data_type="24" />
			<row_data_type_field id="40" si_name="father_id" scalar_data_type="9" />
			<row_data_type_field id="41" si_name="father_name" scalar_data_type="24" />
			<row_data_type_field id="42" si_name="mother_id" scalar_data_type="9" />
			<row_data_type_field id="43" si_name="mother_name" scalar_data_type="24" />
		</row_data_type>
		<row_data_type id="44" si_name="user_auth">
			<row_data_type_field id="45" si_name="user_id" scalar_data_type="9" />
			<row_data_type_field id="46" si_name="login_name" scalar_data_type="23" />
			<row_data_type_field id="47" si_name="login_pass" scalar_data_type="23" />
			<row_data_type_field id="48" si_name="private_name" scalar_data_type="24" />
			<row_data_type_field id="49" si_name="private_email" scalar_data_type="24" />
			<row_data_type_field id="50" si_name="may_login" scalar_data_type="19" />
			<row_data_type_field id="51" si_name="max_sessions" scalar_data_type="7" />
		</row_data_type>
		<row_data_type id="52" si_name="user_profile">
			<row_data_type_field id="53" si_name="user_id" scalar_data_type="9" />
			<row_data_type_field id="54" si_name="public_name" scalar_data_type="25" />
			<row_data_type_field id="55" si_name="public_email" scalar_data_type="25" />
			<row_data_type_field id="56" si_name="web_url" scalar_data_type="25" />
			<row_data_type_field id="57" si_name="contact_net" scalar_data_type="25" />
			<row_data_type_field id="58" si_name="contact_phy" scalar_data_type="25" />
			<row_data_type_field id="59" si_name="bio" scalar_data_type="25" />
			<row_data_type_field id="60" si_name="plan" scalar_data_type="25" />
			<row_data_type_field id="61" si_name="comments" scalar_data_type="25" />
		</row_data_type>
		<row_data_type id="62" si_name="user">
			<row_data_type_field id="63" si_name="user_id" scalar_data_type="9" />
			<row_data_type_field id="64" si_name="login_name" scalar_data_type="23" />
			<row_data_type_field id="65" si_name="login_pass" scalar_data_type="23" />
			<row_data_type_field id="66" si_name="private_name" scalar_data_type="24" />
			<row_data_type_field id="67" si_name="private_email" scalar_data_type="24" />
			<row_data_type_field id="68" si_name="may_login" scalar_data_type="19" />
			<row_data_type_field id="69" si_name="max_sessions" scalar_data_type="7" />
			<row_data_type_field id="70" si_name="public_name" scalar_data_type="25" />
			<row_data_type_field id="71" si_name="public_email" scalar_data_type="25" />
			<row_data_type_field id="72" si_name="web_url" scalar_data_type="25" />
			<row_data_type_field id="73" si_name="contact_net" scalar_data_type="25" />
			<row_data_type_field id="74" si_name="contact_phy" scalar_data_type="25" />
			<row_data_type_field id="75" si_name="bio" scalar_data_type="25" />
			<row_data_type_field id="76" si_name="plan" scalar_data_type="25" />
			<row_data_type_field id="77" si_name="comments" scalar_data_type="25" />
		</row_data_type>
		<row_data_type id="78" si_name="user_pref">
			<row_data_type_field id="79" si_name="user_id" scalar_data_type="9" />
			<row_data_type_field id="80" si_name="pref_name" scalar_data_type="26" />
			<row_data_type_field id="81" si_name="pref_value" scalar_data_type="27" />
		</row_data_type>
		<row_data_type id="82" si_name="user_theme">
			<row_data_type_field id="83" si_name="theme_name" scalar_data_type="27" />
			<row_data_type_field id="84" si_name="theme_count" scalar_data_type="9" />
		</row_data_type>
	</elements>
	<blueprints>
		<catalog id="85" si_name="The Catalog Blueprint">
			<owner id="86" si_name="Gene\'s Owner" />
			<schema id="87" si_name="gene" owner="86">
				<table id="88" si_name="person" row_data_type="30">
					<table_field id="89" si_row_field="31" mandatory="1" default_val="1" auto_inc="1" />
					<table_field id="90" si_row_field="33" mandatory="1" />
					<table_index id="91" si_name="primary" index_type="UNIQUE">
						<table_index_field id="92" si_field="31" />
					</table_index>
					<table_index id="93" si_name="ak_alternate_id" index_type="UNIQUE">
						<table_index_field id="94" si_field="32" />
					</table_index>
					<table_index id="95" si_name="fk_father" index_type="FOREIGN" f_table="88">
						<table_index_field id="96" si_field="35" f_field="31" />
					</table_index>
					<table_index id="97" si_name="fk_mother" index_type="FOREIGN" f_table="88">
						<table_index_field id="98" si_field="36" f_field="31" />
					</table_index>
				</table>
				<view id="99" si_name="person_with_parents" view_type="JOINED" row_data_type="37">
					<view_src id="100" si_name="self" match="88">
						<view_src_field id="101" si_match_field="31" />
						<view_src_field id="102" si_match_field="33" />
						<view_src_field id="103" si_match_field="35" />
						<view_src_field id="104" si_match_field="36" />
					</view_src>
					<view_src id="105" si_name="father" match="88">
						<view_src_field id="106" si_match_field="31" />
						<view_src_field id="107" si_match_field="33" />
					</view_src>
					<view_src id="108" si_name="mother" match="88">
						<view_src_field id="109" si_match_field="31" />
						<view_src_field id="110" si_match_field="33" />
					</view_src>
					<view_field id="111" si_row_field="38" src_field="101" />
					<view_field id="112" si_row_field="39" src_field="102" />
					<view_field id="113" si_row_field="40" src_field="106" />
					<view_field id="114" si_row_field="41" src_field="107" />
					<view_field id="115" si_row_field="42" src_field="109" />
					<view_field id="116" si_row_field="43" src_field="110" />
					<view_join id="117" lhs_src="100" rhs_src="105" join_op="LEFT">
						<view_join_field id="118" lhs_src_field="103" rhs_src_field="106" />
					</view_join>
					<view_join id="119" lhs_src="100" rhs_src="108" join_op="LEFT">
						<view_join_field id="120" lhs_src_field="104" rhs_src_field="109" />
					</view_join>
				</view>
				<table id="121" si_name="user_auth" row_data_type="44">
					<table_field id="122" si_row_field="45" mandatory="1" default_val="1" auto_inc="1" />
					<table_field id="123" si_row_field="46" mandatory="1" />
					<table_field id="124" si_row_field="47" mandatory="1" />
					<table_field id="125" si_row_field="48" mandatory="1" />
					<table_field id="126" si_row_field="49" mandatory="1" />
					<table_field id="127" si_row_field="50" mandatory="1" />
					<table_field id="128" si_row_field="51" mandatory="1" default_val="3" />
					<table_index id="129" si_name="primary" index_type="UNIQUE">
						<table_index_field id="130" si_field="45" />
					</table_index>
					<table_index id="131" si_name="ak_login_name" index_type="UNIQUE">
						<table_index_field id="132" si_field="46" />
					</table_index>
					<table_index id="133" si_name="ak_private_email" index_type="UNIQUE">
						<table_index_field id="134" si_field="49" />
					</table_index>
				</table>
				<table id="135" si_name="user_profile" row_data_type="52">
					<table_field id="136" si_row_field="53" mandatory="1" />
					<table_field id="137" si_row_field="54" mandatory="1" />
					<table_index id="138" si_name="primary" index_type="UNIQUE">
						<table_index_field id="139" si_field="53" />
					</table_index>
					<table_index id="140" si_name="ak_public_name" index_type="UNIQUE">
						<table_index_field id="141" si_field="54" />
					</table_index>
					<table_index id="142" si_name="fk_user" index_type="FOREIGN" f_table="121">
						<table_index_field id="143" si_field="53" f_field="45" />
					</table_index>
				</table>
				<view id="144" si_name="user" view_type="JOINED" row_data_type="62">
					<view_src id="145" si_name="user_auth" match="121">
						<view_src_field id="146" si_match_field="45" />
						<view_src_field id="147" si_match_field="46" />
						<view_src_field id="148" si_match_field="47" />
						<view_src_field id="149" si_match_field="48" />
						<view_src_field id="150" si_match_field="49" />
						<view_src_field id="151" si_match_field="50" />
						<view_src_field id="152" si_match_field="51" />
					</view_src>
					<view_src id="153" si_name="user_profile" match="135">
						<view_src_field id="154" si_match_field="53" />
						<view_src_field id="155" si_match_field="54" />
						<view_src_field id="156" si_match_field="55" />
						<view_src_field id="157" si_match_field="56" />
						<view_src_field id="158" si_match_field="57" />
						<view_src_field id="159" si_match_field="58" />
						<view_src_field id="160" si_match_field="59" />
						<view_src_field id="161" si_match_field="60" />
						<view_src_field id="162" si_match_field="61" />
					</view_src>
					<view_field id="163" si_row_field="63" src_field="146" />
					<view_field id="164" si_row_field="64" src_field="147" />
					<view_field id="165" si_row_field="65" src_field="148" />
					<view_field id="166" si_row_field="66" src_field="149" />
					<view_field id="167" si_row_field="67" src_field="150" />
					<view_field id="168" si_row_field="68" src_field="151" />
					<view_field id="169" si_row_field="69" src_field="152" />
					<view_field id="170" si_row_field="70" src_field="155" />
					<view_field id="171" si_row_field="71" src_field="156" />
					<view_field id="172" si_row_field="72" src_field="157" />
					<view_field id="173" si_row_field="73" src_field="158" />
					<view_field id="174" si_row_field="74" src_field="159" />
					<view_field id="175" si_row_field="75" src_field="160" />
					<view_field id="176" si_row_field="76" src_field="161" />
					<view_field id="177" si_row_field="77" src_field="162" />
					<view_join id="178" lhs_src="145" rhs_src="153" join_op="LEFT">
						<view_join_field id="179" lhs_src_field="146" rhs_src_field="154" />
					</view_join>
				</view>
				<table id="180" si_name="user_pref" row_data_type="78">
					<table_field id="181" si_row_field="79" mandatory="1" />
					<table_field id="182" si_row_field="80" mandatory="1" />
					<table_index id="183" si_name="primary" index_type="UNIQUE">
						<table_index_field id="184" si_field="79" />
						<table_index_field id="185" si_field="80" />
					</table_index>
					<table_index id="186" si_name="fk_user" index_type="FOREIGN" f_table="121">
						<table_index_field id="187" si_field="79" f_field="45" />
					</table_index>
				</table>
			</schema>
		</catalog>
		<application id="188" si_name="My App">
			<view id="189" si_name="user_theme" view_type="JOINED" row_data_type="82">
				<view_src id="190" si_name="user_pref" match="180">
					<view_src_field id="191" si_match_field="80" />
					<view_src_field id="192" si_match_field="81" />
				</view_src>
				<view_field id="193" si_row_field="83" src_field="192" />
				<view_expr id="194" view_part="RESULT" set_result_field="84" cont_type="SCALAR" valf_call_sroutine="COUNT">
					<view_expr id="195" cont_type="SCALAR" valf_src_field="192" />
				</view_expr>
				<view_expr id="196" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="EQ">
					<view_expr id="197" cont_type="SCALAR" valf_src_field="191" />
					<view_expr id="198" cont_type="SCALAR" valf_literal="theme" scalar_data_type="5" />
				</view_expr>
				<view_expr id="199" view_part="GROUP" cont_type="SCALAR" valf_src_field="192" />
				<view_expr id="200" view_part="HAVING" cont_type="SCALAR" valf_call_sroutine="GT">
					<view_expr id="201" cont_type="SCALAR" valf_call_sroutine="COUNT" />
					<view_expr id="202" cont_type="SCALAR" valf_literal="1" scalar_data_type="9" />
				</view_expr>
				<view_expr id="203" view_part="ORDER" cont_type="SCALAR" valf_result_field="84" />
				<view_expr id="204" view_part="ORDER" cont_type="SCALAR" valf_result_field="83" />
			</view>
			<routine id="205" si_name="get_user" routine_type="FUNCTION" return_cont_type="CURSOR">
				<routine_arg id="206" si_name="curr_uid" cont_type="SCALAR" scalar_data_type="9" />
				<view id="207" si_name="get_user" view_type="JOINED" row_data_type="62">
					<view_src id="208" si_name="m" match="144">
						<view_src_field id="209" si_match_field="63" />
						<view_src_field id="210" si_match_field="64" />
					</view_src>
					<view_expr id="211" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="EQ">
						<view_expr id="212" cont_type="SCALAR" valf_src_field="209" />
						<view_expr id="213" cont_type="SCALAR" valf_p_routine_item="206" />
					</view_expr>
					<view_expr id="214" view_part="ORDER" cont_type="SCALAR" valf_src_field="210" />
				</view>
				<routine_stmt id="215" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="216" si_name="get_pwp" routine_type="FUNCTION" return_cont_type="CURSOR">
				<routine_arg id="217" si_name="srchw_fa" cont_type="SCALAR" scalar_data_type="5" />
				<routine_arg id="218" si_name="srchw_mo" cont_type="SCALAR" scalar_data_type="5" />
				<view id="219" si_name="get_pwp" view_type="JOINED" row_data_type="37">
					<view_src id="220" si_name="m" match="99">
						<view_src_field id="221" si_match_field="39" />
						<view_src_field id="222" si_match_field="41" />
						<view_src_field id="223" si_match_field="43" />
					</view_src>
					<view_expr id="224" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="AND">
						<view_expr id="225" cont_type="SCALAR" valf_call_sroutine="LIKE">
							<view_expr id="226" cont_type="SCALAR" valf_src_field="222" />
							<view_expr id="227" cont_type="SCALAR" valf_p_routine_item="217" />
						</view_expr>
						<view_expr id="228" cont_type="SCALAR" valf_call_sroutine="LIKE">
							<view_expr id="229" cont_type="SCALAR" valf_src_field="223" />
							<view_expr id="230" cont_type="SCALAR" valf_p_routine_item="218" />
						</view_expr>
					</view_expr>
					<view_expr id="231" view_part="ORDER" cont_type="SCALAR" valf_src_field="221" />
					<view_expr id="232" view_part="ORDER" cont_type="SCALAR" valf_src_field="222" />
					<view_expr id="233" view_part="ORDER" cont_type="SCALAR" valf_src_field="223" />
				</view>
				<routine_stmt id="234" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="235" si_name="get_theme" routine_type="FUNCTION" return_cont_type="CURSOR">
				<view id="236" si_name="get_theme" view_type="ALIAS" row_data_type="82">
					<view_src id="237" si_name="m" match="189" />
				</view>
				<routine_stmt id="238" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="239" si_name="get_person" routine_type="FUNCTION" return_cont_type="CURSOR">
				<view id="240" si_name="get_person" view_type="ALIAS" row_data_type="30">
					<view_src id="241" si_name="person" match="88" />
				</view>
				<routine_stmt id="242" call_sroutine="CURSOR_OPEN" />
			</routine>
		</application>
	</blueprints>
	<tools />
	<sites>
		<application_instance id="243" si_name="My App Instance" blueprint="188" />
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
		<row_data_type id="30" si_name="person">
			<row_data_type_field id="31" si_name="person_id" scalar_data_type="int" />
			<row_data_type_field id="32" si_name="alternate_id" scalar_data_type="str20" />
			<row_data_type_field id="33" si_name="name" scalar_data_type="str100" />
			<row_data_type_field id="34" si_name="sex" scalar_data_type="sex" />
			<row_data_type_field id="35" si_name="father_id" scalar_data_type="int" />
			<row_data_type_field id="36" si_name="mother_id" scalar_data_type="int" />
		</row_data_type>
		<row_data_type id="37" si_name="person_with_parents">
			<row_data_type_field id="38" si_name="self_id" scalar_data_type="int" />
			<row_data_type_field id="39" si_name="self_name" scalar_data_type="str100" />
			<row_data_type_field id="40" si_name="father_id" scalar_data_type="int" />
			<row_data_type_field id="41" si_name="father_name" scalar_data_type="str100" />
			<row_data_type_field id="42" si_name="mother_id" scalar_data_type="int" />
			<row_data_type_field id="43" si_name="mother_name" scalar_data_type="str100" />
		</row_data_type>
		<row_data_type id="44" si_name="user_auth">
			<row_data_type_field id="45" si_name="user_id" scalar_data_type="int" />
			<row_data_type_field id="46" si_name="login_name" scalar_data_type="str20" />
			<row_data_type_field id="47" si_name="login_pass" scalar_data_type="str20" />
			<row_data_type_field id="48" si_name="private_name" scalar_data_type="str100" />
			<row_data_type_field id="49" si_name="private_email" scalar_data_type="str100" />
			<row_data_type_field id="50" si_name="may_login" scalar_data_type="boolean" />
			<row_data_type_field id="51" si_name="max_sessions" scalar_data_type="byte" />
		</row_data_type>
		<row_data_type id="52" si_name="user_profile">
			<row_data_type_field id="53" si_name="user_id" scalar_data_type="int" />
			<row_data_type_field id="54" si_name="public_name" scalar_data_type="str250" />
			<row_data_type_field id="55" si_name="public_email" scalar_data_type="str250" />
			<row_data_type_field id="56" si_name="web_url" scalar_data_type="str250" />
			<row_data_type_field id="57" si_name="contact_net" scalar_data_type="str250" />
			<row_data_type_field id="58" si_name="contact_phy" scalar_data_type="str250" />
			<row_data_type_field id="59" si_name="bio" scalar_data_type="str250" />
			<row_data_type_field id="60" si_name="plan" scalar_data_type="str250" />
			<row_data_type_field id="61" si_name="comments" scalar_data_type="str250" />
		</row_data_type>
		<row_data_type id="62" si_name="user">
			<row_data_type_field id="63" si_name="user_id" scalar_data_type="int" />
			<row_data_type_field id="64" si_name="login_name" scalar_data_type="str20" />
			<row_data_type_field id="65" si_name="login_pass" scalar_data_type="str20" />
			<row_data_type_field id="66" si_name="private_name" scalar_data_type="str100" />
			<row_data_type_field id="67" si_name="private_email" scalar_data_type="str100" />
			<row_data_type_field id="68" si_name="may_login" scalar_data_type="boolean" />
			<row_data_type_field id="69" si_name="max_sessions" scalar_data_type="byte" />
			<row_data_type_field id="70" si_name="public_name" scalar_data_type="str250" />
			<row_data_type_field id="71" si_name="public_email" scalar_data_type="str250" />
			<row_data_type_field id="72" si_name="web_url" scalar_data_type="str250" />
			<row_data_type_field id="73" si_name="contact_net" scalar_data_type="str250" />
			<row_data_type_field id="74" si_name="contact_phy" scalar_data_type="str250" />
			<row_data_type_field id="75" si_name="bio" scalar_data_type="str250" />
			<row_data_type_field id="76" si_name="plan" scalar_data_type="str250" />
			<row_data_type_field id="77" si_name="comments" scalar_data_type="str250" />
		</row_data_type>
		<row_data_type id="78" si_name="user_pref">
			<row_data_type_field id="79" si_name="user_id" scalar_data_type="int" />
			<row_data_type_field id="80" si_name="pref_name" scalar_data_type="entitynm" />
			<row_data_type_field id="81" si_name="pref_value" scalar_data_type="generic" />
		</row_data_type>
		<row_data_type id="82" si_name="user_theme">
			<row_data_type_field id="83" si_name="theme_name" scalar_data_type="generic" />
			<row_data_type_field id="84" si_name="theme_count" scalar_data_type="int" />
		</row_data_type>
	</elements>
	<blueprints>
		<catalog id="85" si_name="The Catalog Blueprint">
			<owner id="86" si_name="Gene\'s Owner" />
			<schema id="87" si_name="gene" owner="Gene\'s Owner">
				<table id="88" si_name="person" row_data_type="person">
					<table_field id="89" si_row_field="person_id" mandatory="1" default_val="1" auto_inc="1" />
					<table_field id="90" si_row_field="name" mandatory="1" />
					<table_index id="91" si_name="primary" index_type="UNIQUE">
						<table_index_field id="92" si_field="person_id" />
					</table_index>
					<table_index id="93" si_name="ak_alternate_id" index_type="UNIQUE">
						<table_index_field id="94" si_field="alternate_id" />
					</table_index>
					<table_index id="95" si_name="fk_father" index_type="FOREIGN" f_table="person">
						<table_index_field id="96" si_field="father_id" f_field="person_id" />
					</table_index>
					<table_index id="97" si_name="fk_mother" index_type="FOREIGN" f_table="person">
						<table_index_field id="98" si_field="mother_id" f_field="person_id" />
					</table_index>
				</table>
				<view id="99" si_name="person_with_parents" view_type="JOINED" row_data_type="person_with_parents">
					<view_src id="100" si_name="self" match="person">
						<view_src_field id="101" si_match_field="person_id" />
						<view_src_field id="102" si_match_field="name" />
						<view_src_field id="103" si_match_field="father_id" />
						<view_src_field id="104" si_match_field="mother_id" />
					</view_src>
					<view_src id="105" si_name="father" match="person">
						<view_src_field id="106" si_match_field="person_id" />
						<view_src_field id="107" si_match_field="name" />
					</view_src>
					<view_src id="108" si_name="mother" match="person">
						<view_src_field id="109" si_match_field="person_id" />
						<view_src_field id="110" si_match_field="name" />
					</view_src>
					<view_field id="111" si_row_field="self_id" src_field="[person_id,self]" />
					<view_field id="112" si_row_field="self_name" src_field="[name,self]" />
					<view_field id="113" si_row_field="father_id" src_field="[person_id,father]" />
					<view_field id="114" si_row_field="father_name" src_field="[name,father]" />
					<view_field id="115" si_row_field="mother_id" src_field="[person_id,mother]" />
					<view_field id="116" si_row_field="mother_name" src_field="[name,mother]" />
					<view_join id="117" lhs_src="self" rhs_src="father" join_op="LEFT">
						<view_join_field id="118" lhs_src_field="father_id" rhs_src_field="person_id" />
					</view_join>
					<view_join id="119" lhs_src="self" rhs_src="mother" join_op="LEFT">
						<view_join_field id="120" lhs_src_field="mother_id" rhs_src_field="person_id" />
					</view_join>
				</view>
				<table id="121" si_name="user_auth" row_data_type="user_auth">
					<table_field id="122" si_row_field="user_id" mandatory="1" default_val="1" auto_inc="1" />
					<table_field id="123" si_row_field="login_name" mandatory="1" />
					<table_field id="124" si_row_field="login_pass" mandatory="1" />
					<table_field id="125" si_row_field="private_name" mandatory="1" />
					<table_field id="126" si_row_field="private_email" mandatory="1" />
					<table_field id="127" si_row_field="may_login" mandatory="1" />
					<table_field id="128" si_row_field="max_sessions" mandatory="1" default_val="3" />
					<table_index id="129" si_name="primary" index_type="UNIQUE">
						<table_index_field id="130" si_field="user_id" />
					</table_index>
					<table_index id="131" si_name="ak_login_name" index_type="UNIQUE">
						<table_index_field id="132" si_field="login_name" />
					</table_index>
					<table_index id="133" si_name="ak_private_email" index_type="UNIQUE">
						<table_index_field id="134" si_field="private_email" />
					</table_index>
				</table>
				<table id="135" si_name="user_profile" row_data_type="user_profile">
					<table_field id="136" si_row_field="user_id" mandatory="1" />
					<table_field id="137" si_row_field="public_name" mandatory="1" />
					<table_index id="138" si_name="primary" index_type="UNIQUE">
						<table_index_field id="139" si_field="user_id" />
					</table_index>
					<table_index id="140" si_name="ak_public_name" index_type="UNIQUE">
						<table_index_field id="141" si_field="public_name" />
					</table_index>
					<table_index id="142" si_name="fk_user" index_type="FOREIGN" f_table="user_auth">
						<table_index_field id="143" si_field="user_id" f_field="user_id" />
					</table_index>
				</table>
				<view id="144" si_name="user" view_type="JOINED" row_data_type="user">
					<view_src id="145" si_name="user_auth" match="user_auth">
						<view_src_field id="146" si_match_field="user_id" />
						<view_src_field id="147" si_match_field="login_name" />
						<view_src_field id="148" si_match_field="login_pass" />
						<view_src_field id="149" si_match_field="private_name" />
						<view_src_field id="150" si_match_field="private_email" />
						<view_src_field id="151" si_match_field="may_login" />
						<view_src_field id="152" si_match_field="max_sessions" />
					</view_src>
					<view_src id="153" si_name="user_profile" match="user_profile">
						<view_src_field id="154" si_match_field="user_id" />
						<view_src_field id="155" si_match_field="public_name" />
						<view_src_field id="156" si_match_field="public_email" />
						<view_src_field id="157" si_match_field="web_url" />
						<view_src_field id="158" si_match_field="contact_net" />
						<view_src_field id="159" si_match_field="contact_phy" />
						<view_src_field id="160" si_match_field="bio" />
						<view_src_field id="161" si_match_field="plan" />
						<view_src_field id="162" si_match_field="comments" />
					</view_src>
					<view_field id="163" si_row_field="user_id" src_field="[user_id,user_auth]" />
					<view_field id="164" si_row_field="login_name" src_field="[login_name,user_auth]" />
					<view_field id="165" si_row_field="login_pass" src_field="[login_pass,user_auth]" />
					<view_field id="166" si_row_field="private_name" src_field="[private_name,user_auth]" />
					<view_field id="167" si_row_field="private_email" src_field="[private_email,user_auth]" />
					<view_field id="168" si_row_field="may_login" src_field="[may_login,user_auth]" />
					<view_field id="169" si_row_field="max_sessions" src_field="[max_sessions,user_auth]" />
					<view_field id="170" si_row_field="public_name" src_field="[public_name,user_profile]" />
					<view_field id="171" si_row_field="public_email" src_field="[public_email,user_profile]" />
					<view_field id="172" si_row_field="web_url" src_field="[web_url,user_profile]" />
					<view_field id="173" si_row_field="contact_net" src_field="[contact_net,user_profile]" />
					<view_field id="174" si_row_field="contact_phy" src_field="[contact_phy,user_profile]" />
					<view_field id="175" si_row_field="bio" src_field="[bio,user_profile]" />
					<view_field id="176" si_row_field="plan" src_field="[plan,user_profile]" />
					<view_field id="177" si_row_field="comments" src_field="[comments,user_profile]" />
					<view_join id="178" lhs_src="user_auth" rhs_src="user_profile" join_op="LEFT">
						<view_join_field id="179" lhs_src_field="user_id" rhs_src_field="user_id" />
					</view_join>
				</view>
				<table id="180" si_name="user_pref" row_data_type="user_pref">
					<table_field id="181" si_row_field="user_id" mandatory="1" />
					<table_field id="182" si_row_field="pref_name" mandatory="1" />
					<table_index id="183" si_name="primary" index_type="UNIQUE">
						<table_index_field id="184" si_field="user_id" />
						<table_index_field id="185" si_field="pref_name" />
					</table_index>
					<table_index id="186" si_name="fk_user" index_type="FOREIGN" f_table="user_auth">
						<table_index_field id="187" si_field="user_id" f_field="user_id" />
					</table_index>
				</table>
			</schema>
		</catalog>
		<application id="188" si_name="My App">
			<view id="189" si_name="user_theme" view_type="JOINED" row_data_type="user_theme">
				<view_src id="190" si_name="user_pref" match="[user_pref,gene,The Catalog Blueprint]">
					<view_src_field id="191" si_match_field="pref_name" />
					<view_src_field id="192" si_match_field="pref_value" />
				</view_src>
				<view_field id="193" si_row_field="theme_name" src_field="[pref_value,user_pref]" />
				<view_expr id="194" view_part="RESULT" set_result_field="theme_count" cont_type="SCALAR" valf_call_sroutine="COUNT">
					<view_expr id="195" cont_type="SCALAR" valf_src_field="[pref_value,user_pref]" />
				</view_expr>
				<view_expr id="196" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="EQ">
					<view_expr id="197" cont_type="SCALAR" valf_src_field="[pref_name,user_pref]" />
					<view_expr id="198" cont_type="SCALAR" valf_literal="theme" scalar_data_type="str30" />
				</view_expr>
				<view_expr id="199" view_part="GROUP" cont_type="SCALAR" valf_src_field="[pref_value,user_pref]" />
				<view_expr id="200" view_part="HAVING" cont_type="SCALAR" valf_call_sroutine="GT">
					<view_expr id="201" cont_type="SCALAR" valf_call_sroutine="COUNT" />
					<view_expr id="202" cont_type="SCALAR" valf_literal="1" scalar_data_type="int" />
				</view_expr>
				<view_expr id="203" view_part="ORDER" cont_type="SCALAR" valf_result_field="theme_count" />
				<view_expr id="204" view_part="ORDER" cont_type="SCALAR" valf_result_field="theme_name" />
			</view>
			<routine id="205" si_name="get_user" routine_type="FUNCTION" return_cont_type="CURSOR">
				<routine_arg id="206" si_name="curr_uid" cont_type="SCALAR" scalar_data_type="int" />
				<view id="207" si_name="get_user" view_type="JOINED" row_data_type="user">
					<view_src id="208" si_name="m" match="[user,gene,The Catalog Blueprint]">
						<view_src_field id="209" si_match_field="user_id" />
						<view_src_field id="210" si_match_field="login_name" />
					</view_src>
					<view_expr id="211" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="EQ">
						<view_expr id="212" cont_type="SCALAR" valf_src_field="[user_id,m]" />
						<view_expr id="213" cont_type="SCALAR" valf_p_routine_item="curr_uid" />
					</view_expr>
					<view_expr id="214" view_part="ORDER" cont_type="SCALAR" valf_src_field="[login_name,m]" />
				</view>
				<routine_stmt id="215" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="216" si_name="get_pwp" routine_type="FUNCTION" return_cont_type="CURSOR">
				<routine_arg id="217" si_name="srchw_fa" cont_type="SCALAR" scalar_data_type="str30" />
				<routine_arg id="218" si_name="srchw_mo" cont_type="SCALAR" scalar_data_type="str30" />
				<view id="219" si_name="get_pwp" view_type="JOINED" row_data_type="person_with_parents">
					<view_src id="220" si_name="m" match="[person_with_parents,gene,The Catalog Blueprint]">
						<view_src_field id="221" si_match_field="self_name" />
						<view_src_field id="222" si_match_field="father_name" />
						<view_src_field id="223" si_match_field="mother_name" />
					</view_src>
					<view_expr id="224" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="AND">
						<view_expr id="225" cont_type="SCALAR" valf_call_sroutine="LIKE">
							<view_expr id="226" cont_type="SCALAR" valf_src_field="[father_name,m]" />
							<view_expr id="227" cont_type="SCALAR" valf_p_routine_item="srchw_fa" />
						</view_expr>
						<view_expr id="228" cont_type="SCALAR" valf_call_sroutine="LIKE">
							<view_expr id="229" cont_type="SCALAR" valf_src_field="[mother_name,m]" />
							<view_expr id="230" cont_type="SCALAR" valf_p_routine_item="srchw_mo" />
						</view_expr>
					</view_expr>
					<view_expr id="231" view_part="ORDER" cont_type="SCALAR" valf_src_field="[self_name,m]" />
					<view_expr id="232" view_part="ORDER" cont_type="SCALAR" valf_src_field="[father_name,m]" />
					<view_expr id="233" view_part="ORDER" cont_type="SCALAR" valf_src_field="[mother_name,m]" />
				</view>
				<routine_stmt id="234" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="235" si_name="get_theme" routine_type="FUNCTION" return_cont_type="CURSOR">
				<view id="236" si_name="get_theme" view_type="ALIAS" row_data_type="user_theme">
					<view_src id="237" si_name="m" match="user_theme" />
				</view>
				<routine_stmt id="238" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="239" si_name="get_person" routine_type="FUNCTION" return_cont_type="CURSOR">
				<view id="240" si_name="get_person" view_type="ALIAS" row_data_type="person">
					<view_src id="241" si_name="person" match="[person,gene,The Catalog Blueprint]" />
				</view>
				<routine_stmt id="242" call_sroutine="CURSOR_OPEN" />
			</routine>
		</application>
	</blueprints>
	<tools />
	<sites>
		<application_instance id="243" si_name="My App Instance" blueprint="My App" />
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
		<row_data_type id="30" si_name="person">
			<row_data_type_field id="31" si_name="person_id" scalar_data_type="int" />
			<row_data_type_field id="32" si_name="alternate_id" scalar_data_type="str20" />
			<row_data_type_field id="33" si_name="name" scalar_data_type="str100" />
			<row_data_type_field id="34" si_name="sex" scalar_data_type="sex" />
			<row_data_type_field id="35" si_name="father_id" scalar_data_type="int" />
			<row_data_type_field id="36" si_name="mother_id" scalar_data_type="int" />
		</row_data_type>
		<row_data_type id="37" si_name="person_with_parents">
			<row_data_type_field id="38" si_name="self_id" scalar_data_type="int" />
			<row_data_type_field id="39" si_name="self_name" scalar_data_type="str100" />
			<row_data_type_field id="40" si_name="father_id" scalar_data_type="int" />
			<row_data_type_field id="41" si_name="father_name" scalar_data_type="str100" />
			<row_data_type_field id="42" si_name="mother_id" scalar_data_type="int" />
			<row_data_type_field id="43" si_name="mother_name" scalar_data_type="str100" />
		</row_data_type>
		<row_data_type id="44" si_name="user_auth">
			<row_data_type_field id="45" si_name="user_id" scalar_data_type="int" />
			<row_data_type_field id="46" si_name="login_name" scalar_data_type="str20" />
			<row_data_type_field id="47" si_name="login_pass" scalar_data_type="str20" />
			<row_data_type_field id="48" si_name="private_name" scalar_data_type="str100" />
			<row_data_type_field id="49" si_name="private_email" scalar_data_type="str100" />
			<row_data_type_field id="50" si_name="may_login" scalar_data_type="boolean" />
			<row_data_type_field id="51" si_name="max_sessions" scalar_data_type="byte" />
		</row_data_type>
		<row_data_type id="52" si_name="user_profile">
			<row_data_type_field id="53" si_name="user_id" scalar_data_type="int" />
			<row_data_type_field id="54" si_name="public_name" scalar_data_type="str250" />
			<row_data_type_field id="55" si_name="public_email" scalar_data_type="str250" />
			<row_data_type_field id="56" si_name="web_url" scalar_data_type="str250" />
			<row_data_type_field id="57" si_name="contact_net" scalar_data_type="str250" />
			<row_data_type_field id="58" si_name="contact_phy" scalar_data_type="str250" />
			<row_data_type_field id="59" si_name="bio" scalar_data_type="str250" />
			<row_data_type_field id="60" si_name="plan" scalar_data_type="str250" />
			<row_data_type_field id="61" si_name="comments" scalar_data_type="str250" />
		</row_data_type>
		<row_data_type id="62" si_name="user">
			<row_data_type_field id="63" si_name="user_id" scalar_data_type="int" />
			<row_data_type_field id="64" si_name="login_name" scalar_data_type="str20" />
			<row_data_type_field id="65" si_name="login_pass" scalar_data_type="str20" />
			<row_data_type_field id="66" si_name="private_name" scalar_data_type="str100" />
			<row_data_type_field id="67" si_name="private_email" scalar_data_type="str100" />
			<row_data_type_field id="68" si_name="may_login" scalar_data_type="boolean" />
			<row_data_type_field id="69" si_name="max_sessions" scalar_data_type="byte" />
			<row_data_type_field id="70" si_name="public_name" scalar_data_type="str250" />
			<row_data_type_field id="71" si_name="public_email" scalar_data_type="str250" />
			<row_data_type_field id="72" si_name="web_url" scalar_data_type="str250" />
			<row_data_type_field id="73" si_name="contact_net" scalar_data_type="str250" />
			<row_data_type_field id="74" si_name="contact_phy" scalar_data_type="str250" />
			<row_data_type_field id="75" si_name="bio" scalar_data_type="str250" />
			<row_data_type_field id="76" si_name="plan" scalar_data_type="str250" />
			<row_data_type_field id="77" si_name="comments" scalar_data_type="str250" />
		</row_data_type>
		<row_data_type id="78" si_name="user_pref">
			<row_data_type_field id="79" si_name="user_id" scalar_data_type="int" />
			<row_data_type_field id="80" si_name="pref_name" scalar_data_type="entitynm" />
			<row_data_type_field id="81" si_name="pref_value" scalar_data_type="generic" />
		</row_data_type>
		<row_data_type id="82" si_name="user_theme">
			<row_data_type_field id="83" si_name="theme_name" scalar_data_type="generic" />
			<row_data_type_field id="84" si_name="theme_count" scalar_data_type="int" />
		</row_data_type>
	</elements>
	<blueprints>
		<catalog id="85" si_name="The Catalog Blueprint">
			<owner id="86" si_name="Gene\'s Owner" />
			<schema id="87" si_name="gene" owner="Gene\'s Owner">
				<table id="88" si_name="person" row_data_type="person">
					<table_field id="89" si_row_field="person_id" mandatory="1" default_val="1" auto_inc="1" />
					<table_field id="90" si_row_field="name" mandatory="1" />
					<table_index id="91" si_name="primary" index_type="UNIQUE">
						<table_index_field id="92" si_field="person_id" />
					</table_index>
					<table_index id="93" si_name="ak_alternate_id" index_type="UNIQUE">
						<table_index_field id="94" si_field="alternate_id" />
					</table_index>
					<table_index id="95" si_name="fk_father" index_type="FOREIGN" f_table="person">
						<table_index_field id="96" si_field="father_id" f_field="person_id" />
					</table_index>
					<table_index id="97" si_name="fk_mother" index_type="FOREIGN" f_table="person">
						<table_index_field id="98" si_field="mother_id" f_field="person_id" />
					</table_index>
				</table>
				<view id="99" si_name="person_with_parents" view_type="JOINED" row_data_type="person_with_parents">
					<view_src id="100" si_name="self" match="person">
						<view_src_field id="101" si_match_field="person_id" />
						<view_src_field id="102" si_match_field="name" />
						<view_src_field id="103" si_match_field="father_id" />
						<view_src_field id="104" si_match_field="mother_id" />
					</view_src>
					<view_src id="105" si_name="father" match="person">
						<view_src_field id="106" si_match_field="person_id" />
						<view_src_field id="107" si_match_field="name" />
					</view_src>
					<view_src id="108" si_name="mother" match="person">
						<view_src_field id="109" si_match_field="person_id" />
						<view_src_field id="110" si_match_field="name" />
					</view_src>
					<view_field id="111" si_row_field="self_id" src_field="[person_id,self]" />
					<view_field id="112" si_row_field="self_name" src_field="[name,self]" />
					<view_field id="113" si_row_field="father_id" src_field="[person_id,father]" />
					<view_field id="114" si_row_field="father_name" src_field="[name,father]" />
					<view_field id="115" si_row_field="mother_id" src_field="[person_id,mother]" />
					<view_field id="116" si_row_field="mother_name" src_field="[name,mother]" />
					<view_join id="117" lhs_src="self" rhs_src="father" join_op="LEFT">
						<view_join_field id="118" lhs_src_field="father_id" rhs_src_field="person_id" />
					</view_join>
					<view_join id="119" lhs_src="self" rhs_src="mother" join_op="LEFT">
						<view_join_field id="120" lhs_src_field="mother_id" rhs_src_field="person_id" />
					</view_join>
				</view>
				<table id="121" si_name="user_auth" row_data_type="user_auth">
					<table_field id="122" si_row_field="user_id" mandatory="1" default_val="1" auto_inc="1" />
					<table_field id="123" si_row_field="login_name" mandatory="1" />
					<table_field id="124" si_row_field="login_pass" mandatory="1" />
					<table_field id="125" si_row_field="private_name" mandatory="1" />
					<table_field id="126" si_row_field="private_email" mandatory="1" />
					<table_field id="127" si_row_field="may_login" mandatory="1" />
					<table_field id="128" si_row_field="max_sessions" mandatory="1" default_val="3" />
					<table_index id="129" si_name="primary" index_type="UNIQUE">
						<table_index_field id="130" si_field="user_id" />
					</table_index>
					<table_index id="131" si_name="ak_login_name" index_type="UNIQUE">
						<table_index_field id="132" si_field="login_name" />
					</table_index>
					<table_index id="133" si_name="ak_private_email" index_type="UNIQUE">
						<table_index_field id="134" si_field="private_email" />
					</table_index>
				</table>
				<table id="135" si_name="user_profile" row_data_type="user_profile">
					<table_field id="136" si_row_field="user_id" mandatory="1" />
					<table_field id="137" si_row_field="public_name" mandatory="1" />
					<table_index id="138" si_name="primary" index_type="UNIQUE">
						<table_index_field id="139" si_field="user_id" />
					</table_index>
					<table_index id="140" si_name="ak_public_name" index_type="UNIQUE">
						<table_index_field id="141" si_field="public_name" />
					</table_index>
					<table_index id="142" si_name="fk_user" index_type="FOREIGN" f_table="user_auth">
						<table_index_field id="143" si_field="user_id" f_field="user_id" />
					</table_index>
				</table>
				<view id="144" si_name="user" view_type="JOINED" row_data_type="user">
					<view_src id="145" si_name="user_auth" match="user_auth">
						<view_src_field id="146" si_match_field="user_id" />
						<view_src_field id="147" si_match_field="login_name" />
						<view_src_field id="148" si_match_field="login_pass" />
						<view_src_field id="149" si_match_field="private_name" />
						<view_src_field id="150" si_match_field="private_email" />
						<view_src_field id="151" si_match_field="may_login" />
						<view_src_field id="152" si_match_field="max_sessions" />
					</view_src>
					<view_src id="153" si_name="user_profile" match="user_profile">
						<view_src_field id="154" si_match_field="user_id" />
						<view_src_field id="155" si_match_field="public_name" />
						<view_src_field id="156" si_match_field="public_email" />
						<view_src_field id="157" si_match_field="web_url" />
						<view_src_field id="158" si_match_field="contact_net" />
						<view_src_field id="159" si_match_field="contact_phy" />
						<view_src_field id="160" si_match_field="bio" />
						<view_src_field id="161" si_match_field="plan" />
						<view_src_field id="162" si_match_field="comments" />
					</view_src>
					<view_field id="163" si_row_field="user_id" src_field="[user_id,user_auth]" />
					<view_field id="164" si_row_field="login_name" src_field="login_name" />
					<view_field id="165" si_row_field="login_pass" src_field="login_pass" />
					<view_field id="166" si_row_field="private_name" src_field="private_name" />
					<view_field id="167" si_row_field="private_email" src_field="private_email" />
					<view_field id="168" si_row_field="may_login" src_field="may_login" />
					<view_field id="169" si_row_field="max_sessions" src_field="max_sessions" />
					<view_field id="170" si_row_field="public_name" src_field="public_name" />
					<view_field id="171" si_row_field="public_email" src_field="public_email" />
					<view_field id="172" si_row_field="web_url" src_field="web_url" />
					<view_field id="173" si_row_field="contact_net" src_field="contact_net" />
					<view_field id="174" si_row_field="contact_phy" src_field="contact_phy" />
					<view_field id="175" si_row_field="bio" src_field="bio" />
					<view_field id="176" si_row_field="plan" src_field="plan" />
					<view_field id="177" si_row_field="comments" src_field="comments" />
					<view_join id="178" lhs_src="user_auth" rhs_src="user_profile" join_op="LEFT">
						<view_join_field id="179" lhs_src_field="user_id" rhs_src_field="user_id" />
					</view_join>
				</view>
				<table id="180" si_name="user_pref" row_data_type="user_pref">
					<table_field id="181" si_row_field="user_id" mandatory="1" />
					<table_field id="182" si_row_field="pref_name" mandatory="1" />
					<table_index id="183" si_name="primary" index_type="UNIQUE">
						<table_index_field id="184" si_field="user_id" />
						<table_index_field id="185" si_field="pref_name" />
					</table_index>
					<table_index id="186" si_name="fk_user" index_type="FOREIGN" f_table="user_auth">
						<table_index_field id="187" si_field="user_id" f_field="user_id" />
					</table_index>
				</table>
			</schema>
		</catalog>
		<application id="188" si_name="My App">
			<view id="189" si_name="user_theme" view_type="JOINED" row_data_type="user_theme">
				<view_src id="190" si_name="user_pref" match="user_pref">
					<view_src_field id="191" si_match_field="pref_name" />
					<view_src_field id="192" si_match_field="pref_value" />
				</view_src>
				<view_field id="193" si_row_field="theme_name" src_field="pref_value" />
				<view_expr id="194" view_part="RESULT" set_result_field="theme_count" cont_type="SCALAR" valf_call_sroutine="COUNT">
					<view_expr id="195" cont_type="SCALAR" valf_src_field="pref_value" />
				</view_expr>
				<view_expr id="196" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="EQ">
					<view_expr id="197" cont_type="SCALAR" valf_src_field="pref_name" />
					<view_expr id="198" cont_type="SCALAR" valf_literal="theme" scalar_data_type="str30" />
				</view_expr>
				<view_expr id="199" view_part="GROUP" cont_type="SCALAR" valf_src_field="pref_value" />
				<view_expr id="200" view_part="HAVING" cont_type="SCALAR" valf_call_sroutine="GT">
					<view_expr id="201" cont_type="SCALAR" valf_call_sroutine="COUNT" />
					<view_expr id="202" cont_type="SCALAR" valf_literal="1" scalar_data_type="int" />
				</view_expr>
				<view_expr id="203" view_part="ORDER" cont_type="SCALAR" valf_result_field="theme_count" />
				<view_expr id="204" view_part="ORDER" cont_type="SCALAR" valf_result_field="theme_name" />
			</view>
			<routine id="205" si_name="get_user" routine_type="FUNCTION" return_cont_type="CURSOR">
				<routine_arg id="206" si_name="curr_uid" cont_type="SCALAR" scalar_data_type="int" />
				<view id="207" si_name="get_user" view_type="JOINED" row_data_type="user">
					<view_src id="208" si_name="m" match="user">
						<view_src_field id="209" si_match_field="user_id" />
						<view_src_field id="210" si_match_field="login_name" />
					</view_src>
					<view_expr id="211" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="EQ">
						<view_expr id="212" cont_type="SCALAR" valf_src_field="user_id" />
						<view_expr id="213" cont_type="SCALAR" valf_p_routine_item="curr_uid" />
					</view_expr>
					<view_expr id="214" view_part="ORDER" cont_type="SCALAR" valf_src_field="login_name" />
				</view>
				<routine_stmt id="215" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="216" si_name="get_pwp" routine_type="FUNCTION" return_cont_type="CURSOR">
				<routine_arg id="217" si_name="srchw_fa" cont_type="SCALAR" scalar_data_type="str30" />
				<routine_arg id="218" si_name="srchw_mo" cont_type="SCALAR" scalar_data_type="str30" />
				<view id="219" si_name="get_pwp" view_type="JOINED" row_data_type="person_with_parents">
					<view_src id="220" si_name="m" match="person_with_parents">
						<view_src_field id="221" si_match_field="self_name" />
						<view_src_field id="222" si_match_field="father_name" />
						<view_src_field id="223" si_match_field="mother_name" />
					</view_src>
					<view_expr id="224" view_part="WHERE" cont_type="SCALAR" valf_call_sroutine="AND">
						<view_expr id="225" cont_type="SCALAR" valf_call_sroutine="LIKE">
							<view_expr id="226" cont_type="SCALAR" valf_src_field="father_name" />
							<view_expr id="227" cont_type="SCALAR" valf_p_routine_item="srchw_fa" />
						</view_expr>
						<view_expr id="228" cont_type="SCALAR" valf_call_sroutine="LIKE">
							<view_expr id="229" cont_type="SCALAR" valf_src_field="mother_name" />
							<view_expr id="230" cont_type="SCALAR" valf_p_routine_item="srchw_mo" />
						</view_expr>
					</view_expr>
					<view_expr id="231" view_part="ORDER" cont_type="SCALAR" valf_src_field="self_name" />
					<view_expr id="232" view_part="ORDER" cont_type="SCALAR" valf_src_field="father_name" />
					<view_expr id="233" view_part="ORDER" cont_type="SCALAR" valf_src_field="mother_name" />
				</view>
				<routine_stmt id="234" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="235" si_name="get_theme" routine_type="FUNCTION" return_cont_type="CURSOR">
				<view id="236" si_name="get_theme" view_type="ALIAS" row_data_type="user_theme">
					<view_src id="237" si_name="m" match="user_theme" />
				</view>
				<routine_stmt id="238" call_sroutine="CURSOR_OPEN" />
			</routine>
			<routine id="239" si_name="get_person" routine_type="FUNCTION" return_cont_type="CURSOR">
				<view id="240" si_name="get_person" view_type="ALIAS" row_data_type="person">
					<view_src id="241" si_name="person" match="person" />
				</view>
				<routine_stmt id="242" call_sroutine="CURSOR_OPEN" />
			</routine>
		</application>
	</blueprints>
	<tools />
	<sites>
		<application_instance id="243" si_name="My App Instance" blueprint="My App" />
	</sites>
	<circumventions />
</root>
'
	;
}

######################################################################

1;
