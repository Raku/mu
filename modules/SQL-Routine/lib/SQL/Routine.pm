#!pugs
use v6;

require Locale::KeyedText-0.0.4;

######################################################################

=head1 NAME

SQL::Routine - Specify all database tasks with SQL routines

=head1 DEPENDENCIES

Perl Version: 6

Core Modules: I<none>

Non-Core Modules: 

	Locale::KeyedText-0.0.3 (for error messages)

=head1 COPYRIGHT AND LICENSE

This file is part of the SQL::Routine library (libSQLRT).

SQL::Routine is Copyright (c) 1999-2005, Darren R. Duncan.  All rights
reserved. Address comments, suggestions, and bug reports to
B<perl@DarrenDuncan.net>, or visit "http://www.DarrenDuncan.net" for more
information.

SQL::Routine is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License (GPL) version 2 as published by the
Free Software Foundation (http://www.fsf.org/).  You should have received a
copy of the GPL as part of the SQL::Routine distribution, in the file named
"LICENSE"; if not, write to the Free Software Foundation, Inc., 59 Temple
Place, Suite 330, Boston, MA 02111-1307 USA.

Linking SQL::Routine statically or dynamically with other modules is making a
combined work based on SQL::Routine.  Thus, the terms and conditions of the GPL
cover the whole combination.  As a special exception, the copyright holders of
SQL::Routine give you permission to link SQL::Routine with independent modules,
regardless of the license terms of these independent modules, and to copy and
distribute the resulting combined work under terms of your choice, provided
that every copy of the combined work is accompanied by a complete copy of the
source code of SQL::Routine (the version of SQL::Routine used to produce the
combined work), being distributed under the terms of the GPL plus this
exception.  An independent module is a module which is not derived from or
based on SQL::Routine, and which is fully useable when not linked to
SQL::Routine in any form.

Any versions of SQL::Routine that you modify and distribute must carry
prominent notices stating that you changed the files and the date of any
changes, in addition to preserving this original copyright notice and other
credits. SQL::Routine is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.

While it is by no means required, the copyright holders of SQL::Routine would
appreciate being informed any time you create a modified version of
SQL::Routine that you are willing to distribute, because that is a practical
way of suggesting improvements to the standard version.

=cut

######################################################################
######################################################################

# These are programmatically recognized enumerations of values that 
# particular Node attributes are allowed to have.  They are given names 
# here so that multiple Node types can make use of the same value lists.  
# Currently only the codes are shown, but attributes may be attached later.
my %ENUMERATED_TYPES is constant = (
	'container_type' => { <
		ERROR SCALAR ROW SC_ARY RW_ARY CONN CURSOR LIST SRT_NODE SRT_NODE_LIST
	>.map:{ ($_ => 1) } },
	'exception_type' => { <
		SRTX_NO_ENVI_LOAD_FAILED SRTX_ENVI_EXEC_FAILED 
		SRTX_NO_CONN_SERVER_ABSENT SRTX_NO_CONN_BAD_AUTH SRTX_NO_CONN_ACTIVE_LOST
	>.map:{ ($_ => 1) } },
	'standard_routine' => { <
		CATALOG_LIST CATALOG_INFO CATALOG_VERIFY 
		CATALOG_CREATE CATALOG_DELETE CATALOG_CLONE CATALOG_MOVE
		CATALOG_OPEN 
		CATALOG_CLOSE 
		CATALOG_PING CATALOG_ATTACH CATALOG_DETACH 
		SCHEMA_LIST SCHEMA_INFO SCHEMA_VERIFY
		SCHEMA_CREATE SCHEMA_DELETE SCHEMA_CLONE SCHEMA_UPDATE 
		DOMAIN_LIST DOMAIN_INFO DOMAIN_VERIFY
		DOMAIN_CREATE DOMAIN_DELETE DOMAIN_CLONE DOMAIN_UPDATE
		SEQU_LIST SEQU_INFO SEQU_VERIFY
		SEQU_CREATE SEQU_DELETE SEQU_CLONE SEQU_UPDATE
		TABLE_LIST TABLE_INFO TABLE_VERIFY
		TABLE_CREATE TABLE_DELETE TABLE_CLONE TABLE_UPDATE
		VIEW_LIST VIEW_INFO VIEW_VERIFY
		VIEW_CREATE VIEW_DELETE VIEW_CLONE VIEW_UPDATE
		ROUTINE_LIST ROUTINE_INFO ROUTINE_VERIFY 
		ROUTINE_CREATE ROUTINE_DELETE ROUTINE_CLONE ROUTINE_UPDATE
		USER_LIST USER_INFO USER_VERIFY
		USER_CREATE USER_DELETE USER_CLONE USER_UPDATE USER_GRANT USER_REVOKE
		REC_FETCH 
		REC_VERIFY REC_INSERT REC_UPDATE 
		REC_DELETE REC_REPLACE REC_CLONE REC_LOCK REC_UNLOCK
		RETURN
		CURSOR_OPEN CURSOR_CLOSE CURSOR_FETCH
		SELECT INSERT UPDATE DELETE 
		COMMIT ROLLBACK
		LOCK UNLOCK 
		PLAIN THROW TRY CATCH IF ELSEIF ELSE SWITCH CASE OTHERWISE FOREACH 
		FOR WHILE UNTIL MAP GREP REGEXP 
		LOOP CONDITION LOGIC 
		CAST
		NOT AND OR XOR
		EQ NE LT GT LE GE IS_NULL NOT_NULL COALESCE SWITCH LIKE
		ADD SUB MUL DIV DIVI MOD ROUND ABS POWER LOG
		SCONCAT SLENGTH SINDEX SUBSTR SREPEAT STRIM SPAD SPADL LC UC
		COUNT MIN MAX SUM AVG CONCAT EVERY ANY EXISTS
		GB_SETS GB_RLUP GB_CUBE
	>.map:{ ($_ => 1) } },
	'standard_routine_context' => { <
		CONN_CX CURSOR_CX
	>.map:{ ($_ => 1) } },
	'standard_routine_arg' => { <
		RECURSIVE LINK_BP SOURCE_LINK_BP DEST_LINK_BP 
		LOGIN_NAME LOGIN_PASS
		RETURN_VALUE
		SELECT_DEFN INSERT_DEFN UPDATE_DEFN DELETE_DEFN
		CAST_TARGET CAST_OPERAND
		FACTOR FACTORS LHS RHS ARG TERMS
		LOOK_IN CASES DEFAULT LOOK_FOR FIXED_LEFT FIXED_RIGHT
		START REMOVE DIVIDEND DIVISOR PLACES OPERAND RADIX EXPONENT
		SOURCE START_POS STR_LEN REPEAT
	>.map:{ ($_ => 1) } },
	'simple_scalar_type' => { <
		NUM_INT NUM_EXA NUM_APR STR_BIT STR_CHAR BOOLEAN 
		DATM_FULL DATM_DATE DATM_TIME INTRVL_YM INTRVL_DT 
	>.map:{ ($_ => 1) } },
	'char_enc_type' => { <
		UTF8 UTF16 UTF32 ASCII ANSEL EBCDIC
	>.map:{ ($_ => 1) } },
	'calendar' => { <
		ABS GRE JUL CHI HEB ISL JPN
	>.map:{ ($_ => 1) } },
	'privilege_type' => { <
		ALL SELECT DELETE INSERT UPDATE CONNECT EXECUTE CREATE ALTER DROP 
	>.map:{ ($_ => 1) } },
	'table_index_type' => { <
		ATOMIC FULLTEXT UNIQUE FOREIGN UFOREIGN
	>.map:{ ($_ => 1) } },
	'view_type' => { <
		ALIAS JOINED GROUPED COMPOUND INSERT UPDATE DELETE
	>.map:{ ($_ => 1) } },
	'compound_operator' => { <
		UNION DIFFERENCE INTERSECTION EXCLUSION
	>.map:{ ($_ => 1) } },
	'join_operator' => { <
		CROSS INNER LEFT RIGHT FULL
	>.map:{ ($_ => 1) } },
	'view_part' => { <
		RESULT SET FROM WHERE GROUP HAVING WINDOW ORDER MAXR SKIPR
	>.map:{ ($_ => 1) } },
	'routine_type' => { <
		PACKAGE TRIGGER PROCEDURE FUNCTION BLOCK
	>.map:{ ($_ => 1) } },
	'basic_trigger_event' => { <
		BEFR_INS AFTR_INS INST_INS 
		BEFR_UPD AFTR_UPD INST_UPD 
		BEFR_DEL AFTR_DEL INST_DEL
	>.map:{ ($_ => 1) } },
	'user_type' => { <
		ROOT SCHEMA_OWNER DATA_EDITOR ANONYMOUS
	>.map:{ ($_ => 1) } },
);

# Names of hash keys in %NODE_TYPES elements:
my $TPI_AT_SEQUENCE = 'at_sequence'; # Array of all 'attribute' names in canon order
my $TPI_PP_PSEUDONODE = 'pp_pseudonode'; # If set, Nodes of this type have a hard-coded pseudo-parent
my $TPI_PP_NREF     = 'pp_nref'; # An array ref whose values are enums and each matches a single %NODE_TYPES key.
my $TPI_AT_LITERALS = 'at_literals'; # Hash - Keys are attr names a Node can have which have literal values
	# Values are enums and say what literal data type the attribute has, like int or bool or str
my $TPI_AT_ENUMS    = 'at_enums'; # Hash - Keys are attr names a Node can have which are enumerated values
	# Values are enums and match a %ENUMERATED_TYPES key
my $TPI_AT_NREFS    = 'at_nrefs'; # Hash - Keys are attr names a Node can have which are Node Ref/Id values
	# Values are array refs whose values are enums and each matches a single %NODE_TYPES key, 
	# but an empty array matches all Node types.
my $TPI_SI_ATNM     = 'si_atnm'; # The surrogate identifier, distinct under primary parent and always-mandatory
	# Is an array of 4 cstr elements, one for id|lit|enum|nref; 1 elem is valued, other 3 are undef
	# External code can opt specify a Node by the value of this attr-name rather of its Id
	# If set_attributes() is given a non-Hash value, it will resolve to setting either this 'SI' 
	# attribute or the Node's 'id' attribute depending on whether it looks like an 'id' attribute.
my $TPI_WR_ATNM     = 'wr_atnm'; # A wrapper attribute
my $TPI_MA_ATNMS    = 'ma_atnms'; # Array of always-mandatory ('MA') attributes
	# The array contains 3 elements, one each for lit, enum, nref; each inner elem is a MA boolean
my $TPI_MUTEX_ATGPS = 'mutex_atgps'; # Array of groups of mutually exclusive attributes
	# Each array element is an array ref with 5 elements: 1. mutex-name (cstr); 2. lit members (ary); 
	# 3. enum members (ary); 4. nref members (ary); 5. mandatory-flag (boolean).
my $TPI_LOCAL_ATDPS = 'local_atdps'; # Array of attributes depended-on by other attrs in same Nodes
	# Each array element is an array ref with 4 elements: 
	# 1. undef or depended on lit attr name (cstr); 2. undef or depended on enum attr name (cstr); 
	# 3. undef or depended on nref attr name (cstr); 4. an array ref of N elements where 
	# each element is an array ref with 5 elements: 
		# 1. an array ref with 0..N elements that are names of dependent lit attrs; 
		# 2. an array ref with 0..N elements that are names of dependent enum attrs; 
		# 3. an array ref with 0..N elements that are names of dependent nref attrs; 
		# 4. an array ref with 0..N elements that are depended-on values, one of which must 
		# be matched, if depended-on attr is an enum, or which is empty otherwise;
		# 5. mandatory-flag (boolean).
my $TPI_ANCES_ATCORS = 'ances_atcors'; # Hash of Arrays of steps to follow when linking Nodes using SI
	# Each hash key is a Node-ref attr name, each hash value is an array of steps.
my $TPI_REMOTE_ADDR = 'remote_addr'; # Array of ancestor Node type names under which this Node can be remotely addressed
my $TPI_CHILD_QUANTS = 'child_quants'; # Array of quantity limits for child Nodes
	# Each array element is an array ref with 3 elements: 
	# 1. child-node-type (cstr); 2. range-min (uint); 3. range-max (uint)
my $TPI_MUDI_ATGPS  = 'mudi_atgps'; # Array of groups of mutually distinct attributes
	# Each array element is an array ref with 2 elements: 1. mudi-name (cstr); 
	# 2. an array ref of N elements where each element is an array ref with 4 elements:
		# 1. child-node-type (cstr);
		# 2. an array ref with 0..N elements that are names of lit child-node-attrs; 
		# 3. an array ref with 0..N elements that are names of enum child-node-attrs; 
		# 4. an array ref with 0..N elements that are names of nref child-node-attrs.

# Names of special "pseudo-Nodes" that are used in an XML version of this structure.
my $SQLRT_L1_ROOT_PSND is constant = 'root';
my $SQLRT_L2_ELEM_PSND is constant = 'elements';
my $SQLRT_L2_BLPR_PSND is constant = 'blueprints';
my $SQLRT_L2_TOOL_PSND is constant = 'tools';
my $SQLRT_L2_SITE_PSND is constant = 'sites';
my $SQLRT_L2_CIRC_PSND is constant = 'circumventions';
my @L2_PSEUDONODE_LIST is constant = ($SQLRT_L2_ELEM_PSND, $SQLRT_L2_BLPR_PSND, 
	$SQLRT_L2_TOOL_PSND, $SQLRT_L2_SITE_PSND, $SQLRT_L2_CIRC_PSND);
# This hash is used like the subsequent %NODE_TYPES for specific purposes.
my %PSEUDONODE_TYPES is constant = (
	$SQLRT_L1_ROOT_PSND => {
	},
	$SQLRT_L2_ELEM_PSND => {
	},
	$SQLRT_L2_BLPR_PSND => {
		$TPI_CHILD_QUANTS => [
			['application',1,undef],
		],
	},
	$SQLRT_L2_TOOL_PSND => {
	},
	$SQLRT_L2_SITE_PSND => {
		$TPI_CHILD_QUANTS => [
			['application_instance',1,undef],
		],
	},
	$SQLRT_L2_CIRC_PSND => {
	},
);

# These are used with $TPI_ANCES_ATCORS:
my $S is constant = '.';
my $P is constant = '..';
my $R is constant = '...';
my $C is constant = '....';
# These are the allowed Node types, with their allowed attributes and their 
# allowed child Node types.  They are used for method input checking and 
# other related tasks.
my %NODE_TYPES is constant = (
	'scalar_data_type' => {
		$TPI_AT_SEQUENCE => [< 
			id si_name base_type num_precision num_scale num_octets num_unsigned 
			max_octets max_chars store_fixed char_enc trim_white uc_latin lc_latin 
			pad_char trim_pad calendar with_zone range_min range_max 
		>],
		$TPI_PP_PSEUDONODE => $SQLRT_L2_ELEM_PSND,
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
			'num_precision' => 'uint',
			'num_scale' => 'uint',
			'num_octets' => 'uint',
			'num_unsigned' => 'bool',
			'max_octets' => 'uint',
			'max_chars' => 'uint',
			'store_fixed' => 'bool',
			'trim_white' => 'bool',
			'uc_latin' => 'bool',
			'lc_latin' => 'bool',
			'pad_char' => 'cstr',
			'trim_pad' => 'bool',
			'with_zone' => 'sint',
			'range_min' => 'misc',
			'range_max' => 'misc',
		},
		$TPI_AT_ENUMS => {
			'base_type' => 'simple_scalar_type',
			'char_enc' => 'char_enc_type',
			'calendar' => 'calendar',
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_MA_ATNMS => [[],['base_type'],[]],
		$TPI_MUTEX_ATGPS => [
			['num_size',['num_precision','num_octets'],[],[],0],
		],
		$TPI_LOCAL_ATDPS => [
			[undef,'base_type',undef,[
				[['num_precision'],[],[],['NUM_INT','NUM_EXA','NUM_APR'],0],
				[['num_scale'],[],[],['NUM_EXA','NUM_APR'],0],
				[['num_octets'],[],[],['NUM_INT','NUM_APR'],0],
				[['num_unsigned'],[],[],['NUM_INT','NUM_EXA','NUM_APR'],0],
				[['max_octets'],[],[],['STR_BIT'],1],
				[['max_chars'],[],[],['STR_CHAR'],1],
				[[],['char_enc'],[],['STR_CHAR'],1],
				[['trim_white'],[],[],['STR_CHAR'],0],
				[['uc_latin','lc_latin'],[],[],['STR_CHAR'],0],
				[['pad_char'],[],[],['STR_CHAR'],0],
				[['trim_pad'],[],[],['STR_CHAR'],0],
				[[],['calendar'],[],['DATM_FULL','DATM_DATE'],1],
				[['with_zone'],[],[],['DATM_FULL','DATM_DATE','DATM_TIME'],0],
			]],
			['num_precision',undef,undef,[
				[['num_scale'],[],[],[],0],
			]],
		],
	},
	'scalar_data_type_opt' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_value 
		>],
		$TPI_PP_NREF => ['scalar_data_type'],
		$TPI_AT_LITERALS => {
			'si_value' => 'misc',
		},
		$TPI_SI_ATNM => [undef,'si_value',undef,undef],
	},
	'row_data_type' => {
		$TPI_AT_SEQUENCE => [< 
			id si_name
		>],
		$TPI_PP_PSEUDONODE => $SQLRT_L2_ELEM_PSND,
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_CHILD_QUANTS => [
			['row_data_type_field',1,undef],
		],
	},
	'row_data_type_field' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name scalar_data_type
		>],
		$TPI_PP_NREF => ['row_data_type'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'scalar_data_type' => ['scalar_data_type'],
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_MA_ATNMS => [[],[],['scalar_data_type']],
	},
	'catalog' => {
		$TPI_AT_SEQUENCE => [< 
			id si_name single_schema
		>],
		$TPI_PP_PSEUDONODE => $SQLRT_L2_BLPR_PSND,
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
			'single_schema' => 'bool',
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
	},
	'application' => {
		$TPI_AT_SEQUENCE => [< 
			id si_name 
		>],
		$TPI_PP_PSEUDONODE => $SQLRT_L2_BLPR_PSND,
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
	},
	'owner' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name 
		>],
		$TPI_PP_NREF => ['catalog'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
	},
	'catalog_link' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name target
		>],
		$TPI_PP_NREF => ['catalog','application'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'target' => ['catalog'],
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_MA_ATNMS => [[],[],['target']],
	},
	'schema' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name owner 
		>],
		$TPI_PP_NREF => ['catalog'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'owner' => ['owner'],
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_MA_ATNMS => [[],[],['owner']],
	},
	'role' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name
		>],
		$TPI_PP_NREF => ['catalog'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
	},
	'privilege_on' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_priv_on
		>],
		$TPI_PP_NREF => ['role'],
		$TPI_AT_NREFS => {
			'si_priv_on' => ['schema','scalar_domain','row_domain','sequence','table','view','routine'],
		},
		$TPI_SI_ATNM => [undef,undef,undef,'si_priv_on'],
	},
	'privilege_for' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_priv_type
		>],
		$TPI_PP_NREF => ['privilege_on'],
		$TPI_AT_ENUMS => {
			'si_priv_type' => 'privilege_type',
		},
		$TPI_SI_ATNM => [undef,undef,'si_priv_type',undef],
	},
	'scalar_domain' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name data_type
		>],
		$TPI_PP_NREF => ['schema','application'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'data_type' => ['scalar_data_type'],
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_MA_ATNMS => [[],[],['data_type']],
		$TPI_REMOTE_ADDR => ['catalog'],
	},
	'row_domain' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name data_type
		>],
		$TPI_PP_NREF => ['schema','application'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'data_type' => ['row_data_type'],
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_WR_ATNM => 'data_type',
		$TPI_MA_ATNMS => [[],[],['data_type']],
		$TPI_REMOTE_ADDR => ['catalog'],
	},
	'sequence' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name increment min_val max_val start_val cycle order 
		>],
		$TPI_PP_NREF => ['schema','application'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
			'increment' => 'sint',
			'min_val' => 'sint',
			'max_val' => 'sint',
			'start_val' => 'sint',
			'cycle' => 'bool',
			'order' => 'bool',
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_REMOTE_ADDR => ['catalog'],
	},
	'table' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name row_data_type
		>],
		$TPI_PP_NREF => ['schema','application'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'row_data_type' => ['row_data_type','row_domain'],
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_WR_ATNM => 'row_data_type',
		$TPI_MA_ATNMS => [[],[],['row_data_type']],
		$TPI_REMOTE_ADDR => ['catalog'],
	},
	'table_field' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_row_field mandatory default_val auto_inc default_seq 
		>],
		$TPI_PP_NREF => ['table'],
		$TPI_AT_LITERALS => {
			'mandatory' => 'bool',
			'default_val' => 'misc',
			'auto_inc' => 'bool',
		},
		$TPI_AT_NREFS => {
			'si_row_field' => ['row_data_type_field'],
			'default_seq' => ['sequence'],
		},
		$TPI_SI_ATNM => [undef,undef,undef,'si_row_field'],
		$TPI_MUTEX_ATGPS => [
			['default',['default_val'],[],['default_seq'],0],
		],
		$TPI_ANCES_ATCORS => {
			'si_row_field' => [$S,$P],
		},
	},
	'table_index' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name index_type f_table 
		>],
		$TPI_PP_NREF => ['table'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
		},
		$TPI_AT_ENUMS => {
			'index_type' => 'table_index_type',
		},
		$TPI_AT_NREFS => {
			'f_table' => ['table'],
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_MA_ATNMS => [[],['index_type'],[]],
		$TPI_LOCAL_ATDPS => [
			[undef,'index_type',undef,[
				[[],[],['f_table'],['FOREIGN','UFOREIGN'],1],
			]],
		],
		$TPI_CHILD_QUANTS => [
			['table_index_field',1,undef],
		],
		$TPI_MUDI_ATGPS => [
			['ak_f_table_field',[
				['table_index_field',[],[],['f_field']],
			]],
		],
	},
	'table_index_field' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_field f_field 
		>],
		$TPI_PP_NREF => ['table_index'],
		$TPI_AT_NREFS => {
			'si_field' => ['row_data_type_field'],
			'f_field' => ['row_data_type_field'],
		},
		$TPI_SI_ATNM => [undef,undef,undef,'si_field'],
		$TPI_ANCES_ATCORS => {
			'si_field' => [$S,$P,$P],
			'f_field' => [$S,$P,'f_table'],
		},
	},
	'view' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name view_type row_data_type recursive compound_op 
			distinct_rows may_write set_p_routine_item ins_p_routine_item
		>],
		$TPI_PP_NREF => ['view','routine','schema','application'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
			'recursive' => 'bool',
			'distinct_rows' => 'bool',
			'may_write' => 'bool',
		},
		$TPI_AT_ENUMS => {
			'view_type' => 'view_type',
			'compound_op' => 'compound_operator',
		},
		$TPI_AT_NREFS => {
			'row_data_type' => ['row_data_type','row_domain'],
			'set_p_routine_item' => ['routine_arg','routine_var'],
			'ins_p_routine_item' => ['routine_arg','routine_var'],
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_WR_ATNM => 'row_data_type',
		$TPI_MA_ATNMS => [[],['view_type'],[]],
		$TPI_LOCAL_ATDPS => [
			[undef,'view_type',undef,[
				[[],[],['row_data_type'],['ALIAS','JOINED','GROUPED','COMPOUND','INSERT'],1],
				[['recursive'],[],[],['JOINED','GROUPED','COMPOUND'],0],
				[[],['compound_op'],[],['COMPOUND'],1],
				[['distinct_rows'],[],[],['JOINED','GROUPED','COMPOUND'],0],
				[['may_write'],[],[],['ALIAS','JOINED','GROUPED','COMPOUND'],0],
				[[],[],['set_p_routine_item'],['ALIAS','JOINED','GROUPED','COMPOUND'],0],
				[[],[],['ins_p_routine_item'],['INSERT'],1],
			]],
		],
		$TPI_REMOTE_ADDR => ['catalog'],
		$TPI_MUDI_ATGPS => [
			['ak_join',[
				['view_join',[],[],['lhs_src','rhs_src']],
			]],
			['ak_join_limit_one',[
				['view_join',[],[],['rhs_src']],
			]],
			['ak_expr_set_result_field',[
				['view_expr',[],[],['set_result_field']],
			]],
			['ak_expr_set_src_field',[
				['view_expr',[],[],['set_src_field']],
			]],
			['ak_expr_call_src_arg',[
				['view_expr',[],[],['call_src_arg']],
			]],
		],
	},
	'view_arg' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name cont_type scalar_data_type row_data_type 
		>],
		$TPI_PP_NREF => ['view'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
		},
		$TPI_AT_ENUMS => {
			'cont_type' => 'container_type',
		},
		$TPI_AT_NREFS => {
			'scalar_data_type' => ['scalar_data_type','scalar_domain'],
			'row_data_type' => ['row_data_type','row_domain'],
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_WR_ATNM => 'row_data_type',
		$TPI_MA_ATNMS => [[],['cont_type'],[]],
		$TPI_MUTEX_ATGPS => [
			['data_type',[],[],['scalar_data_type','row_data_type'],1],
		],
		$TPI_LOCAL_ATDPS => [
			[undef,'cont_type',undef,[
				[[],[],['scalar_data_type'],['SCALAR','SC_ARY'],1],
				[[],[],['row_data_type'],['ROW','RW_ARY'],1],
			]],
		],
	},
	'view_src' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name match catalog_link may_write
		>],
		$TPI_PP_NREF => ['view'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
			'may_write' => 'bool',
		},
		$TPI_AT_NREFS => {
			'match' => ['table','view','view_arg','routine_arg','routine_var'],
			'catalog_link' => ['catalog_link'],
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
	},
	'view_src_arg' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_match_view_arg
		>],
		$TPI_PP_NREF => ['view_src'],
		$TPI_AT_NREFS => {
			'si_match_view_arg' => ['view_arg'],
		},
		$TPI_SI_ATNM => [undef,undef,undef,'si_match_view_arg'],
	},
	'view_src_field' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_match_field
		>],
		$TPI_PP_NREF => ['view_src'],
		$TPI_AT_NREFS => {
			'si_match_field' => ['row_data_type_field'],
		},
		$TPI_SI_ATNM => [undef,undef,undef,'si_match_field'],
		$TPI_ANCES_ATCORS => {
			'si_match_field' => [$S,$P,'match'],
		},
	},
	'view_field' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_row_field src_field 
		>],
		$TPI_PP_NREF => ['view'],
		$TPI_AT_NREFS => {
			'si_row_field' => ['row_data_type_field'],
			'src_field' => ['view_src_field'],
		},
		$TPI_SI_ATNM => [undef,undef,undef,'si_row_field'],
		$TPI_ANCES_ATCORS => {
			'si_row_field' => [$S,$P],
			'src_field' => [$S,$P,$C],
		},
	},
	'view_join' => {
		$TPI_AT_SEQUENCE => [< 
			id pp lhs_src rhs_src join_op 
		>],
		$TPI_PP_NREF => ['view'],
		$TPI_AT_ENUMS => {
			'join_op' => 'join_operator',
		},
		$TPI_AT_NREFS => {
			'lhs_src' => ['view_src'],
			'rhs_src' => ['view_src'],
		},
		$TPI_SI_ATNM => ['id',undef,undef,undef],
		$TPI_MA_ATNMS => [[],['join_op'],['lhs_src','rhs_src']],
		$TPI_CHILD_QUANTS => [
			['view_join_field',1,undef],
		],
		$TPI_MUDI_ATGPS => [
			['ak_lhs_field',[
				['view_join_field',[],[],['lhs_src_field']],
			]],
			['ak_rhs_field',[
				['view_join_field',[],[],['rhs_src_field']],
			]],
		],
	},
	'view_join_field' => {
		$TPI_AT_SEQUENCE => [< 
			id pp lhs_src_field rhs_src_field 
		>],
		$TPI_PP_NREF => ['view_join'],
		$TPI_AT_NREFS => {
			'lhs_src_field' => ['view_src_field'],
			'rhs_src_field' => ['view_src_field'],
		},
		$TPI_SI_ATNM => ['id',undef,undef,undef],
		$TPI_MA_ATNMS => [[],[],['lhs_src_field','rhs_src_field']],
		$TPI_ANCES_ATCORS => {
			'lhs_src_field' => [$S,$P,'lhs_src'],
			'rhs_src_field' => [$S,$P,'rhs_src'],
		},
	},
	'view_compound_elem' => {
		$TPI_AT_SEQUENCE => [< 
			id pp operand
		>],
		$TPI_PP_NREF => ['view'],
		$TPI_AT_NREFS => {
			'operand' => ['view_src'],
		},
		$TPI_SI_ATNM => ['id',undef,undef,undef],
		$TPI_MA_ATNMS => [[],[],['operand']],
	},
	'view_expr' => {
		$TPI_AT_SEQUENCE => [< 
			id pp view_part set_result_field set_src_field call_src_arg 
			call_view_arg call_sroutine_cxt call_sroutine_arg call_uroutine_cxt call_uroutine_arg 
			cont_type valf_literal scalar_data_type valf_src_field valf_result_field 
			valf_p_view_arg valf_p_routine_item valf_seq_next 
			valf_call_view valf_call_sroutine valf_call_uroutine catalog_link
		>],
		$TPI_PP_NREF => ['view_expr','view'],
		$TPI_AT_LITERALS => {
			'valf_literal' => 'misc',
		},
		$TPI_AT_ENUMS => {
			'view_part' => 'view_part',
			'call_sroutine_cxt' => 'standard_routine_context',
			'call_sroutine_arg' => 'standard_routine_arg',
			'cont_type' => 'container_type',
			'valf_call_sroutine' => 'standard_routine',
		},
		$TPI_AT_NREFS => {
			'set_result_field' => ['row_data_type_field'],
			'set_src_field' => ['view_src_field'],
			'call_src_arg' => ['view_src_arg'],
			'call_view_arg' => ['view_arg'],
			'call_uroutine_cxt' => ['routine_context'],
			'call_uroutine_arg' => ['routine_arg'],
			'scalar_data_type' => ['scalar_data_type','scalar_domain'],
			'valf_src_field' => ['view_src_field'],
			'valf_result_field' => ['row_data_type_field'],
			'valf_p_view_arg' => ['view_arg'],
			'valf_p_routine_item' => ['routine_context','routine_arg','routine_var'],
			'valf_seq_next' => ['sequence'],
			'valf_call_view' => ['view'],
			'valf_call_uroutine' => ['routine'],
			'catalog_link' => ['catalog_link'],
		},
		$TPI_SI_ATNM => ['id',undef,undef,undef],
		$TPI_MA_ATNMS => [[],['cont_type'],[]],
		$TPI_LOCAL_ATDPS => [
			[undef,'view_part',undef,[
				[[],[],['set_result_field'],['RESULT'],1],
				[[],[],['set_src_field'],['SET'],1],
				[[],[],['call_src_arg'],['FROM'],1],
			]],
			['valf_literal',undef,undef,[
				[[],[],['scalar_data_type'],[],1],
			]],
			[undef,undef,'valf_call_uroutine',[
				[[],[],['catalog_link'],[],0],
			]],
		],
		$TPI_ANCES_ATCORS => {
			'set_result_field' => [$S,$R,$P],
			'set_src_field' => [$S,$R,$P,$C],
			'call_src_arg' => [$S,$R,$P,$C],
			'call_view_arg' => [$S,$P,'valf_call_view'],
			'call_uroutine_cxt' => [$S,$P,'valf_call_uroutine'],
			'call_uroutine_arg' => [$S,$P,'valf_call_uroutine'],
			'valf_src_field' => [$S,$R,$P,$C],
			'valf_result_field' => [$S,$R,$P],
		},
		$TPI_MUDI_ATGPS => [
			['ak_view_arg',[
				['view_expr',[],[],['call_view_arg']],
			]],
			['ak_sroutine_arg',[
				['view_expr',[],['call_sroutine_cxt'],[]],
				['view_expr',[],['call_sroutine_arg'],[]],
			]],
			['ak_uroutine_arg',[
				['view_expr',[],[],['call_uroutine_cxt']],
				['view_expr',[],[],['call_uroutine_arg']],
			]],
		],
	},
	'routine' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name routine_type return_cont_type 
			return_scalar_data_type return_row_data_type 
			trigger_on trigger_event trigger_per_stmt
		>],
		$TPI_PP_NREF => ['routine','schema','application'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
			'trigger_per_stmt' => 'bool',
		},
		$TPI_AT_ENUMS => {
			'routine_type' => 'routine_type',
			'return_cont_type' => 'container_type',
			'trigger_event' => 'basic_trigger_event',
		},
		$TPI_AT_NREFS => {
			'return_scalar_data_type' => ['scalar_data_type','scalar_domain'],
			'return_row_data_type' => ['row_data_type','row_domain'],
			'trigger_on' => ['table','view'],
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_MA_ATNMS => [[],['routine_type'],[]],
		$TPI_LOCAL_ATDPS => [
			[undef,'routine_type',undef,[
				[[],['return_cont_type'],[],['FUNCTION'],1],
				[[],[],['trigger_on'],['TRIGGER'],1],
				[[],['trigger_event'],[],['TRIGGER'],1],
				[['trigger_per_stmt'],[],[],['TRIGGER'],1],
			]],
			[undef,'return_cont_type',undef,[
				[[],[],['return_scalar_data_type'],['SCALAR','SC_ARY'],1],
				[[],[],['return_row_data_type'],['ROW','RW_ARY'],1],
			]],
		],
		$TPI_REMOTE_ADDR => ['catalog'],
		$TPI_CHILD_QUANTS => [
			['routine_context',0,1],
			['routine_stmt',1,undef],
		],
	},
	'routine_context' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name cont_type conn_link curs_view 
		>],
		$TPI_PP_NREF => ['routine'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
		},
		$TPI_AT_ENUMS => {
			'cont_type' => 'container_type',
		},
		$TPI_AT_NREFS => {
			'conn_link' => ['catalog_link'],
			'curs_view' => ['view'],
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_MA_ATNMS => [[],['cont_type'],[]],
		$TPI_MUTEX_ATGPS => [
			['context',[],[],['conn_link','curs_view'],1],
		],
		$TPI_LOCAL_ATDPS => [
			[undef,'cont_type',undef,[
				[[],[],['conn_link'],['CONN'],1],
				[[],[],['curs_view'],['CURSOR'],1],
			]],
		],
	},
	'routine_arg' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name cont_type scalar_data_type row_data_type
			conn_link curs_view 
		>],
		$TPI_PP_NREF => ['routine'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
		},
		$TPI_AT_ENUMS => {
			'cont_type' => 'container_type',
		},
		$TPI_AT_NREFS => {
			'scalar_data_type' => ['scalar_data_type','scalar_domain'],
			'row_data_type' => ['row_data_type','row_domain'],
			'conn_link' => ['catalog_link'],
			'curs_view' => ['view'],
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_WR_ATNM => 'row_data_type',
		$TPI_MA_ATNMS => [[],['cont_type'],[]],
		$TPI_LOCAL_ATDPS => [
			[undef,'cont_type',undef,[
				[[],[],['scalar_data_type'],['SCALAR','SC_ARY'],1],
				[[],[],['row_data_type'],['ROW','RW_ARY'],1],
				[[],[],['conn_link'],['CONN'],1],
				[[],[],['curs_view'],['CURSOR'],1],
			]],
		],
	},
	'routine_var' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name cont_type scalar_data_type row_data_type
			init_lit_val is_constant conn_link curs_view curs_for_update 
		>],
		$TPI_PP_NREF => ['routine'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
			'init_lit_val' => 'misc',
			'is_constant' => 'bool',
			'curs_for_update' => 'bool',
		},
		$TPI_AT_ENUMS => {
			'cont_type' => 'container_type',
		},
		$TPI_AT_NREFS => {
			'scalar_data_type' => ['scalar_data_type','scalar_domain'],
			'row_data_type' => ['row_data_type','row_domain'],
			'conn_link' => ['catalog_link'],
			'curs_view' => ['view'],
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_WR_ATNM => 'row_data_type',
		$TPI_MA_ATNMS => [[],['cont_type'],[]],
		$TPI_LOCAL_ATDPS => [
			[undef,'cont_type',undef,[
				[[],[],['scalar_data_type'],['SCALAR','SC_ARY'],1],
				[[],[],['row_data_type'],['ROW','RW_ARY'],1],
				[['init_lit_val'],[],[],['SCALAR'],0],
				[['is_constant'],[],[],['SCALAR'],0],
				[[],[],['conn_link'],['CONN'],1],
				[[],[],['curs_view'],['CURSOR'],1],
				[['curs_for_update'],[],[],['CURSOR'],0],
			]],
		],
	},
	'routine_stmt' => {
		$TPI_AT_SEQUENCE => [< 
			id pp block_routine assign_dest call_sroutine call_uroutine catalog_link 
		>],
		$TPI_PP_NREF => ['routine'],
		$TPI_AT_ENUMS => {
			'call_sroutine' => 'standard_routine',
		},
		$TPI_AT_NREFS => {
			'block_routine' => ['routine'],
			'assign_dest' => ['routine_context','routine_arg','routine_var'],
			'call_uroutine' => ['routine'],
			'catalog_link' => ['catalog_link'],
		},
		$TPI_SI_ATNM => ['id',undef,undef,undef],
		$TPI_MUTEX_ATGPS => [
			['stmt_type',[],['call_sroutine'],['block_routine','assign_dest','call_uroutine'],1],
		],
		$TPI_LOCAL_ATDPS => [
			[undef,undef,'call_uroutine',[
				[[],[],['catalog_link'],[],0],
			]],
		],
		$TPI_MUDI_ATGPS => [
			['ak_sroutine_arg',[
				['routine_expr',[],['call_sroutine_cxt'],[]],
				['routine_expr',[],['call_sroutine_arg'],[]],
			]],
			['ak_uroutine_arg',[
				['routine_expr',[],[],['call_uroutine_cxt']],
				['routine_expr',[],[],['call_uroutine_arg']],
			]],
		],
	},
	'routine_expr' => {
		$TPI_AT_SEQUENCE => [< 
			id pp call_sroutine_cxt call_sroutine_arg call_uroutine_cxt call_uroutine_arg 
			cont_type valf_literal scalar_data_type valf_p_routine_item valf_seq_next 
			valf_call_sroutine valf_call_uroutine catalog_link act_on
		>],
		$TPI_PP_NREF => ['routine_expr','routine_stmt'],
		$TPI_AT_LITERALS => {
			'valf_literal' => 'misc',
		},
		$TPI_AT_ENUMS => {
			'call_sroutine_cxt' => 'standard_routine_context',
			'call_sroutine_arg' => 'standard_routine_arg',
			'cont_type' => 'container_type',
			'valf_call_sroutine' => 'standard_routine',
		},
		$TPI_AT_NREFS => {
			'call_uroutine_cxt' => ['routine_context'],
			'call_uroutine_arg' => ['routine_arg'],
			'scalar_data_type' => ['scalar_data_type','scalar_domain'],
			'valf_p_routine_item' => ['routine_context','routine_arg','routine_var'],
			'valf_seq_next' => ['sequence'],
			'valf_call_uroutine' => ['routine'],
			'catalog_link' => ['catalog_link'],
			'act_on' => ['catalog_link','schema','scalar_domain','row_domain',
				'sequence','table','view','routine','user'],
		},
		$TPI_SI_ATNM => ['id',undef,undef,undef],
		$TPI_MA_ATNMS => [[],['cont_type'],[]],
		$TPI_LOCAL_ATDPS => [
			[undef,'cont_type',undef,[
				[[],[],['act_on'],['SRT_NODE'],1],
			]],
			['valf_literal',undef,undef,[
				[[],[],['scalar_data_type'],[],1],
			]],
			[undef,undef,'valf_call_uroutine',[
				[[],[],['catalog_link'],[],0],
			]],
		],
		$TPI_ANCES_ATCORS => {
			'call_uroutine_cxt' => [$S,$P,'valf_call_uroutine'],
			'call_uroutine_arg' => [$S,$P,'valf_call_uroutine'],
		},
		$TPI_MUDI_ATGPS => [
			['ak_sroutine_arg',[
				['routine_expr',[],['call_sroutine_cxt'],[]],
				['routine_expr',[],['call_sroutine_arg'],[]],
			]],
			['ak_uroutine_arg',[
				['routine_expr',[],[],['call_uroutine_cxt']],
				['routine_expr',[],[],['call_uroutine_arg']],
			]],
		],
	},
	'data_storage_product' => {
		$TPI_AT_SEQUENCE => [< 
			id si_name product_code is_memory_based is_file_based is_local_proc is_network_svc
		>],
		$TPI_PP_PSEUDONODE => $SQLRT_L2_TOOL_PSND,
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
			'product_code' => 'cstr',
			'is_memory_based' => 'bool',
			'is_file_based' => 'bool',
			'is_local_proc' => 'bool',
			'is_network_svc' => 'bool',
		},
		$TPI_SI_ATNM => ['id',undef,undef,undef],
		$TPI_MA_ATNMS => [['si_name','product_code'],[],[]],
		$TPI_MUTEX_ATGPS => [
			['type',['is_memory_based','is_file_based','is_local_proc','is_network_svc'],[],[],1],
		],
	},
	'data_link_product' => {
		$TPI_AT_SEQUENCE => [< 
			id si_name product_code is_proxy
		>],
		$TPI_PP_PSEUDONODE => $SQLRT_L2_TOOL_PSND,
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
			'product_code' => 'cstr',
			'is_proxy' => 'bool',
		},
		$TPI_SI_ATNM => ['id',undef,undef,undef],
		$TPI_MA_ATNMS => [['si_name','product_code'],[],[]],
	},
	'catalog_instance' => {
		$TPI_AT_SEQUENCE => [< 
			id si_name blueprint product file_path server_ip server_domain server_port
		>],
		$TPI_PP_PSEUDONODE => $SQLRT_L2_SITE_PSND,
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
			'file_path' => 'cstr',
			'server_ip' => 'cstr',
			'server_domain' => 'cstr',
			'server_port' => 'uint',
		},
		$TPI_AT_NREFS => {
			'blueprint' => ['catalog'],
			'product' => ['data_storage_product'],
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_MA_ATNMS => [[],[],['blueprint','product']],
		$TPI_MUDI_ATGPS => [
			['ak_cat_link_inst',[
				['catalog_link_instance',[],[],['blueprint']],
			]],
		],
	},
	'catalog_instance_opt' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_key value 
		>],
		$TPI_PP_NREF => ['catalog_instance'],
		$TPI_AT_LITERALS => {
			'si_key' => 'cstr',
			'value' => 'misc',
		},
		$TPI_SI_ATNM => [undef,'si_key',undef,undef],
		$TPI_MA_ATNMS => [['value'],[],[]],
	},
	'application_instance' => {
		$TPI_AT_SEQUENCE => [< 
			id si_name blueprint 
		>],
		$TPI_PP_PSEUDONODE => $SQLRT_L2_SITE_PSND,
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'blueprint' => ['application'],
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_MA_ATNMS => [[],[],['blueprint']],
		$TPI_MUDI_ATGPS => [
			['ak_cat_link_inst',[
				['catalog_link_instance',[],[],['blueprint']],
			]],
		],
	},
	'catalog_link_instance' => {
		$TPI_AT_SEQUENCE => [< 
			id pp blueprint product target local_dsn login_name login_pass
		>],
		$TPI_PP_NREF => ['catalog_link_instance','catalog_instance','application_instance'],
		$TPI_AT_LITERALS => {
			'local_dsn' => 'cstr',
			'login_name' => 'cstr',
			'login_pass' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'blueprint' => ['catalog_link'],
			'product' => ['data_link_product'],
			'target' => ['catalog_instance'],
		},
		$TPI_SI_ATNM => ['id',undef,undef,undef],
		$TPI_MA_ATNMS => [[],[],['blueprint','product','target']],
		$TPI_ANCES_ATCORS => {
			'blueprint' => [$S,$P,'blueprint'],
		},
		$TPI_CHILD_QUANTS => [
			['catalog_link_instance',0,1],
		],
	},
	'catalog_link_instance_opt' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_key value 
		>],
		$TPI_PP_NREF => ['catalog_link_instance'],
		$TPI_AT_LITERALS => {
			'si_key' => 'cstr',
			'value' => 'misc',
		},
		$TPI_SI_ATNM => [undef,'si_key',undef,undef],
		$TPI_MA_ATNMS => [['value'],[],[]],
	},
	'user' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_name user_type match_owner password default_schema 
		>],
		$TPI_PP_NREF => ['catalog_instance'],
		$TPI_AT_LITERALS => {
			'si_name' => 'cstr',
			'password' => 'cstr',
		},
		$TPI_AT_ENUMS => {
			'user_type' => 'user_type',
		},
		$TPI_AT_NREFS => {
			'match_owner' => ['owner'],
			'default_schema' => ['schema'],
		},
		$TPI_SI_ATNM => [undef,'si_name',undef,undef],
		$TPI_MA_ATNMS => [[],['user_type'],[]],
		$TPI_LOCAL_ATDPS => [
			[undef,'user_type',undef,[
				[[],[],['match_owner'],['SCHEMA_OWNER'],1],
				[['password'],[],[],['ROOT','SCHEMA_OWNER','DATA_EDITOR'],1],
			]],
		],
		$TPI_ANCES_ATCORS => {
			'match_owner' => [$S,$P,'blueprint'],
			'default_schema' => [$S,$P,'blueprint'],
		},
	},
	'user_role' => {
		$TPI_AT_SEQUENCE => [< 
			id pp si_role 
		>],
		$TPI_PP_NREF => ['user'],
		$TPI_AT_NREFS => {
			'si_role' => ['role'],
		},
		$TPI_SI_ATNM => [undef,undef,undef,'si_role'],
		$TPI_ANCES_ATCORS => {
			'si_role' => [$S,$P,$P,'blueprint'],
		},
	},
	'sql_fragment' => {
		$TPI_AT_SEQUENCE => [< 
			id attach_to product is_inside is_before is_after fragment
		>],
		$TPI_PP_PSEUDONODE => $SQLRT_L2_CIRC_PSND,
		$TPI_AT_LITERALS => {
			'is_inside' => 'bool',
			'is_before' => 'bool',
			'is_after' => 'bool',
			'fragment' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'attach_to' => [],
			'product' => ['data_storage_product'],
		},
		$TPI_SI_ATNM => ['id',undef,undef,undef],
		$TPI_MA_ATNMS => [[],['attach_to'],[]],
		$TPI_MUTEX_ATGPS => [
			['is_where',['is_inside','is_before','is_after'],[],[],0],
		],
	},
);

# This structure is used as a speed-efficiency measure.  It creates a reverse-index of sorts 
# out of each SI_ATNM that resembles and is used as a simpler version of a MUDI_ATGP.
# It makes the distinct constraint property of surrogate node ids faster to enforce.
# This structure's main hash has a key for each Node type or pseudo-Node type that can have primary-child Nodes; 
# the value for each main hash key is a hash whose keys are the names of Node types that can be primary-children 
# of the aforementioned primary-parent types, and whose values are the SI attribute name of the primary-child Node types.
# Any Node types that can not have primary-child Nodes do not appear in the main structure hash.
my %TYPE_CHILD_SI_ATNMS = ();
for %NODE_TYPES.kv -> $_node_type, $_type_info {
	my $si_atnm = $_type_info.{$TPI_SI_ATNM};
	if( my $pp_psnd = $_type_info.{$TPI_PP_PSEUDONODE} ) {
		%TYPE_CHILD_SI_ATNMS{$pp_psnd} ||= {};
		%TYPE_CHILD_SI_ATNMS{$pp_psnd}.{$_node_type} = $si_atnm;
	} else { # no pseudonode, so must be PP attrs
		for $_type_info.{$TPI_PP_NREF} -> $pp_node_type {
			%TYPE_CHILD_SI_ATNMS{$pp_node_type} ||= {};
			%TYPE_CHILD_SI_ATNMS{$pp_node_type}.{$_node_type} = $si_atnm;
		}
	}
}

# These special hash keys are used by the get_all_properties[/*]() methods, 
# and/or by the build*node*() functions and methods for RAD:
my $NAMED_NODE_TYPE is constant = 'NODE_TYPE'; # str - what type of Node we are
my $NAMED_ATTRS     is constant = 'ATTRS'; # hash - all attributes, including 'id' (and 'pp' if appropriate)
my $NAMED_CHILDREN  is constant = 'CHILDREN'; # array - list of primary-child Node descriptors
my $ATTR_ID         is constant = 'id'; # attribute name to use for the node id
my $ATTR_PP         is constant = 'pp'; # attribute name to use for the node's primary parent nref

######################################################################
######################################################################

subtype NodeID of Int where { .defined and $_ > 0 }

######################################################################
######################################################################

role SQL::Routine-0.0.2 {

######################################################################

multi method valid_enumerated_types() returns Hash of Bool {
	return {%ENUMERATED_TYPES.keys.map:{ ($_ => 1) }};
}

multi method valid_enumerated_types( Str $type ) returns Bool {
	return %ENUMERATED_TYPES{$type}.exists;
}

multi method valid_enumerated_type_values( Str $type ) returns Hash of Str {
	%ENUMERATED_TYPES{$type}.exists or return;
	return {%ENUMERATED_TYPES{$type}};
}

multi method valid_enumerated_type_values( Str $type, Str $value ) returns Bool {
	%ENUMERATED_TYPES{$type}.exists or return;
	return %ENUMERATED_TYPES{$type}.{$value}.exists;
}

multi method valid_node_types() returns Hash of Bool {
	return {%NODE_TYPES.keys.map:{ ($_ => 1) }};
}

multi method valid_node_types( Str $type ) returns Bool {
	return %NODE_TYPES{$type}.exists;
}

multi method node_types_with_pseudonode_parents() returns Hash of Str {
	return {%NODE_TYPES.keys.
		grep:{ %NODE_TYPES{$_}.{$TPI_PP_PSEUDONODE} }.
		map:{ ($_ => %NODE_TYPES{$_}.{$TPI_PP_PSEUDONODE}) }};
}

multi method node_types_with_pseudonode_parents( Str $type ) returns Str {
	%NODE_TYPES{$type}.exists or return;
	return %NODE_TYPES{$type}.{$TPI_PP_PSEUDONODE};
}

multi method node_types_with_primary_parent_attributes() returns Hash of Array of Str {
	return {%NODE_TYPES.keys.
		grep:{ %NODE_TYPES{$_}.{$TPI_PP_NREF} }.
		map:{ ($_ => [%NODE_TYPES{$_}.{$TPI_PP_NREF}]) }};
}

multi method node_types_with_primary_parent_attributes( Str $type ) returns Array of Str {
	%NODE_TYPES{$type}.exists or return;
	%NODE_TYPES{$type}.{$TPI_PP_NREF}.exists or return;
	return [%NODE_TYPES{$type}.{$TPI_PP_NREF}];
}

multi method valid_node_type_literal_attributes( Str $type ) returns Hash of Str {
	%NODE_TYPES{$type}.exists or return;
	%NODE_TYPES{$type}.{$TPI_AT_LITERALS}.exists or return;
	return {%NODE_TYPES{$type}.{$TPI_AT_LITERALS}};
}

multi method valid_node_type_literal_attributes( Str $type, Str $attr ) returns Str {
	%NODE_TYPES{$type}.exists or return;
	%NODE_TYPES{$type}.{$TPI_AT_LITERALS}.exists or return;
	return %NODE_TYPES{$type}.{$TPI_AT_LITERALS}.{$attr};
}

multi method valid_node_type_enumerated_attributes( Str $type ) returns Hash of Str {
	%NODE_TYPES{$type}.exists or return;
	%NODE_TYPES{$type}.{$TPI_AT_ENUMS}.exists or return;
	return {%NODE_TYPES{$type}.{$TPI_AT_ENUMS}};
}

multi method valid_node_type_enumerated_attributes( Str $type, Str $attr ) returns Str {
	%NODE_TYPES{$type}.exists or return;
	%NODE_TYPES{$type}.{$TPI_AT_ENUMS}.exists or return;
	return %NODE_TYPES{$type}.{$TPI_AT_ENUMS}.{$attr};
}

multi method valid_node_type_node_ref_attributes( Str $type ) returns Hash of Array of Str {
	%NODE_TYPES{$type}.exists or return;
	%NODE_TYPES{$type}.{$TPI_AT_NREFS}.exists or return;
	return {%NODE_TYPES{$type}.{$TPI_AT_NREFS}.pairs.map:{ ($_.key => [$_.value]) }};
}

multi method valid_node_type_node_ref_attributes( Str $type, Str $attr ) returns Array of Str {
	%NODE_TYPES{$type}.exists or return;
	%NODE_TYPES{$type}.{$TPI_AT_NREFS}.exists or return;
	%NODE_TYPES{$type}.{$TPI_AT_NREFS}.{$attr}.exists or return;
	return [%NODE_TYPES{$type}.{$TPI_AT_NREFS}.{$attr}];
}

multi method valid_node_type_surrogate_id_attributes() {
	%NODE_TYPES{$type}.exists or return;
	return {%NODE_TYPES.pairs.map:{ ($_.key => $_.value.{$TPI_SI_ATNM}.grep:{ $_ }.[0]) }};
}

multi method valid_node_type_surrogate_id_attributes( Str $type ) {
	%NODE_TYPES{$type}.exists or return;
	return %NODE_TYPES{$type}.{$TPI_SI_ATNM}.grep:{ $_ }.[0];
}

######################################################################
# This is a 'protected' method; only sub-classes should invoke it.

method :_serialize_as_perl( $self: %node_dump, Str ?$pad ) returns Str {
	$pad ||= '';
	my $padc = "$pad\t\t";
	my $node_type = %node_dump{$NAMED_NODE_TYPE};
	my @attr_seq = %NODE_TYPES{$node_type}.{$TPI_AT_SEQUENCE};
	my %attrs = %node_dump{$NAMED_ATTRS};
	return (
		$pad~"{\n",
		$pad~"\t'"~$NAMED_NODE_TYPE~"' => '"~$node_type~"',\n",
		(scalar(%attrs.keys) ?? (
			$pad~"\t'"~$NAMED_ATTRS~"' => {\n",
			@attr_seq.grep:{ %attrs{$_}.defined }.map:{
				$pad~"\t\t'"~$_~"' => "~(
					%attrs{$_}.meta.isa(Array) ?? 
						"["~%attrs{$_}.map:{ 
								$_.defined ?? "'"~$self.:_s_a_p_esc($_)~"'" :: "undef"
							}.join( ',' )~"]" :: 
						"'"~$self.:_s_a_p_esc(%attrs{$_})~"'"
				)~",\n"
			},
			$pad~"\t},\n",
		) :: ''),
		(scalar(@{%node_dump{$NAMED_CHILDREN}}) ?? (
			$pad~"\t'"~$NAMED_CHILDREN~"' => [\n",
			%node_dump{$NAMED_CHILDREN}.map:{ $self.:_serialize_as_perl( $_,$padc ) },
			$pad~"\t],\n",
		) :: ''),
		$pad~"},\n",
	).join( '' );
}

method :_s_a_p_esc( Str $text ) returns Str {
	$text ~~ s:g/\\/\\\\/;
	$text ~~ s:g/'/\\'/;
	return $text;
}

######################################################################
# This is a 'protected' method; only sub-classes should invoke it.

method :_serialize_as_xml( $self: %node_dump, Str ?$pad ) returns Str {
	$pad ||= '';
	my $padc = "$pad\t";
	my $node_type = %node_dump{$NAMED_NODE_TYPE};
	my @attr_seq = %NODE_TYPES{$node_type}.{$TPI_AT_SEQUENCE};
	my %attrs = %node_dump{$NAMED_ATTRS};
	return (
		$pad~'<'~$node_type,
		@attr_seq.grep:{ %attrs{$_}.defined }.map:{
			' '~$_~'="'~(
				%attrs{$_}.meta.isa(Array) ?? 
					"["~%attrs{$_}.map:{ 
						$_.defined ?? $self.:_s_a_x_esc($_) :: ""
					}.join( ',' )~"]" :: 
					$self.:_s_a_x_esc(%attrs{$_})
			)~'"' 
		},
		(scalar(@{%node_dump{$NAMED_CHILDREN}}) ?? (
			'>'~"\n",
			%node_dump{$NAMED_CHILDREN}.map:{ $self.:_serialize_as_xml( $_,$padc ) },
			$pad~'</'~$node_type~'>'~"\n",
		) :: ' />'~"\n"),
	).join( '' );
}

method :_s_a_x_esc( Str $text ) returns Str {
	$text ~~ s:g/\&/&amp;/;
	$text ~~ s:g/\"/&quot;/;
	$text ~~ s:g/\>/&gt;/;
	$text ~~ s:g/\</&lt;/;
	return $text;
}

######################################################################
# This is a 'protected' method; only sub-classes should invoke it.

method :_throw_error_message( $self: KeyName $msg_key, Any ?%msg_vars is shape(KeyName) ) {
	# Throws an exception consisting of an object.  A Container property is not 
	# used to store object so things work properly in multi-threaded environment; 
	# an exception is only supposed to affect the thread that calls it.
	if( $self.meta.isa( SQL::Routine::Node ) ) {
		%msg_vars{'NTYPE'} = $self.:node_type;
		%msg_vars{'NID'} = $self.:node_id;
		if( $self.:container ) {
			%msg_vars{'SIDCH'} = $self.get_surrogate_id_chain();
		}
	}
	for %msg_vars.kv -> $var_name, $var_value {
		if( $var_value.meta.isa( Array ) ) {
			%msg_vars{$var_name} = 'PERL_ARRAY:['~$var_value.map:{$_||''}.join(',')~']';
		}
	}
	throw Locale::KeyedText.new_message( $msg_key, %msg_vars );
}

######################################################################
# These are convenience wrapper methods.

method new_container {
	return SQL::Routine::Container.new();
}

method new_node( Str $node_type ) {
	return SQL::Routine::Node.new( $node_type );
}

######################################################################

multi method build_lonely_node( $self: $node_type, %attrs ) {
	my $node = $self.new_node( $node_type );
	%attrs = $self.:_build_node_normalize_attrs( $node, %attrs );
	$node.set_attributes( %attrs );
	return $node;
}

multi method build_lonely_node( $self: %args ) {
	return $self.build_lonely_node( %args{$NAMED_NODE_TYPE}, %args{$NAMED_ATTRS} );
}

method :_build_node_normalize_attrs( SQL::Routine::Node $node, %attrs ) {
	if( %attrs.meta.isa(Hash) ) {
		%attrs = {%attrs}; # copy this, to preserve caller environment
	} elsif( %attrs.defined ) {
		if( %attrs ~~ m/^\d+$/ and %attrs > 0 ) { # looks like a node id
			%attrs = { $ATTR_ID => %attrs };
		} else { # does not look like node id
			%attrs = { (grep:{ $_ } @{%NODE_TYPES{$node.:node_type}.{$TPI_SI_ATNM}})[0] => %attrs };
		}
	} else {
		%attrs = {};
	}
	return %attrs;
}

method build_container( $self: @list, Bool $auto_assert, Bool $auto_ids, Bool $match_surr_ids ) {
	my $container = $self.new_container();
	$auto_assert and $container.auto_assert_deferrable_constraints( 1 );
	$auto_ids and $container.auto_set_node_ids( 1 );
	$match_surr_ids and $container.may_match_surrogate_node_ids( 1 );
	$container.build_child_node_trees( @list );
	return $container;
}

######################################################################

} # role SQL::Routine

######################################################################
######################################################################

class SQL::Routine::Container {
	does SQL::Routine;
	trusts SQL::Routine::Node;
	has Bool $:auto_ass_def_con; # boolean - false by def
		# When this flag is true, SQL::Routine's build_*() methods will
		# automatically invoke assert_deferrable_constraints() on the newly created Node,
		# if it is in this Container, prior to returning it.  The use of this method
		# helps isolate bad input bugs faster by flagging them closer to when they were
		# created; it is especially useful with the build*tree() methods.
	has Bool $:auto_set_nids; # boolean - false by def
		# When this flag is true, SQL::Routine will automatically generate and set a Node Id for 
		# a Node that lacks one as soon as there is an attempt to put that Node in this Container.
		# When this flag is false, a missing Node Id will cause an exception to be raised instead.
	has Bool $:may_match_snids; # boolean - false by def
		# When this flag is true, SQL::Routine will accept a wider range of input values when setting 
		# Node ref attribute values, beyond Node object references and integers representing Node ids to 
		# look up; if other types of values are provided, SQL::Routine will try to look up Nodes based 
		# on other attributes than the Id, usually 'si_name', before giving up on finding a Node to link.
	has SQL::Routine::Node %:all_nodes is shape(NodeID); # hash of Node refs; find any Node by its node_id quickly
	has Array of SQL::Routine::Node %:pseudonodes; # hash of arrays of Node refs
		# This property is for remembering the insert order of Nodes having hardwired pseudonode parents
	has NodeID $:next_free_nid; # uint; next free node id
		# Value is one higher than the highest Node ID that is or was in use by a Node in this Container.
	has Bool $:def_con_tested; # boolean - true by def, false when changes made
		# This property is a status flag which says there have been no changes to the Nodes 
		# in this Container since the last time assert_deferrable_constraints() passed its tests, 
		# and so the current Nodes are still valid.  It is used internally by 
		# assert_deferrable_constraints() to make code faster by avoiding un-necessary 
		# repeated tests from multiple external Container.assert_deferrable_constraints() calls.
		# It is set true on a new empty Container, and set false when any Nodes are moved in 
		# or out of the "well known" state within that Container, or are changed while in that state.
	#has SQL::Routine::Node $:curr_node; # ref to a Node; used when "streaming" to or from XML
		# I may instead make a new inner class for this, and there can be several of these 
		# per container, such as if multiple streams are working in different areas at once; 
		# any Container property would then just have a list of those active objects, 
		# so they can be killed (return links to Container obj broken) if their Container is destroyed.
	# To do: have attribute to indicate an edit in progress 
		# or that there was a failure resulting in inconsistent data;
		# this may be set by a method which partly implements a data change 
		# which is not backed out of, before that function throws an exception;
		# this property may best just be inside the thrown Locale::KeyedText object;
		# OTOH, if users have coarse-grained locks on Containers for threads, we could have a property,
		# since a call to an editing method would check and clear that before the thread releases lock

######################################################################

method new( $class: ) returns SQL::Routine::Container {
	my $container = $class.bless( {} );
	$container.:auto_ass_def_con = 0;
	$container.:auto_set_nids = 0;
	$container.:may_match_snids = 0;
	$container.:all_nodes = {};
	$container.:pseudonodes = { @L2_PSEUDONODE_LIST.map:{ ($_ => []) } };
	$container.:next_free_nid = 1;
	$container.:def_con_tested = 1;
	return $container;
}

######################################################################

method destroy( $container:  ) {
	# Since we probably have circular refs, we must explicitly be destroyed.
	for $container.:all_nodes.values -> $node {
		$node = ();
	}
	$container = ();
}

######################################################################

method auto_assert_deferrable_constraints( $container: Bool ?$new_value ) {
	if( $new_value.defined ) {
		$container.:auto_ass_def_con = $new_value;
	}
	return $container.:auto_ass_def_con;
}

######################################################################

method auto_set_node_ids( $container: Bool ?$new_value ) {
	if( $new_value.defined ) {
		$container.:auto_set_nids = $new_value;
	}
	return $container.:auto_set_nids;
}

######################################################################

method may_match_surrogate_node_ids( $container: Bool ?$new_value ) {
	if( $new_value.defined ) {
		$container.:may_match_snids = $new_value;
	}
	return $container.:may_match_snids;
}

######################################################################

method get_child_nodes( $container: Str ?$node_type ) {
	my $pseudonodes = $container.:pseudonodes;
	if( $node_type.defined ) {
		unless( %NODE_TYPES{$node_type} ) {
			$container.:_throw_error_message( 'SRT_C_GET_CH_NODES_BAD_TYPE', { 'ARGNTYPE' => $node_type } );
		}
		my $pp_pseudonode = %NODE_TYPES{$node_type}.{$TPI_PP_PSEUDONODE} or return [];
		return [grep:{ $_.:node_type eq $node_type } @{$pseudonodes.{$pp_pseudonode}}];
	} else {
		return [@L2_PSEUDONODE_LIST.map:{ @{$pseudonodes.{$_}} }];
	}
}

######################################################################

method find_node_by_id( $container: NodeID $node_id ) {
	$node_id.defined or $container.:_throw_error_message( 'SRT_C_FIND_NODE_BY_ID_NO_ARG_ID' );
	return $container.:all_nodes.{$node_id};
}

######################################################################

method find_child_node_by_surrogate_id( $container: @target_attr_value ) {
	@target_attr_value.defined or $container.:_throw_error_message( 'SRT_C_FIND_CH_ND_BY_SID_NO_ARG_VAL' );
	my ($l2_psn, $chain_first, @chain_rest);
	unless( @target_attr_value.[0].defined ) {
		# The given surrogate id chain starts with [undef,'root',<l2-psn>,<chain-of-node-si>].
		(undef, undef, $l2_psn, $chain_first, @chain_rest) = @target_attr_value;
	} else {
		# The given surrogate id chain starts with [<chain-of-node-si>].
		($chain_first, @chain_rest) = @target_attr_value;
	}
	my $pseudonodes = $container.:pseudonodes;
	my @nodes_to_search;
	if( $l2_psn and @L2_PSEUDONODE_LIST.grep:{ $l2_psn eq $_ } ) {
		# Search only children of a specific pseudo-Node.
		@nodes_to_search = @{$pseudonodes.{$l2_psn}};
	} else {
		# Search children of all pseudo-Nodes.
		@nodes_to_search = @L2_PSEUDONODE_LIST.map:{ @{$pseudonodes.{$_}} };
	}
	for @nodes_to_search -> $child {
		if( my $si_atvl = $child.get_surrogate_id_attribute( 1 ) ) {
			if( $si_atvl eq $chain_first ) {
				return @chain_rest ?? $child.find_child_node_by_surrogate_id( @chain_rest ) :: $child;
			}
		}
	}
	return;
}

######################################################################

method get_next_free_node_id( $container: ) returns NodeID {
	return $container.:next_free_nid;
}

######################################################################

method deferrable_constraints_are_tested( $container: ) returns Bool {
	return $container.:def_con_tested;
}

method assert_deferrable_constraints( $container: ) {
	if( $container.:def_con_tested ) {
		return;
	}
	# Test nodes in the same order that they appear in the Node tree.
	for @L2_PSEUDONODE_LIST -> $pseudonode_name {
		SQL::Routine::Node.:_assert_child_comp_deferrable_constraints( $pseudonode_name, $container );
		for $container.:pseudonodes.{$pseudonode_name} -> $child_node {
			$container.:_assert_deferrable_constraints( $child_node );
		}
	}
	$container.:def_con_tested = 1;
}

method :_assert_deferrable_constraints( $container: SQL::Routine::Node $node ) {
	$node.assert_deferrable_constraints();
	for $node.:prim_child_nrefs -> $child_node {
		$container.:_assert_deferrable_constraints( $child_node );
	}
}

######################################################################

method get_all_properties( $node: Bool ?$links_as_si, Bool ?$want_shortest ) {
	return $container.:_get_all_properties( $links_as_si, $want_shortest );
}

method :_get_all_properties( $node: Bool ?$links_as_si, Bool ?$want_shortest ) {
	my %pseudonodes = $container.:pseudonodes;
	return {
		$NAMED_NODE_TYPE => $SQLRT_L1_ROOT_PSND,
		$NAMED_ATTRS => {},
		$NAMED_CHILDREN => [@L2_PSEUDONODE_LIST.map:{ {
			$NAMED_NODE_TYPE => $_,
			$NAMED_ATTRS => {},
			$NAMED_CHILDREN => [%pseudonodes{$_}.map:{ $_.:_get_all_properties( $links_as_si, $want_shortest ) } ],
		} }],
	};
}

method get_all_properties_as_perl_str( $node: Bool ?$links_as_si, Bool ?$want_shortest ) {
	return $container.:_serialize_as_perl( $container.:_get_all_properties( $links_as_si, $want_shortest ) );
}

method get_all_properties_as_xml_str( $node: Bool ?$links_as_si, Bool ?$want_shortest ) {
	return '<?xml version="1.0" encoding="UTF-8"?>'~"\n"~
		$container.:_serialize_as_xml( $container.:_get_all_properties( $links_as_si, $want_shortest ) );
}

######################################################################

multi method build_node( $container: $node_type, %attrs ) {
	return $container.:_build_node_is_child_or_not( $node_type, %attrs );
}

multi method build_node( $container: %args ) {
	return $container.build_node( %args{$NAMED_NODE_TYPE}, %args{$NAMED_ATTRS} );
}

method :_build_node_is_child_or_not( $container: $node_type, %attrs, $pp_node ) {
	my $node = $container.new_node( $node_type );
	%attrs = $container.:_build_node_normalize_attrs( $node, %attrs );
	if( my $node_id = delete( %attrs.{$ATTR_ID} ) ) {
		$node.set_node_id( $node_id );
	}
	$node.put_in_container( $container );
	my $pp_in_attrs = delete( %attrs.{$ATTR_PP} ); # ensure won't override any $pp_node
	if( $pp_node ) {
		$pp_node.add_child_node( $node );
	} else {
		$pp_in_attrs and $node.set_primary_parent_attribute( $pp_in_attrs );
	}
	$node.set_attributes( %attrs );
	if( $container.:auto_ass_def_con ) {
		try {
			$node.assert_deferrable_constraints(); # check that this Node's own attrs are correct
			CATCH {
				my $exception = $!;
				unless( $exception.get_message_key() eq 'SRT_N_ASDC_CH_N_TOO_FEW_SET' ) {
					throw $exception; # don't trap any other types of exceptions
				}
			}
		}
	}
	return $node;
}

multi method build_child_node( $container: $node_type, %attrs ) {
	if( $node_type eq $SQLRT_L1_ROOT_PSND or @L2_PSEUDONODE_LIST.grep:{ $_ eq $node_type } ) {
		return $container;
	} else { # $node_type is not a valid pseudo-Node
		my $node = $container.:_build_node_is_child_or_not( $node_type, %attrs );
		unless( %NODE_TYPES{$node_type}.{$TPI_PP_PSEUDONODE} ) {
			$node.take_from_container(); # so the new Node doesn't persist
			$container.:_throw_error_message( 'SRT_C_BUILD_CH_ND_NO_PSND', { 'ARGNTYPE' => $node_type } );
		}
		return $node;
	}
}

multi method build_child_node( $container: %args ) {
	return $container.build_child_node_tree( %args{$NAMED_NODE_TYPE}, %args{$NAMED_ATTRS} );
}

method build_child_nodes( $container: @list ) {
	for @list -> $element {
		$container.build_child_node( $element );
	}
}

multi method build_child_node_tree( $container: $node_type, %attrs, @children ) {
	if( $node_type eq $SQLRT_L1_ROOT_PSND or @L2_PSEUDONODE_LIST.grep:{ $_ eq $node_type } ) {
		$container.build_child_node_trees( @children );
		return $container;
	} else { # $node_type is not a valid pseudo-Node
		my $node = $container.build_child_node( $node_type, %attrs );
		$node.build_child_node_trees( @children );
		return $node;
	}
}

multi method build_child_node_tree( $container: %args ) {
	return $container.build_child_node_tree( %args{$NAMED_NODE_TYPE}, %args{$NAMED_ATTRS}, %args{$NAMED_CHILDREN} );
}

method build_child_node_trees( $container: @list ) {
	for @list -> $element {
		$container.build_child_node_tree( $element );
	}
}

######################################################################

} # class SQL::Routine::Container

######################################################################
######################################################################

class SQL::Routine::Node {
	does SQL::Routine;
	trusts SQL::Routine::Container;
		# The C version will have the following comprise fields in a Node struct;
		# all fields will be integers or memory references or enums; none will be strings.
	has Str $:node_type; # str (enum) - what type of Node this is, can not change once set
		# The Node type is the only property which absolutely can not change, and is set when object created.
		# (All other Node properties start out undefined or false, and are set separately from object creation.)
		# C version of this will be an enumerated value.
	has NodeID $:node_id; # uint - unique identifier attribute for this node within container+type
		# Node id must be set when/before Node is put in a container; may lack one when not in container.
		# C version of this will be an unsigned integer.
		# This property corresponds to a Node attribute named 'id'.
	has SQL::Routine::Node $:pp_nref; # Node - special Node attr which points to primary-parent Node (or id reps other Node)
		# C version of this will be either an array or a single struct, to handle pointer vs uint
		# Value can only be actual reference when Node is in a Container, and pointed to must be in same
		# This property is analagous to a non-existing AT_NREFS element whose name is "pp".
		# When converting to XML, this "pp" attribute won't become an XML attr (redundant)
	has Str %:at_literals; # hash (enum,lit) - attrs of Node which are non-enum, non-id literal values
		# C version of this will be an array (pointer) of Literal structs.
		# We already know what all the attributes can be for each node type, so the size of the array 
		# will be fixed and known in advance, allowing it to be all allocated with one malloc() call.
		# Each attribute struct would be at a specific array index; 
		# C macros/constants will give names to the indices, like with the hash keys for the above.
	has Str %:at_enums; # hash (enum,enum) - attrs of Node which are enumerated values
		# C version of this will be an array (pointer) of enumerated values.
	has SQL::Routine::Node %:at_nrefs; # hash (enum,Node) - attrs of Node which point to other Nodes (or ids rep other Nodes)
		# C version of this will be either multiple arrays or a single array of structs, to handle pointer vs uint
		# Hash elements can only be actual references when Node is in a Container, and pointed to must be in same
	has SQL::Routine::Container $:container; # ref to Container this Node lives in
		# C version of this would be a pointer to a Container struct
	has SQL::Routine::Node @:prim_child_nrefs; # array - list of refs to other Nodes having actual refs to this one
		# We use this to reciprocate actual refs from the PP_NREF property of other Nodes to us.
		# When converting to XML, only child Nodes linked through PRIM_CHILD_NREFS are rendered.
		# Every Node in this list is guaranteed to appear in this list exactly once.
	has SQL::Routine::Node @:link_child_nrefs; # array - list of refs to other Nodes having actual refs to this one
		# We use this to reciprocate actual refs from the AT_NREFS property of other Nodes to us.
		# When converting to XML, only child Nodes linked through PRIM_CHILD_NREFS are rendered.
		# C version will be a double-linked list with each element representing a Node struct.
		# Each Node in this list may possibly appear in this list more than once.
		# It is important to ensure that if a Node links to us multiple times (via multiple AT_NREFS) 
		# then we include the other Node in our child list just as many times; eg: 2 here means 2 back; 
		# however, when rendering to XML, we only render a Node once, and not as many times as linked; 
		# it is also possible that we may never be put in this situation from real-world usage.
		# Note that in the above situation, a normalized child list would have the above two links sitting 
		# adjacent to each other; put_in_container() will do this, but subsequent calls to 
		# set_node_ref_attribute() might not.  In the interest of simplicity, any method that wants to 
		# change the order of a child list should also normalize any multiple same-child occurrances.

######################################################################

method new( $class: Str $node_type ) returns SQL::Routine::Node {
	my $node = $class.bless( {} );

	$node_type.defined or $node.:_throw_error_message( 'SRT_N_NEW_NODE_NO_ARGS' );
	my %type_info = %NODE_TYPES{$node_type};
	unless( %type_info ) {
		$node.:_throw_error_message( 'SRT_N_NEW_NODE_BAD_TYPE', { 'ARGNTYPE' => $node_type } );
	}

	$node.:node_type = $node_type;
	$node.:node_id = undef;
	$node.:pp_nref = undef;
	$node.:at_literals = {};
	$node.:at_enums = {};
	$node.:at_nrefs = {};
	$node.:container = undef;
	$node.:prim_child_nrefs = [];
	$node.:link_child_nrefs = [];

	return $node;
}

######################################################################

method delete_node( $node: ) {
	if( $node.:container ) {
		$node.:_throw_error_message( 'SRT_N_DEL_NODE_IN_CONT' );
	}

	# Ultimately the pure-Perl version of this method is a no-op because once 
	# a Node is not in a Container, there are no references to it by any 
	# SQL::Routine/::* object; it will vanish when external refs go away.
	# This function is a placeholder for the C version, which will require 
	# explicit memory deallocation.
}

######################################################################

method get_node_type( $node: ) {
	return $node.:node_type;
}

######################################################################

method get_node_id( $node: ) {
	return $node.:node_id;
}

method clear_node_id( $node: ) {
	if( $node.:container ) {
		$node.:_throw_error_message( 'SRT_N_CLEAR_NODE_ID_IN_CONT' );
	}
	$node.:node_id = undef;
}

method set_node_id( $node: $new_id ) {
	$new_id.defined or $node.:_throw_error_message( 'SRT_N_SET_NODE_ID_NO_ARGS' );

	unless( $new_id ~~ m/^\d+$/ and $new_id > 0 ) {
		$node.:_throw_error_message( 'SRT_N_SET_NODE_ID_BAD_ARG', { 'ARG' => $new_id } );
	}

	if( !$node.:container ) {
		$node.:node_id = $new_id;
		return;
	}

	# We would never get here if $node didn't also have a NODE_ID
	my $old_id = $node.:node_id;

	if( $new_id == $old_id ) {
		return; # no-op; new id same as old
	}
	my $rh_cal = $node.:container.:all_nodes;

	if( $rh_cal.{$new_id} ) {
		$node.:_throw_error_message( 'SRT_N_SET_NODE_ID_DUPL_ID', { 'ARG' => $new_id } );
	}

	# The following seq should leave state consistent or recoverable if the thread dies
	$rh_cal.{$new_id} = $node; # temp reserve new+old
	$node.:node_id = $new_id; # change self from old to new
	delete( $rh_cal.{$old_id} ); # now only new reserved
	if( $node.:container ) {
		$node.:container.:def_con_tested = 0; # A "Well Known" Node was changed.
	}

	# Now adjust our "next free node id" counter if appropriate
	if( $new_id >= $node.:container.:next_free_nid ) {
		$node.:container.:next_free_nid = 1 + $new_id;
	}
}

######################################################################

method get_primary_parent_attribute( $node: ) {
	%NODE_TYPES{$node.:node_type}.{$TPI_PP_NREF} or $node.:_throw_error_message( 'SRT_N_GET_PP_AT_NO_PP_AT' );
	return $node.:_get_primary_parent_attribute();
}

method :_get_primary_parent_attribute( $node: ) {
	return $node.:pp_nref;
}

method clear_primary_parent_attribute( $node: ) {
	%NODE_TYPES{$node.:node_type}.{$TPI_PP_NREF} or $node.:_throw_error_message( 'SRT_N_CLEAR_PP_AT_NO_PP_AT' );
	$node.:_clear_primary_parent_attribute();
}

method :_clear_primary_parent_attribute( $node: ) {
	my $pp_node = $node.:pp_nref or return; # no-op; attr not set
	if( ref($pp_node) eq ref($node) ) {
		# The attribute value is a Node object, so clear its link back.
		my $siblings = $pp_node.:prim_child_nrefs;
		@siblings = grep:{ $_ ne $node } @siblings; # remove the occurance
	}
	$node.:pp_nref = undef; # removes link to primary-parent, if any
	if( $node.:container ) {
		$node.:container.:def_con_tested = 0; # A "Well Known" Node was changed.
	}
}

method set_primary_parent_attribute( $node: $attr_value ) {
	my $exp_node_types = %NODE_TYPES{$node.:node_type}.{$TPI_PP_NREF} or 
		$node.:_throw_error_message( 'SRT_N_SET_PP_AT_NO_PP_AT' );
	$attr_value.defined or $node.:_throw_error_message( 'SRT_N_SET_PP_AT_NO_ARG_VAL' );
	$node.:_set_primary_parent_attribute( $exp_node_types, $attr_value );
}

method :_set_primary_parent_attribute( $node: $exp_node_types, $attr_value ) {
	$attr_value = $node.:_normalize_primary_parent_or_node_ref_attribute_value( 
		'SRT_N_SET_PP_AT', $ATTR_PP, $exp_node_types, $attr_value );

	if( $node.:pp_nref and $attr_value eq $node.:pp_nref ) {
		return; # no-op; new attribute value same as old
	}

	if( ref($attr_value) eq ref($node) ) {
		# Attempt is to link two Nodes in the same Container; it would be okay, except 
		# that we still have to check for circular primary parent Node references.
		my $pp_node = $attr_value;
		do { # Also make sure we aren't trying to link to ourself.
			if( $pp_node eq $node ) {
				$node.:_throw_error_message( 'SRT_N_SET_PP_AT_CIRC_REF' );
			}
		} while( $pp_node = $pp_node.:pp_nref );
		# For simplicity, we assume circular refs via Node-ref attrs other than 'pp' are impossible.
	}

	$node.:_clear_primary_parent_attribute(); # clears any existing link through this attribute
	$node.:pp_nref = $attr_value;
	if( ref($attr_value) eq ref($node) ) {
		# The attribute value is a Node object, so that Node should link back now.
		push( @{$attr_value.:prim_child_nrefs}, $node );
	}
	if( $node.:container ) {
		$node.:container.:def_con_tested = 0; # A "Well Known" Node was changed.
	}
}

######################################################################

method get_literal_attribute( $node: Str $attr_name ) {
	$attr_name.defined or $node.:_throw_error_message( 'SRT_N_GET_LIT_AT_NO_ARGS' );
	my %type_info = %NODE_TYPES{$node.:node_type};
	%type_info{$TPI_AT_LITERALS} && %type_info{$TPI_AT_LITERALS}.{$attr_name} or 
		$node.:_throw_error_message( 'SRT_N_GET_LIT_AT_INVAL_NM', { 'ATNM' => $attr_name } );
	return $node.:_get_literal_attribute( $attr_name );
}

method :_get_literal_attribute( $node: Str $attr_name ) {
	return $node.:at_literals.{$attr_name};
}

method get_literal_attributes( $node: ) {
	return {%{$node.:at_literals}};
}

method clear_literal_attribute( $node: Str $attr_name ) {
	$attr_name.defined or $node.:_throw_error_message( 'SRT_N_CLEAR_LIT_AT_NO_ARGS' );
	my %type_info = %NODE_TYPES{$node.:node_type};
	%type_info{$TPI_AT_LITERALS} && %type_info{$TPI_AT_LITERALS}.{$attr_name} or 
		$node.:_throw_error_message( 'SRT_N_CLEAR_LIT_AT_INVAL_NM', { 'ATNM' => $attr_name } );
	$node.:_clear_literal_attribute( $attr_name );
}

method :_clear_literal_attribute( $node: Str $attr_name ) {
	delete( $node.:at_literals.{$attr_name} );
	if( $node.:container ) {
		$node.:container.:def_con_tested = 0; # A "Well Known" Node was changed.
	}
}

method clear_literal_attributes( $node: ) {
	$node.:at_literals = {};
	if( $node.:container ) {
		$node.:container.:def_con_tested = 0; # A "Well Known" Node was changed.
	}
}

method set_literal_attribute( $node: Str $attr_name, $attr_value ) {
	$attr_name.defined or $node.:_throw_error_message( 'SRT_N_SET_LIT_AT_NO_ARGS' );
	my %type_info = %NODE_TYPES{$node.:node_type};
	my $exp_lit_type = %type_info{$TPI_AT_LITERALS} && %type_info{$TPI_AT_LITERALS}.{$attr_name} or 
		$node.:_throw_error_message( 'SRT_N_SET_LIT_AT_INVAL_NM', { 'ATNM' => $attr_name } );
	$attr_value.defined or $node.:_throw_error_message( 'SRT_N_SET_LIT_AT_NO_ARG_VAL', { 'ATNM' => $attr_name } );
	$node.:_set_literal_attribute( $attr_name, $exp_lit_type, $attr_value );
}

method :_set_literal_attribute( $node: Str $attr_name, $exp_lit_type, $attr_value ) {
	if( ref($attr_value) ) {
		$node.:_throw_error_message( 'SRT_N_SET_LIT_AT_INVAL_V_IS_REF', 
			{ 'ATNM' => $attr_name, 'ARG_REF_TYPE' => ref($attr_value) } );
	}

	my $node_type = $node.:node_type;

	if( $exp_lit_type eq 'bool' ) {
		unless( $attr_value eq '0' or $attr_value eq '1' ) {
			$node.:_throw_error_message( 'SRT_N_SET_LIT_AT_INVAL_V_BOOL', 
				{ 'ATNM' => $attr_name, 'ARG' => $attr_value } );
		}

	} elsif( $exp_lit_type eq 'uint' ) {
		unless( $attr_value ~~ m/^\d+$/ and $attr_value > 0 ) {
			$node.:_throw_error_message( 'SRT_N_SET_LIT_AT_INVAL_V_UINT', 
				{ 'ATNM' => $attr_name, 'ARG' => $attr_value } );
		}

	} elsif( $exp_lit_type eq 'sint' ) {
		unless( $attr_value ~~ m/^-?\d+$/ ) {
			$node.:_throw_error_message( 'SRT_N_SET_LIT_AT_INVAL_V_SINT', 
				{ 'ATNM' => $attr_name, 'ARG' => $attr_value } );
		}

	} else {} # $exp_lit_type eq 'cstr' or 'misc'; no change to value needed

	$node.:at_literals.{$attr_name} = $attr_value;
	if( $node.:container ) {
		$node.:container.:def_con_tested = 0; # A "Well Known" Node was changed.
	}
}

method set_literal_attributes( $node: %attrs ) {
	%attrs.defined or $node.:_throw_error_message( 'SRT_N_SET_LIT_ATS_NO_ARGS' );
	unless( %attrs.meta.isa(Hash) ) {
		$node.:_throw_error_message( 'SRT_N_SET_LIT_ATS_BAD_ARGS', { 'ARG' => %attrs } );
	}
	for %attrs.kv -> $attr_name, $attr_value {
		$node.set_literal_attribute( $attr_name, $attr_value );
	}
}

######################################################################

method get_enumerated_attribute( $node: Str $attr_name ) {
	$attr_name.defined or $node.:_throw_error_message( 'SRT_N_GET_ENUM_AT_NO_ARGS' );
	my %type_info = %NODE_TYPES{$node.:node_type};
	%type_info{$TPI_AT_ENUMS} && %type_info{$TPI_AT_ENUMS}.{$attr_name} or 
		$node.:_throw_error_message( 'SRT_N_GET_ENUM_AT_INVAL_NM', { 'ATNM' => $attr_name } );
	return $node.:_get_enumerated_attribute( $attr_name );
}

method :_get_enumerated_attribute( $node: Str $attr_name ) {
	return $node.:at_enums.{$attr_name};
}

method get_enumerated_attributes( $node: ) {
	return {%{$node.:at_enums}};
}

method clear_enumerated_attribute( $node: Str $attr_name ) {
	$attr_name.defined or $node.:_throw_error_message( 'SRT_N_CLEAR_ENUM_AT_NO_ARGS' );
	my %type_info = %NODE_TYPES{$node.:node_type};
	%type_info{$TPI_AT_ENUMS} && %type_info{$TPI_AT_ENUMS}.{$attr_name} or 
		$node.:_throw_error_message( 'SRT_N_CLEAR_ENUM_AT_INVAL_NM', { 'ATNM' => $attr_name } );
	$node.:_clear_enumerated_attribute( $attr_name );
}

method :_clear_enumerated_attribute( $node: Str $attr_name ) {
	delete( $node.:at_enums.{$attr_name} );
	if( $node.:container ) {
		$node.:container.:def_con_tested = 0; # A "Well Known" Node was changed.
	}
}

method clear_enumerated_attributes( $node: ) {
	$node.:at_enums = {};
	if( $node.:container ) {
		$node.:container.:def_con_tested = 0; # A "Well Known" Node was changed.
	}
}

method set_enumerated_attribute( $node: Str $attr_name, $attr_value ) {
	$attr_name.defined or $node.:_throw_error_message( 'SRT_N_SET_ENUM_AT_NO_ARGS' );
	my %type_info = %NODE_TYPES{$node.:node_type};
	my $exp_enum_type = %type_info{$TPI_AT_ENUMS} && %type_info{$TPI_AT_ENUMS}.{$attr_name} or 
		$node.:_throw_error_message( 'SRT_N_SET_ENUM_AT_INVAL_NM', { 'ATNM' => $attr_name } );
	$attr_value.defined or $node.:_throw_error_message( 'SRT_N_SET_ENUM_AT_NO_ARG_VAL', { 'ATNM' => $attr_name } );
	$node.:_set_enumerated_attribute( $attr_name, $exp_enum_type, $attr_value );
}

method :_set_enumerated_attribute( $node: Str $attr_name, $exp_enum_type, $attr_value ) {
	unless( %ENUMERATED_TYPES{$exp_enum_type}.{$attr_value} ) {
		$node.:_throw_error_message( 'SRT_N_SET_ENUM_AT_INVAL_V', { 'ATNM' => $attr_name, 
			'ENUMTYPE' => $exp_enum_type, 'ARG' => $attr_value } );
	}

	$node.:at_enums.{$attr_name} = $attr_value;
	if( $node.:container ) {
		$node.:container.:def_con_tested = 0; # A "Well Known" Node was changed.
	}
}

method set_enumerated_attributes( $node: %attrs ) {
	%attrs.defined or $node.:_throw_error_message( 'SRT_N_SET_ENUM_ATS_NO_ARGS' );
	unless( %attrs.meta.isa(Hash) ) {
		$node.:_throw_error_message( 'SRT_N_SET_ENUM_ATS_BAD_ARGS', { 'ARG' => %attrs } );
	}
	for %attrs.kv -> $attr_name, $attr_value {
		$node.set_enumerated_attribute( $attr_name, $attr_value );
	}
}

######################################################################

method get_node_ref_attribute( $node: Str $attr_name, Bool ?$get_target_si ) {
	$attr_name.defined or $node.:_throw_error_message( 'SRT_N_GET_NREF_AT_NO_ARGS' );
	my %type_info = %NODE_TYPES{$node.:node_type};
	%type_info{$TPI_AT_NREFS} && %type_info{$TPI_AT_NREFS}.{$attr_name} or 
		$node.:_throw_error_message( 'SRT_N_GET_NREF_AT_INVAL_NM', { 'ATNM' => $attr_name } );
	return $node.:_get_node_ref_attribute( $attr_name, $get_target_si );
}

method :_get_node_ref_attribute( $node: Str $attr_name, Bool ?$get_target_si ) {
	my $attr_val = $node.:at_nrefs.{$attr_name};
	if( $get_target_si and $node.:container and $attr_val.defined ) {
		return $attr_val.get_surrogate_id_attribute( $get_target_si );
	} else {
		return $attr_val;
	}
}

method get_node_ref_attributes( $node: Bool ?$get_target_si ) {
	my %at_nrefs = $node.:at_nrefs;
	if( $get_target_si and $node.:container ) {
		return { %at_nrefs.values.map:{ ( 
				$_.get_surrogate_id_attribute( $get_target_si )
			) } };
	} else {
		return {%at_nrefs};
	}
}

method clear_node_ref_attribute( $node: Str $attr_name ) {
	$attr_name.defined or $node.:_throw_error_message( 'SRT_N_CLEAR_NREF_AT_NO_ARGS' );
	my %type_info = %NODE_TYPES{$node.:node_type};
	%type_info{$TPI_AT_NREFS} && %type_info{$TPI_AT_NREFS}.{$attr_name} or 
		$node.:_throw_error_message( 'SRT_N_CLEAR_NREF_AT_INVAL_NM', { 'ATNM' => $attr_name } );
	$node.:_clear_node_ref_attribute( $attr_name );
}

method :_clear_node_ref_attribute( $node: Str $attr_name ) {
	my $attr_value = $node.:at_nrefs.{$attr_name} or return; # no-op; attr not set
	if( ref($attr_value) eq ref($node) ) {
		# The attribute value is a Node object, so clear its link back.
		my $ra_children_of_parent = $attr_value.:link_child_nrefs;
		for 0..$#{$ra_children_of_parent} -> $i {
			if( $ra_children_of_parent.[$i] eq $node ) {
				# remove first instance of $node from it's parent's child list
				splice( @ra_children_of_parent, $i, 1 );
				last;
			}
		}
	}
	delete( $node.:at_nrefs.{$attr_name} ); # removes link to link-parent, if any
	if( $node.:container ) {
		$node.:container.:def_con_tested = 0; # A "Well Known" Node was changed.
	}
}

method clear_node_ref_attributes( $node: ) {
	for $node.:at_nrefs.keys -> $attr_name {
		$node.:_clear_node_ref_attribute( $attr_name );
	}
	if( $node.:container ) {
		$node.:container.:def_con_tested = 0; # A "Well Known" Node was changed.
	}
}

method set_node_ref_attribute( $node: Str $attr_name, $attr_value ) {
	$attr_name.defined or $node.:_throw_error_message( 'SRT_N_SET_NREF_AT_NO_ARGS' );
	my %type_info = %NODE_TYPES{$node.:node_type};
	my $exp_node_types = %type_info{$TPI_AT_NREFS} && %type_info{$TPI_AT_NREFS}.{$attr_name} or 
		$node.:_throw_error_message( 'SRT_N_SET_NREF_AT_INVAL_NM', { 'ATNM' => $attr_name } );
	$attr_value.defined or $node.:_throw_error_message( 'SRT_N_SET_NREF_AT_NO_ARG_VAL', { 'ATNM' => $attr_name } );
	$node.:_set_node_ref_attribute( $attr_name, $exp_node_types, $attr_value );
}

method :_set_node_ref_attribute( $node: Str $attr_name, $exp_node_types, $attr_value ) {
	$attr_value = $node.:_normalize_primary_parent_or_node_ref_attribute_value( 
		'SRT_N_SET_NREF_AT', $attr_name, $exp_node_types, $attr_value );

	if( $node.:at_nrefs.{$attr_name} and $attr_value eq $node.:at_nrefs.{$attr_name} ) {
		return; # no-op; new attribute value same as old
	}

	$node.:_clear_node_ref_attribute( $attr_name ); # clears any existing link through this attribute
	$node.:at_nrefs.{$attr_name} = $attr_value;
	if( ref($attr_value) eq ref($node) ) {
		# The attribute value is a Node object, so that Node should link back now.
		push( @{$attr_value.:link_child_nrefs}, $node );
	}
	if( $node.:container ) {
		$node.:container.:def_con_tested = 0; # A "Well Known" Node was changed.
	}
}

method :_normalize_primary_parent_or_node_ref_attribute_value( $node: Str $error_key_pfx, Str $attr_name, $exp_node_types, $attr_value ) {
	if( ref($attr_value) eq ref($node) ) {
		# We were given a Node object for a new attribute value.
		unless( grep:{ $attr_value.:node_type eq $_ } @exp_node_types ) {
			$node.:_throw_error_message( $error_key_pfx~'_WRONG_NODE_TYPE', { 'ATNM' => $attr_name, 
				'EXPNTYPE' => $exp_node_types, 'ARGNTYPE' => $attr_value.:node_type } );
		}
		if( $attr_value.:container and $node.:container ) {
			unless( $attr_value.:container eq $node.:container ) {
				$node.:_throw_error_message( $error_key_pfx~'_DIFF_CONT', { 'ATNM' => $attr_name } );
			}
			# If we get here, both Nodes are in the same Container and can link
		} elsif( $attr_value.:container or $node.:container ) {
			$node.:_throw_error_message( $error_key_pfx~'_ONE_CONT', { 'ATNM' => $attr_name } );
		} elsif( !$attr_value.:node_id ) {
			# both Nodes are not in Containers, and $attr_value has no Node Id
			$node.:_throw_error_message( $error_key_pfx~'_MISS_NID', { 'ATNM' => $attr_name } );
		} else {
			# both Nodes are not in Containers, and $attr_value has Node Id, so can link
			$attr_value = $attr_value.:node_id;
		} 

	} elsif( $attr_value ~~ m/^\d+$/ and $attr_value > 0 ) {
		# We were given a Node Id for a new attribute value.
		if( my $container = $node.:container ) {
			my $searched_attr_value = $container.:all_nodes.{$attr_value};
			unless( $searched_attr_value and grep:{ $searched_attr_value.:node_type eq $_ } @exp_node_types ) {
				$node.:_throw_error_message( $error_key_pfx~'_NONEX_NID', 
					{ 'ATNM' => $attr_name, 'ARG' => $attr_value, 'EXPNTYPE' => $exp_node_types } );
			}
			$attr_value = $searched_attr_value;
		}

	} else {
		# We were given a Surrogate Node Id for a new attribute value.
		if( $attr_name eq $ATTR_PP ) {
			# This should only be encountered by set_primary_parent_attribute().
			$node.:_throw_error_message( 'SRT_N_SET_PP_AT_NO_ALLOW_SID_FOR_PP', { 'ARG' => $attr_value } );
		}
		# These next two should only be encountered by set_node_ref_attribute().
		unless( $node.:container and $node.:container.:may_match_snids ) {
			$node.:_throw_error_message( 'SRT_N_SET_NREF_AT_NO_ALLOW_SID', { 'ATNM' => $attr_name, 'ARG' => $attr_value } );
		}
		my $searched_attr_values = $node.find_node_by_surrogate_id( $attr_name, $attr_value );
		unless( $searched_attr_values ) {
			$node.:_throw_error_message( 'SRT_N_SET_NREF_AT_NONEX_SID', 
				{ 'ATNM' => $attr_name, 'ARG' => $attr_value, 'EXPNTYPE' => $exp_node_types } );
		}
		if( @searched_attr_values > 1 ) {
			$node.:_throw_error_message( 'SRT_N_SET_NREF_AT_AMBIG_SID', 
				{ 'ATNM' => $attr_name, 'ARG' => $attr_value, 'EXPNTYPE' => $exp_node_types, 
				'CANDIDATES' => [';', @searched_attr_values.map:{ (@{$_.:_get_surrogate_id_chain()},';') }] } );
		}
		$attr_value = $searched_attr_values.[0];
	}

	return $attr_value;
}

method set_node_ref_attributes( $node: %attrs ) {
	%attrs.defined or $node.:_throw_error_message( 'SRT_N_SET_NREF_ATS_NO_ARGS' );
	unless( %attrs.meta.isa(Hash) ) {
		$node.:_throw_error_message( 'SRT_N_SET_NREF_ATS_BAD_ARGS', { 'ARG' => %attrs } );
	}
	for %attrs.kv -> $attr_name, $attr_value {
		$node.set_node_ref_attribute( $attr_name, $attr_value );
	}
}

######################################################################

method get_surrogate_id_attribute( $node: Bool ?$get_target_si ) {
	my ($id, $lit, $enum, $nref) = @{%NODE_TYPES{$node.:node_type}.{$TPI_SI_ATNM}};
	$id and return $node.get_node_id();
	$lit and return $node.:_get_literal_attribute( $lit );
	$enum and return $node.:_get_enumerated_attribute( $enum );
	$nref and return $node.:_get_node_ref_attribute( $nref, $get_target_si );
}

method clear_surrogate_id_attribute( $node: ) {
	my ($id, $lit, $enum, $nref) = @{%NODE_TYPES{$node.:node_type}.{$TPI_SI_ATNM}};
	$id and return $node.clear_node_id();
	$lit and return $node.:_clear_literal_attribute( $lit );
	$enum and return $node.:_clear_enumerated_attribute( $enum );
	$nref and return $node.:_clear_node_ref_attribute( $nref );
}

method set_surrogate_id_attribute( $node: $attr_value ) {
	$attr_value.defined or $node.:_throw_error_message( 'SRT_N_SET_SI_AT_NO_ARGS' );
	my %type_info = %NODE_TYPES{$node.:node_type};
	my ($id, $lit, $enum, $nref) = @{%type_info{$TPI_SI_ATNM}};
	$id and return $node.set_node_id( $attr_value );
	$lit and return $node.:_set_literal_attribute( $lit, %type_info{$TPI_AT_LITERALS}.{$lit}, $attr_value );
	$enum and return $node.:_set_enumerated_attribute( $enum, %type_info{$TPI_AT_ENUMS}.{$enum}, $attr_value );
	$nref and return $node.:_set_node_ref_attribute( $nref, %type_info{$TPI_AT_NREFS}.{$nref}, $attr_value );
}

######################################################################

method get_attribute( $node: Str $attr_name, Bool ?$get_target_si ) {
	$attr_name.defined or $node.:_throw_error_message( 'SRT_N_GET_AT_NO_ARGS' );
	my %type_info = %NODE_TYPES{$node.:node_type};
	$attr_name eq $ATTR_ID and return $node.get_node_id();
	$attr_name eq $ATTR_PP && %type_info{$TPI_PP_NREF} and 
		return $node.:_get_primary_parent_attribute();
	%type_info{$TPI_AT_LITERALS} && %type_info{$TPI_AT_LITERALS}.{$attr_name} and 
		return $node.:_get_literal_attribute( $attr_name );
	%type_info{$TPI_AT_ENUMS} && %type_info{$TPI_AT_ENUMS}.{$attr_name} and 
		return $node.:_get_enumerated_attribute( $attr_name );
	%type_info{$TPI_AT_NREFS} && %type_info{$TPI_AT_NREFS}.{$attr_name} and 
		return $node.:_get_node_ref_attribute( $attr_name, $get_target_si );
	$node.:_throw_error_message( 'SRT_N_GET_AT_INVAL_NM', { 'ATNM' => $attr_name } );
}

method get_attributes( $node: Bool ?$get_target_si ) {
	return {
		$ATTR_ID => $node.get_node_id(),
		(%NODE_TYPES{$node.:node_type}.{$TPI_PP_NREF} ?? 
			($ATTR_PP => $node.:_get_primary_parent_attribute()) :: ()),
		%{$node.get_literal_attributes()},
		%{$node.get_enumerated_attributes()},
		%{$node.get_node_ref_attributes( $get_target_si )},
	};
}

method clear_attribute( $node: Str $attr_name ) {
	$attr_name.defined or $node.:_throw_error_message( 'SRT_N_CLEAR_AT_NO_ARGS' );
	my %type_info = %NODE_TYPES{$node.:node_type};
	$attr_name eq $ATTR_ID and return $node.clear_node_id();
	$attr_name eq $ATTR_PP && %type_info{$TPI_PP_NREF} and 
		return $node.:_clear_primary_parent_attribute();
	%type_info{$TPI_AT_LITERALS} && %type_info{$TPI_AT_LITERALS}.{$attr_name} and 
		return $node.:_clear_literal_attribute( $attr_name );
	%type_info{$TPI_AT_ENUMS} && %type_info{$TPI_AT_ENUMS}.{$attr_name} and 
		return $node.:_clear_enumerated_attribute( $attr_name );
	%type_info{$TPI_AT_NREFS} && %type_info{$TPI_AT_NREFS}.{$attr_name} and 
		return $node.:_clear_node_ref_attribute( $attr_name );
	$node.:_throw_error_message( 'SRT_N_CLEAR_AT_INVAL_NM', { 'ATNM' => $attr_name } );
}

method clear_attributes( $node: ) {
	$node.clear_node_id();
	%NODE_TYPES{$node.:node_type}.{$TPI_PP_NREF} and $node.:_clear_primary_parent_attribute();
	$node.clear_literal_attributes();
	$node.clear_enumerated_attributes();
	$node.clear_node_ref_attributes();
}

method set_attribute( $node: Str $attr_name, $attr_value ) {
	$attr_name.defined or $node.:_throw_error_message( 'SRT_N_SET_AT_NO_ARGS' );
	$attr_value.defined or $node.:_throw_error_message( 'SRT_N_SET_AT_NO_ARG_VAL', { 'ATNM' => $attr_name } );
	my %type_info = %NODE_TYPES{$node.:node_type};
	if( $attr_name eq $ATTR_ID ) {
		return $node.set_node_id( $attr_value );
	}
	if( my $exp_node_types = $attr_name eq $ATTR_PP && %type_info{$TPI_PP_NREF} ) {
		return $node.:_set_primary_parent_attribute( $exp_node_types, $attr_value );
	}
	if( my $exp_lit_type = %type_info{$TPI_AT_LITERALS} && %type_info{$TPI_AT_LITERALS}.{$attr_name} ) {
		return $node.:_set_literal_attribute( $attr_name, $exp_lit_type, $attr_value );
	}
	if( my $exp_enum_type = %type_info{$TPI_AT_ENUMS} && %type_info{$TPI_AT_ENUMS}.{$attr_name} ) {
		return $node.:_set_enumerated_attribute( $attr_name, $exp_enum_type, $attr_value );
	}
	if( my $exp_node_types = %type_info{$TPI_AT_NREFS} && %type_info{$TPI_AT_NREFS}.{$attr_name} ) {
		return $node.:_set_node_ref_attribute( $attr_name, $exp_node_types, $attr_value );
	}
	$node.:_throw_error_message( 'SRT_N_SET_AT_INVAL_NM', { 'ATNM' => $attr_name } );
}

method set_attributes( $node: %attrs ) {
	%attrs.defined or $node.:_throw_error_message( 'SRT_N_SET_ATS_NO_ARGS' );
	unless( %attrs.meta.isa(Hash) ) {
		$node.:_throw_error_message( 'SRT_N_SET_ATS_BAD_ARGS', { 'ARG' => %attrs } );
	}
	my %type_info = %NODE_TYPES{$node.:node_type};
	for %attrs.kv -> $attr_name, $attr_value {
		$attr_value.defined or $node.:_throw_error_message( 'SRT_N_SET_ATS_NO_ARG_ELEM_VAL', { 'ATNM' => $attr_name } );
		if( $attr_name eq $ATTR_ID ) {
			$node.set_node_id( $attr_value );
			next;
		}
		if( my $exp_node_types = $attr_name eq $ATTR_PP && %type_info{$TPI_PP_NREF} ) {
			$node.:_set_primary_parent_attribute( $exp_node_types, $attr_value );
			next;
		}
		if( my $exp_lit_type = %type_info{$TPI_AT_LITERALS} && %type_info{$TPI_AT_LITERALS}.{$attr_name} ) {
			$node.:_set_literal_attribute( $attr_name, $exp_lit_type, $attr_value );
			next;
		}
		if( my $exp_enum_type = %type_info{$TPI_AT_ENUMS} && %type_info{$TPI_AT_ENUMS}.{$attr_name} ) {
			$node.:_set_enumerated_attribute( $attr_name, $exp_enum_type, $attr_value );
			next;
		}
		if( my $exp_node_types = %type_info{$TPI_AT_NREFS} && %type_info{$TPI_AT_NREFS}.{$attr_name} ) {
			$node.:_set_node_ref_attribute( $attr_name, $exp_node_types, $attr_value );
			next;
		}
		$node.:_throw_error_message( 'SRT_N_SET_ATS_INVAL_ELEM_NM', { 'ATNM' => $attr_name } );
	}
}

######################################################################

method get_container( $node: ) {
	return $node.:container;
}

method put_in_container( $node: SQL::Routine::Container $new_container ) {
	$new_container.defined or $node.:_throw_error_message( 'SRT_N_PI_CONT_NO_ARGS' );

	unless( $new_container.meta.isa(SQL::Routine::Container) ) {
		$node.:_throw_error_message( 'SRT_N_PI_CONT_BAD_ARG', { 'ARG' => $new_container } );
	}

	if( $node.:container ) {
		if( $new_container eq $node.:container ) {
			return; # no-op; new container same as old
		}
		$node.:_throw_error_message( 'SRT_N_PI_CONT_HAVE_ALREADY' );
	}
	my $node_type = $node.:node_type;

	my $node_id = $node.:node_id;
	unless( $node_id ) {
		if( $new_container.:auto_set_nids ) {
			$node_id = $node.:node_id = $new_container.get_next_free_node_id();
		} else {
			$node.:_throw_error_message( 'SRT_N_PI_CONT_NO_NODE_ID' );
		}
	}

	if( $new_container.:all_nodes.{$node_id} ) {
		$node.:_throw_error_message( 'SRT_N_PI_CONT_DUPL_ID' );
	}

	# Note: No recursion tests are necessary in put_in_container(); any existing Node 
	# that the newly added Node would link to can not already be the new Node's direct 
	# or indirect child, since Nodes in Containers can't reference Nodes that aren't.

	my $exp_pp_types = %NODE_TYPES{$node_type}.{$TPI_PP_NREF};
	my $exp_at_nrefs_types = %NODE_TYPES{$node_type}.{$TPI_AT_NREFS};
	my $rh_at_nodes_nids = $node.:at_nrefs; # all values should be node ids now
	my $rh_can = $new_container.:all_nodes;

	my $pp_node = $node.:pp_nref; # value should be id number
	if( my $pp_node_id = $pp_node ) {
		my $pp_node_ref = $rh_can.{$pp_node_id};
		unless( $pp_node_ref and grep:{ $pp_node_ref.:node_type eq $_ } @exp_pp_types ) {
			$node.:_throw_error_message( 'SRT_N_PI_CONT_NONEX_AT_NREF', 
				{ 'EXPNTYPE' => $exp_pp_types, 'EXPNID' => $pp_node_id } );
		}
		$pp_node = $pp_node_ref; # change id number to node ref
	}
	my %at_nodes_refs = (); # values put in here will be actual references
	for $rh_at_nodes_nids.keys -> $at_nodes_atnm {
		# Note that if $tpi_at_nodes is undefined, expect that this foreach loop will not run
		my $exp_at_nref_types = $exp_at_nrefs_types.{$at_nodes_atnm};
		my $at_nodes_nid = $rh_at_nodes_nids.{$at_nodes_atnm};
		my $at_nodes_ref = $rh_can.{$at_nodes_nid};
		unless( $at_nodes_ref and grep:{ $at_nodes_ref.:node_type eq $_ } @exp_at_nref_types ) {
			$node.:_throw_error_message( 'SRT_N_PI_CONT_NONEX_AT_NREF', 
				{ 'ATNM' => $at_nodes_atnm, 'EXPNTYPE' => $exp_at_nref_types, 'EXPNID' => $at_nodes_nid } );
		}
		$at_nodes_refs{$at_nodes_atnm} = $at_nodes_ref;
	}
	$node.:container = $new_container;
	$node.:pp_nref = $pp_node;
	$node.:at_nrefs = %at_nodes_refs;
	$rh_can.{$node_id} = $node;

	# Now get our parent Nodes to link back to us.
	if( my $pp_pseudonode = %NODE_TYPES{$node_type}.{$TPI_PP_PSEUDONODE} ) {
		push( @{$new_container.:pseudonodes.{$pp_pseudonode}}, $node );
	} elsif( my $pp_node = $node.:pp_nref ) {
		push( @{$pp_node.:prim_child_nrefs}, $node );
	}
	for $node.:at_nrefs.values -> $attr_value {
		push( @{$attr_value.:link_child_nrefs}, $node );
	}

	# Now adjust our "next free node id" counter if appropriate
	if( $node_id >= $node.:container.:next_free_nid ) {
		$node.:container.:next_free_nid = 1 + $node_id;
	}

	$new_container.:def_con_tested = 0; # A Node has become "Well Known".
}

method take_from_container( $node: ) {
	my $container = $node.:container or return; # no-op; node is already not in a container

	if( @{$node.:prim_child_nrefs} > 0 or @{$node.:link_child_nrefs} > 0 ) {
		$node.:_throw_error_message( 'SRT_N_TF_CONT_HAS_CHILD' );
	}

	# Remove our parent Nodes' links back to us.
	my $node_type = $node.:node_type;
	if( my $pp_pseudonode = %NODE_TYPES{$node_type}.{$TPI_PP_PSEUDONODE} ) {
		my $container = $node.:container;
		my $siblings = $container.:pseudonodes.{$pp_pseudonode};
		@siblings = grep:{ $_ ne $node } @siblings; # remove the occurance
	} elsif( my $pp_node = $node.:pp_nref ) {
		my $siblings = $pp_node.:prim_child_nrefs;
		@siblings = grep:{ $_ ne $node } @siblings; # remove the occurance
	}
	for $node.:at_nrefs.values -> $attr_value {
		my $siblings = $attr_value.:link_child_nrefs;
		@siblings = grep:{ $_ ne $node } @siblings; # remove all occurances
	}

	my $pp_node = $node.:pp_nref; # value should be actual ref
	if( $pp_node ) {
		$pp_node = $pp_node.:node_id; # change node ref to node id number
	}
	my $rh_at_nodes_refs = $node.:at_nrefs; # all values should be actual references now
	my %at_nodes_nids = (); # values put in here will be node id numbers
	for $rh_at_nodes_refs.keys -> $at_nodes_atnm {
		$at_nodes_nids{$at_nodes_atnm} = $rh_at_nodes_refs.{$at_nodes_atnm}.:node_id;
	}

	delete( $container.:all_nodes.{$node.:node_id} );
	$node.:at_nrefs = %at_nodes_nids;
	$node.:pp_nref = $pp_node;
	$node.:container = undef;

	$container.:def_con_tested = 0; # A "Well Known" Node is gone.
		# Turn on tests because this Node's absence affects *other* Well Known Nodes.
}

######################################################################

method move_before_sibling( $node: $sibling, $parent ) {
	my $pp_pseudonode = %NODE_TYPES{$node.:node_type}.{$TPI_PP_PSEUDONODE};

	# First make sure we have 3 actual Nodes that are all "Well Known" and in the same Container.

	$node.:container or $node.:_throw_error_message( 'SRT_N_MOVE_PRE_SIB_NO_CONT' );

	$sibling.defined or $node.:_throw_error_message( 'SRT_N_MOVE_PRE_SIB_NO_S_ARG' );
	unless( ref($sibling) eq ref($node) ) {
		$node.:_throw_error_message( 'SRT_N_MOVE_PRE_SIB_BAD_S_ARG', { 'ARG' => $sibling } );
	}
	unless( $sibling.:container and $sibling.:container eq $node.:container ) {
		$node.:_throw_error_message( 'SRT_N_MOVE_PRE_SIB_S_DIFF_CONT' );
	}

	if( $parent.defined ) {
		unless( ref($parent) eq ref($node) ) {
			$node.:_throw_error_message( 'SRT_N_MOVE_PRE_SIB_BAD_P_ARG', { 'ARG' => $parent } );
		}
		unless( $parent.:container and $parent.:container eq $node.:container ) {
			$node.:_throw_error_message( 'SRT_N_MOVE_PRE_SIB_P_DIFF_CONT' );
		}
	} else {
		unless( $parent = $node.:pp_nref ) {
			$pp_pseudonode or $node.:_throw_error_message( 'SRT_N_MOVE_PRE_SIB_NO_P_ARG_OR_PP_OR_PS' );
		}
	}

	# Now get the Node list we're going to search through.

	my $ra_search_list = $parent ?? 
		($parent eq $node.:pp_nref ?? $parent.:prim_child_nrefs :: $parent.:link_child_nrefs) :: 
		$node.:container.:pseudonodes.{$pp_pseudonode};

	# Now confirm the given Nodes are our parent and sibling.
	# For efficiency we also prepare to reorder the Nodes at the same time.

	my @curr_node_refs = ();
	my @sib_node_refs = ();
	my @refs_before_both = ();
	my @refs_after_both = ();

	my $others_go_before = 1;
	for @ra_search_list -> $child {
		if( $child eq $node ) {
			push( @curr_node_refs, $child );
		} elsif( $child eq $sibling ) {
			push( @sib_node_refs, $child );
			$others_go_before = 0;
		} elsif( $others_go_before ) {
			push( @refs_before_both, $child );
		} else {
			push( @refs_after_both, $child );
		}
	}

	scalar( @curr_node_refs ) or $node.:_throw_error_message( 'SRT_N_MOVE_PRE_SIB_P_NOT_P' );
	scalar( @sib_node_refs ) or $node.:_throw_error_message( 'SRT_N_MOVE_PRE_SIB_S_NOT_S' );

	# Everything checks out, so now we perform the reordering.

	@ra_search_list = (@refs_before_both, @curr_node_refs, @sib_node_refs, @refs_after_both);
	$node.:container.:def_con_tested = 0; # "Well Known" Node relation chg.
}

######################################################################

method get_child_nodes( $node: Str ?$node_type ) {
	if( $node_type.defined ) {
		unless( %NODE_TYPES{$node_type} ) {
			$node.:_throw_error_message( 'SRT_N_GET_CH_NODES_BAD_TYPE' );
		}
		return [grep:{ $_.:node_type eq $node_type } @{$node.:prim_child_nrefs}];
	} else {
		return [@{$node.:prim_child_nrefs}];
	}
}

method add_child_node( $node: $new_child ) {
	$new_child.defined or $node.:_throw_error_message( 'SRT_N_ADD_CH_NODE_NO_ARGS' );
	unless( ref($new_child) eq ref($node) ) {
		$node.:_throw_error_message( 'SRT_N_ADD_CH_NODE_BAD_ARG', { 'ARG' => $new_child } );
	}
	$new_child.set_primary_parent_attribute( $node ); # will throw if not same Container
		# will also throw if the change would result in a circular reference
}

method add_child_nodes( $node: @list ) {
	for @list -> $element {
		$node.add_child_node( $element );
	}
}

######################################################################

method get_referencing_nodes( $node: Str $node_type ) {
	if( $node_type.defined ) {
		unless( %NODE_TYPES{$node_type} ) {
			$node.:_throw_error_message( 'SRT_N_GET_REF_NODES_BAD_TYPE' );
		}
		return [grep:{ $_.:node_type eq $node_type } @{$node.:link_child_nrefs}];
	} else {
		return [@{$node.:link_child_nrefs}];
	}
}

######################################################################

method get_surrogate_id_chain( $node: ) {
	$node.:container or $node.:_throw_error_message( 'SRT_N_GET_SID_CHAIN_NOT_IN_CONT' );
	return $node.:_get_surrogate_id_chain();
}

method :_get_surrogate_id_chain( $node: ) {
	my $si_atvl = $node.get_surrogate_id_attribute( 1 ); # target SI lit/enum is being returned as a string
	if( my $pp_node = $node.:pp_nref ) {
		# Current Node has a primary-parent Node; append to its id chain.
		my $elements = $pp_node.:_get_surrogate_id_chain();
		push( @elements, $si_atvl );
		return $elements;
	} elsif( my $l2_psnd = %NODE_TYPES{$node.:node_type}.{$TPI_PP_PSEUDONODE} ) {
		# Current Node has a primary-parent pseudo-Node; append to its id chain.
		return [undef, $SQLRT_L1_ROOT_PSND, $l2_psnd, $si_atvl];
	} else {
		# Current Node is not linked to the main Node tree yet; indicate this with non-undef first chain element.
		return [$si_atvl];
	}
}

######################################################################

method find_node_by_surrogate_id( $node: Str $self_attr_name, @target_attr_value ) {
	$node.:container or $node.:_throw_error_message( 'SRT_N_FIND_ND_BY_SID_NOT_IN_CONT' );
	$self_attr_name.defined or $node.:_throw_error_message( 'SRT_N_FIND_ND_BY_SID_NO_ARGS' );
	my %type_info = %NODE_TYPES{$node.:node_type};
	my $exp_node_types = %type_info{$TPI_AT_NREFS} && %type_info{$TPI_AT_NREFS}.{$self_attr_name} or 
		$node.:_throw_error_message( 'SRT_N_FIND_ND_BY_SID_INVAL_NM', { 'ATNM' => $self_attr_name } );
	@target_attr_value.defined or $node.:_throw_error_message( 
		'SRT_N_FIND_ND_BY_SID_NO_ARG_VAL', { 'ATNM' => $self_attr_name } );
	scalar( @target_attr_value ) >= 1 or $node.:_throw_error_message( 
		'SRT_N_FIND_ND_BY_SID_NO_ARG_VAL', { 'ATNM' => $self_attr_name } );
	for @target_attr_value -> $element {
		$element.defined or $node.:_throw_error_message( 
			'SRT_N_FIND_ND_BY_SID_NO_ARG_VAL', { 'ATNM' => $self_attr_name } );
	}
	# If we get here, most input checks passed.
	my %exp_p_node_types = @exp_node_types.map:{ ($_ => 1) };
	if( my $search_path = %type_info{$TPI_ANCES_ATCORS} && %type_info{$TPI_ANCES_ATCORS}.{$self_attr_name} ) {
		# The value we are searching for must be the child part of a correlated pair.
		return $node.:_find_node_by_surrogate_id_using_path( %exp_p_node_types, @target_attr_value, $search_path );
	}
	# If we get here, the value we are searching for is not the child part of a correlated pair.
	my %remotely_addressable_types = @exp_node_types.
		grep:{ %NODE_TYPES{$_}.{$TPI_REMOTE_ADDR} }.map:{ ($_ => %NODE_TYPES{$_}.{$TPI_REMOTE_ADDR}) };
	my ($unqualified_value, $qualifier_l1, @rest) = @target_attr_value;
	if( $qualifier_l1 ) {
		# An attempt is definitely being made to remotely address a Node.
		scalar( %remotely_addressable_types.keys ) >= 1 or $node.:_throw_error_message( 
			'SRT_N_FIND_ND_BY_SID_NO_REM_ADDR', { 'ATNM' => $self_attr_name, 'ATVL' => @target_attr_value } );
		# If we get here, we are allowed to remotely address a Node.
		return $node.:_find_node_by_surrogate_id_remotely( %remotely_addressable_types, @target_attr_value );
	}
	# If we get here, we are searching with a purely unqualified target SI value.
	# First try to find the target among our ancestors' siblings.
	if( my $result = $node.:_find_node_by_surrogate_id_within_layers( %exp_p_node_types, $unqualified_value ) ) {
		return $result;
	}
	# If we get here, there were no ancestor sibling matches.
	if( scalar( %remotely_addressable_types.keys ) >= 1 ) {
		# If we get here, we are allowed to remotely address a Node.
		return $node.:_find_node_by_surrogate_id_remotely( %remotely_addressable_types, @target_attr_value );
	}
	# If we get here, all search attempts failed.
	return;
}

method :_find_node_by_surrogate_id_remotely( $node: %exp_p_node_types, @target_attr_value ) {
	# Method assumes %exp_p_node_types only contains Node types that can be remotely addressable.
	# Within this method, values of %exp_p_node_types are arrays of expected ancestor types for the keys.
	my @search_chain = reverse @target_attr_value;
	my %exp_anc_node_types = %exp_p_node_types.values.map:{ $_[] }.map:{ ($_ => 1) };
	# First check if we, ourselves, are a child of an expected ancestor type; 
	# if we are, then we should search beneath our own ancestor first.
	my $self_anc_node = $node;
	while( $self_anc_node and !$exp_anc_node_types{$self_anc_node.:node_type} ) {
		$self_anc_node = $self_anc_node.:pp_nref;
	}
	if( $self_anc_node ) {
		# Search beneath our own ancestor first.
		my $curr_node = $node;
		do {
			# $curr_node is everything from our parent to and including the remote ancestor.
			$curr_node = $curr_node.:pp_nref;
			if( my $result = $curr_node.:_find_node_by_surrogate_id_remotely_below_here( %exp_p_node_types, @search_chain ) ) {
				return $result;
			}
		} until( $curr_node eq $self_anc_node );
	}
	# If we get here, we either have no qualified ancestor, or nothing was found when starting beneath it.
	# Now look beneath other allowable ancestors.
	my %psn_roots = %exp_anc_node_types.keys.map:{ %NODE_TYPES{$_}.{$TPI_PP_PSEUDONODE} }.grep:{ $_ }.map:{ ($_ => 1) };
	my $pseudonodes = $node.:container.:pseudonodes;
	my @anc_nodes = @L2_PSEUDONODE_LIST.grep:{ $psn_roots{$_} }.map:{ @{$pseudonodes.{$_}} }.grep:{ $exp_anc_node_types{$_.:node_type} };
	my @matched_node_list = ();
	for @anc_nodes -> $anc_node {
		if( my $result = $anc_node.:_find_node_by_surrogate_id_remotely_below_here( %exp_p_node_types, @search_chain ) ) {
			push( @matched_node_list, @result );
		}
	}
	return @matched_node_list == 0 ?? undef :: @matched_node_list;
}

method :_find_node_by_surrogate_id_remotely_below_here( $node: %exp_p_node_types, $search_chain_in ) {
	# Method assumes %exp_p_node_types only contains Node types that can be remotely addressable.
	my @search_chain = @search_chain_in or return; # search chain empty; no match possible
	my $si_atvl = $node.get_surrogate_id_attribute( 1 ) or return;
	if( %exp_p_node_types.{$node.:node_type} ) {
		# It is illegal to remotely match a Node that is a child of a remotely matcheable type.
		# Therefore, the invocant Node must be the end of the line, win or lose; its children can not be searched.
		if( @search_chain == 1 and $si_atvl eq $search_chain[0] ) {
			# We have a single perfectly matching Node along this path.
			return [$node];
		} else {
			# No match, and we can't go further anyway.
			return; 
		}
	}
	# If we get here, the invocant Node can not be returned regardless of its name; proceed to its children.
	if( @search_chain > 1 and $si_atvl eq $search_chain[0] ) {
		# There are at least 2 chain elements left, so the invocant Node may match the first one.
		shift( @search_chain );
	}
	# If we get here, there is at least 1 more unmatched search chain element.
	my @matched_node_list = ();
	for $node.:prim_child_nrefs -> $child {
		if( my $result = $child.:_find_node_by_surrogate_id_remotely_below_here( %exp_p_node_types, @search_chain ) ) {
			push( @matched_node_list, @result );
		}
	}
	return @matched_node_list == 0 ?? undef :: @matched_node_list;
}

method :_find_node_by_surrogate_id_within_layers( $node: %exp_p_node_types, @target_attr_value ) {
	my $pp_node = $node.:pp_nref;

	# Now determine who our siblings are.

	my @sibling_list = ();
	if( $pp_node ) {
		# We have a normal Node primary-parent, P.
		# Search among all Nodes that have P as their primary-parent Node; these are our siblings.
		@sibling_list = @{$pp_node.:prim_child_nrefs};
	} else {
		# Either we have a pseudo-Node primary-parent, or no parent normal Node is defined for us.
		# Search among all Nodes that have pseudo-Node primary-parents.
		my $pseudonodes = $node.:container.:pseudonodes;
		@sibling_list = @L2_PSEUDONODE_LIST.map:{ @{$pseudonodes.{$_}} };
	}

	# Now search among our siblings for a match.

	for @sibling_list -> $sibling_node {
		%exp_p_node_types.{$sibling_node.:node_type} or next;
		if( my $si_atvl = $sibling_node.get_surrogate_id_attribute( 1 ) ) {
			if( $si_atvl eq @target_attr_value ) {
				return [$sibling_node];
			}
		}
	}

	# Nothing was found among our siblings.

	if( $pp_node ) {
		# We are not at the tree's root, so move upwards by a generation and try again.
		return $pp_node.:_find_node_by_surrogate_id_within_layers( %exp_p_node_types, @target_attr_value );
	} else {
		# There is no further up that we can go, so no match was found.
		return;
	}
}

method :_find_node_by_surrogate_id_using_path( $node: %exp_p_node_types, @target_attr_value, $search_path ) {
	my $curr_node = $node;
	my ($unqualified_value, $qualifier_l1, @rest) = @target_attr_value;

	# Now enumerate through the explicit search path elements, updating $curr_node in the process.

	for @search_path -> $path_seg {
		if( $path_seg eq $S ) { # <self> is a no-op, existing for easier-to-read documentation only
			# no-op
		} elsif( $path_seg eq $P ) { # <primary-parent>
			unless( $curr_node = $curr_node.:pp_nref ) {
				return; # current Node's primary parent isn't set yet (it should be); get out
			}
		} elsif( $path_seg eq $R ) { # <root-of-kind>
			while( $curr_node.:pp_nref and 
					$curr_node.:pp_nref.:node_type eq $curr_node.:node_type ) {
				$curr_node = $curr_node.:pp_nref;
			}
		} elsif( $path_seg eq $C ) { # <primary-child>
			# For simplicity we are assuming the $C is the end of the path; it's grand-child or bust.
			if( $qualifier_l1.defined ) {
				# Given value is qualified; only look within the specified contexts.
				for $curr_node.:prim_child_nrefs -> $child_l1 {
					my $si_atvl = $child_l1.get_surrogate_id_attribute( 1 ) or next;
					$si_atvl eq $qualifier_l1 or next;
					for $child_l1.:prim_child_nrefs -> $child_l2 {
						%exp_p_node_types.{$child_l2.:node_type} or next;
						if( my $si_atvl = $child_l2.get_surrogate_id_attribute( 1 ) ) {
							if( $si_atvl eq $unqualified_value ) {
								return [$child_l2];
							}
						}
					}
				}
			} else { 
				# Given value is unqualified; take any ones that match.
				my @matched_node_list = ();
				for $curr_node.:prim_child_nrefs.map:{ $_.:prim_child_nrefs[] } -> $grandchild {
					%exp_p_node_types.{$grandchild.:node_type} or next;
					if( my $si_atvl = $grandchild.get_surrogate_id_attribute( 1 ) ) {
						if( $si_atvl eq $unqualified_value ) {
							push( @matched_node_list, $grandchild );
						}
					}
				}
				return @matched_node_list == 0 ?? undef :: @matched_node_list;
			}
			return;
		} else { # <nref-attr>; $path_seg is an attribute name
			unless( $curr_node = $curr_node.:at_nrefs.{$path_seg} ) {
				return; # the Node-ref attribute we should follow isn't set yet (it should be); get out
			}
		}
	}

	# We are at the end of the explicit search path, and the start of the implicit path.
	# Now enumerate through any wrapper attributes, if there are any, updating $curr_node in the process.

	while( my $wr_atnm = %NODE_TYPES{$curr_node.:node_type}.{$TPI_WR_ATNM} ) {
		unless( $curr_node = $curr_node.:at_nrefs.{$wr_atnm} ) {
			return; # the Node-ref attribute we should follow isn't set yet (it should be); get out
		}
	}

	# We are at the end of the implicit search path.
	# The required Node must be one of $curr_node's children.

	for $curr_node.:prim_child_nrefs -> $child {
		%exp_p_node_types.{$child.:node_type} or next;
		if( my $si_atvl = $child.get_surrogate_id_attribute( 1 ) ) {
			if( $si_atvl eq $unqualified_value ) {
				return [$child];
			}
		}
	}

	# Nothing was found, nothing more to search.

	return;
}

######################################################################

method find_child_node_by_surrogate_id( $node: @target_attr_value ) {
	$node.:container or $node.:_throw_error_message( 'SRT_N_FIND_CH_ND_BY_SID_NOT_IN_CONT' );
	@target_attr_value.defined or $node.:_throw_error_message( 'SRT_N_FIND_CH_ND_BY_SID_NO_ARG_VAL' );
	if( @target_attr_value.[0].defined ) {
		# The given surrogate id chain is relative to the current Node.
		my $curr_node = $node;
		ELEM: for @target_attr_value -> $chain_element {
			for $curr_node.:prim_child_nrefs -> $child {
				if( my $si_atvl = $child.get_surrogate_id_attribute( 1 ) ) {
					if( $si_atvl eq $chain_element ) {
						$curr_node = $child;
						next ELEM;
					}
				}
			}
			return;
		}
		return $curr_node;
	} else {
		# The given surrogate id chain starts at the root of the current Node's Container.
		return $node.:container.find_child_node_by_surrogate_id( @target_attr_value );
	}
}

######################################################################

method get_relative_surrogate_id( $node: Str $self_attr_name, Bool ?$want_shortest ) {
	$node.:container or $node.:_throw_error_message( 'SRT_N_GET_REL_SID_NOT_IN_CONT' );
	$self_attr_name.defined or $node.:_throw_error_message( 'SRT_N_GET_REL_SID_NO_ARGS' );
	my %type_info = %NODE_TYPES{$node.:node_type};
	my $exp_node_types = %type_info{$TPI_AT_NREFS} && %type_info{$TPI_AT_NREFS}.{$self_attr_name} or 
		$node.:_throw_error_message( 'SRT_N_GET_REL_SID_INVAL_NM', { 'ATNM' => $self_attr_name } );
	my $attr_value_node = $node.:at_nrefs.{$self_attr_name} or return;
	my $attr_value_si_atvl = $attr_value_node.get_surrogate_id_attribute( 1 );
	if( my $search_path = %type_info{$TPI_ANCES_ATCORS} && %type_info{$TPI_ANCES_ATCORS}.{$self_attr_name} ) {
		# The value we are outputting is the child part of a correlated pair.
		if( $search_path.[-1] eq $C ) {
			# For simplicity, assume only one $C, which is at the end of the search path, as find_*() does.
			my $p_of_attr_value_node = $attr_value_node.:pp_nref or return; # linked Node not in tree
			my $p_of_attr_value_si_atvl = $p_of_attr_value_node.get_surrogate_id_attribute( 1 );
			# Now we have the info we need.  However, we may optionally abbreviate output further.
			if( $want_shortest ) {
				# We want to further abbreviate output, check if $attr_value_si_atvl distinct by itself.
				my $p_of_p_of_attr_value_node = $p_of_attr_value_node.:pp_nref or return; # linked Node not in tree
				for $p_of_p_of_attr_value_node.:prim_child_nrefs.map:{ $_.:prim_child_nrefs[] } -> $ch_node {
					if( my $ch_si_atvl = $ch_node.get_surrogate_id_attribute( 1 ) ) {
						if( $ch_si_atvl eq $attr_value_si_atvl and $ch_node ne $attr_value_node ) {
							# The target Node has a cousin Node that has the same surrogate id, so we must qualify ours.
							return [$attr_value_si_atvl, $p_of_attr_value_si_atvl];
						}
					}
				}
				# If we get here, there is no cousin with the same surrogate id, so an unqualified one is okay here.
				return $attr_value_si_atvl;
			} else {
				# We do not want to further abbreviate output, so return fully qualified version.
				return [$attr_value_si_atvl, $p_of_attr_value_si_atvl];
			}
		} else {
			# There is a correlated search path, and it does not have a $C.
			return $attr_value_si_atvl;
		}
	}
	# If we get here, the value we are outputting is not the child part of a correlated pair.
	my %exp_p_node_types = map:{ ($_ => 1) } @exp_node_types;
	my $layered_search_results = $node.:_find_node_by_surrogate_id_within_layers( %exp_p_node_types, $attr_value_si_atvl );
	if( $layered_search_results and $layered_search_results.[0] eq $attr_value_node ) {
		return $attr_value_si_atvl;
	}
	# If we get here, the value we are outputting is not an ancestor's sibling.
	my %remotely_addressable_types = map:{ ($_ => %NODE_TYPES{$_}.{$TPI_REMOTE_ADDR}) } 
		grep:{ %NODE_TYPES{$_}.{$TPI_REMOTE_ADDR} } @exp_node_types;
	scalar( %remotely_addressable_types.keys ) >= 1 or return; # Can't remotely address, so give up.
	# If we get here, we are allowed to remotely address a Node.
	# Now make sure attr-val Node has an ancestor of the expected type.
	my @attr_value_si_chain = ();
	my %exp_anc_node_types = map:{ ($_ => 1) } map:{ @_ } %remotely_addressable_types.values;
	my $attr_value_anc_node = $attr_value_node;
	while( $attr_value_anc_node and !$exp_anc_node_types{$attr_value_anc_node.:node_type} ) {
		my $anc_si_atvl = $attr_value_anc_node.get_surrogate_id_attribute( 1 ) or return; # part of SI not defined yet
		push( @attr_value_si_chain, $anc_si_atvl );
		$attr_value_anc_node = $attr_value_anc_node.:pp_nref;
	}
	$attr_value_anc_node or return; # attr-val does not have expected ancestor
	my $anc_si_atvl = $attr_value_anc_node.get_surrogate_id_attribute( 1 ) or return; # part of SI not defined yet
	push( @attr_value_si_chain, $anc_si_atvl ); # push required ancestor itself
	my @remote_search_results = $node.:_find_node_by_surrogate_id_remotely( %remotely_addressable_types, @attr_value_si_chain );
	@remote_search_results and @remote_search_results.[0] eq $attr_value_node or return; # can't find ourself, oops
	# If we get here, the fully-qualified form of the attr-value can be remotely addressed successfully.
	if( $want_shortest ) {
		# We want to further abbreviate output.
		my ($unqualified, $l2, $l3) = @attr_value_si_chain; # for simplicity, assume no more than 3 levels
		$l2 or return $unqualified; # fully qualified version is only 1 element long
		if( @{$node.:_find_node_by_surrogate_id_remotely( %remotely_addressable_types, [$unqualified] )} == 1 ) {
			# Fully unqualified version returns only one result, so it is currently unambiguous.
			return $unqualified; # 1 element
		}
		$l3 or return @attr_value_si_chain; # fully qualified version is only 2 elements long
		if( @{$node.:_find_node_by_surrogate_id_remotely( %remotely_addressable_types, [$unqualified, $l2] )} == 1 ) {
			# This partially qualified version returns only one result, so it is currently unambiguous.
			return [$unqualified, $l2]; # 2 elements
		}
		if( @{$node.:_find_node_by_surrogate_id_remotely( %remotely_addressable_types, [$unqualified, $l3] )} == 1 ) {
			# This partially qualified version returns only one result, so it is currently unambiguous.
			return [$unqualified, $l3]; # 2 elements
		}
		# If we get here, all shortened versions return multiple results, so return fully qualified version.
		return @attr_value_si_chain; # 3 elements
	} else {
		# We do not want to further abbreviate output, so return fully qualified version.
		return @attr_value_si_chain;
	}
}

######################################################################

method assert_deferrable_constraints( $node: ) {
	# Only "Well Known" Nodes would get this invoked by Container.assert_deferrable_constraints().
	# "Alone" Nodes only get here when Node.assert_deferrable_constraints() 
	# is invoked directly by external code.
	$node.:_assert_in_node_deferrable_constraints(); # can call on Alone, Well Known
	if( $node.:container ) {
		$node.:_assert_parent_ref_scope_deferrable_constraints(); # call on Well Known only
		$node.:_assert_child_comp_deferrable_constraints(); # call on Well Known only
	}
}

method :_assert_in_node_deferrable_constraints( $node: ) {
	# All assertions that can be performed on Nodes of all statuses are done in this method.
	my %type_info = %NODE_TYPES{$node.:node_type};

	# 1: Now assert constraints associated with Node-type details given in each 
	# "Attribute List" section of Language.pod.

	# 1.1: Assert that the NODE_ID attribute is set.
	unless( $node.:node_id.defined ) {
		# This can only possibly fail at deferrable-constraint assertion time with "Alone" Nodes; 
		# it is always-enforced for "Well Known" Nodes.
		$node.:_throw_error_message( 'SRT_N_ASDC_NID_VAL_NO_SET' );
	}

	# 1.2: Assert that any primary parent ("PP") attribute is set.
	unless( $node.:pp_nref.defined or %type_info{$TPI_PP_PSEUDONODE} ) {
		$node.:_throw_error_message( 'SRT_N_ASDC_PP_VAL_NO_SET' );
	}

	# 1.3: Assert that any surrogate id ("SI") attribute is set.
	if( my $si_atnm = %type_info{$TPI_SI_ATNM} ) {
		my (undef, $lit, $enum, $nref) = @si_atnm;
		# Skip 'id', as that's redundant with test 1.1.
		if( $lit ) {
			unless( $node.:at_literals.{$lit}.defined ) {
				$node.:_throw_error_message( 'SRT_N_ASDC_SI_VAL_NO_SET', { 'ATNM' => $lit } );
			}
		}
		if( $enum ) {
			unless( $node.:at_enums.{$enum}.defined ) {
				$node.:_throw_error_message( 'SRT_N_ASDC_SI_VAL_NO_SET', { 'ATNM' => $enum } );
			}
		}
		if( $nref ) {
			unless( $node.:at_nrefs.{$nref}.defined ) {
				$node.:_throw_error_message( 'SRT_N_ASDC_SI_VAL_NO_SET', { 'ATNM' => $nref } );
			}
		}
	}

	# 1.4: Assert that any always-mandatory ("MA") attributes are set.
	if( my $mand_attrs = %type_info{$TPI_MA_ATNMS} ) {
		my (@lits, @enums, @nrefs) = @mand_attrs;
		for @lits -> $attr_name {
			unless( $node.:at_literals.{$attr_name}.defined ) {
				$node.:_throw_error_message( 'SRT_N_ASDC_MA_VAL_NO_SET', { 'ATNM' => $attr_name } );
			}
		}
		for @enums -> $attr_name {
			unless( $node.:at_enums.{$attr_name}.defined ) {
				$node.:_throw_error_message( 'SRT_N_ASDC_MA_VAL_NO_SET', { 'ATNM' => $attr_name } );
			}
		}
		for @nrefs -> $attr_name {
			unless( $node.:at_nrefs.{$attr_name}.defined ) {
				$node.:_throw_error_message( 'SRT_N_ASDC_MA_VAL_NO_SET', { 'ATNM' => $attr_name } );
			}
		}
	}

	# 2: Now assert constraints associated with Node-type details given in each 
	# "Exclusive Attribute Groups List" section of Language.pod.

	if( my @mutex_atgps = %type_info{$TPI_MUTEX_ATGPS} ) {
		for @mutex_atgps -> @mutex_atgp {
			my ($mutex_name, $lits, $enums, $nrefs, $is_mandatory) = @mutex_atgp;
			my @valued_candidates = ();
			for @lits -> $attr_name {
				if( $node.:at_literals.{$attr_name}.defined ) {
					push( @valued_candidates, $attr_name );
				}
			}
			for @enums -> $attr_name {
				if( $node.:at_enums.{$attr_name}.defined ) {
					push( @valued_candidates, $attr_name );
				}
			}
			for @nrefs -> $attr_name {
				if( $node.:at_nrefs.{$attr_name}.defined ) {
					push( @valued_candidates, $attr_name );
				}
			}
			if( scalar( @valued_candidates ) > 1 ) {
				$node.:_throw_error_message( 'SRT_N_ASDC_MUTEX_TOO_MANY_SET', 
					{ 'NUMVALS' => scalar( @valued_candidates ), 
					'ATNMS' => "@valued_candidates", 'MUTEX' => $mutex_name } );
			}
			if( scalar( @valued_candidates ) == 0 ) {
				if( $is_mandatory ) {
					my @possible_candidates = (@lits, @enums, @nrefs);
					$node.:_throw_error_message( 'SRT_N_ASDC_MUTEX_ZERO_SET', 
						{ 'ATNMS' => "@possible_candidates", 'MUTEX' => $mutex_name } );
				}
			}
		}
	}

	# 3: Now assert constraints associated with Node-type details given in each 
	# "Local Attribute Dependencies List" section of Language.pod.

	if( my $local_atdps_list = %type_info{$TPI_LOCAL_ATDPS} ) {
		for @local_atdps_list -> $local_atdps_item {
			my ($dep_on_lit_nm, $dep_on_enum_nm, $dep_on_nref_nm, $dependencies) = @local_atdps_item;
			my $dep_on_attr_nm = $dep_on_lit_nm || $dep_on_enum_nm || $dep_on_nref_nm;
			my $dep_on_attr_val = $dep_on_lit_nm ?? $node.:at_literals.{$dep_on_lit_nm} ::
				$dep_on_enum_nm ?? $node.:at_enums.{$dep_on_enum_nm} ::
				$dep_on_nref_nm ?? $node.:at_nrefs.{$dep_on_nref_nm} :: undef;
			for @dependencies -> $dependency {
				my (@lits, @enums, @nrefs, $dep_on_enum_vals, $is_mandatory) = @dependency;
				my @valued_dependents = ();
				for @lits -> $attr_name {
					if( $node.:at_literals.{$attr_name}.defined ) {
						push( @valued_dependents, $attr_name );
					}
				}
				for @enums -> $attr_name {
					if( $node.:at_enums.{$attr_name}.defined ) {
						push( @valued_dependents, $attr_name );
					}
				}
				for @nrefs -> $attr_name {
					if( $node.:at_nrefs.{$attr_name}.defined ) {
						push( @valued_dependents, $attr_name );
					}
				}
				if( !$dep_on_attr_val.defined ) {
					# The dependency is undef/null, so all dependents must be undef/null.
					if( scalar( @valued_dependents ) > 0 ) {
						$node.:_throw_error_message( 'SRT_N_ASDC_LATDP_DEP_ON_IS_NULL', 
							{ 'DEP_ON' => $dep_on_attr_nm, 'NUMVALS' => scalar( @valued_dependents ), 
							'ATNMS' => "@valued_dependents" } );
					}
					# If we get here, the tests have passed concerning this $dependency.
				} elsif( scalar( @dep_on_enum_vals ) > 0 and 
						!scalar( grep:{ $_ eq $dep_on_attr_val } @dep_on_enum_vals ) ) {
					# Not just any dependency value is acceptable for these dependents, and the
					# dependency has the wrong value for these dependents; the latter must be undef/null.
					if( scalar( @valued_dependents ) > 0 ) {
						$node.:_throw_error_message( 'SRT_N_ASDC_LATDP_DEP_ON_HAS_WRONG_VAL', 
							{ 'DEP_ON' => $dep_on_attr_nm, 'DEP_ON_VAL' => $dep_on_attr_val, 
							'NUMVALS' => scalar( @valued_dependents ), 'ATNMS' => "@valued_dependents" } );
					}
					# If we get here, the tests have passed concerning this $dependency.
				} else {
					# Either any dependency value is acceptable for these dependents, or the valued 
					# dependency has the right value for these dependents; one of them may be set.
					if( scalar( @valued_dependents ) > 1 ) {
						$node.:_throw_error_message( 'SRT_N_ASDC_LATDP_TOO_MANY_SET', 
							{ 'DEP_ON' => $dep_on_attr_nm, 'DEP_ON_VAL' => $dep_on_attr_val, 
							'NUMVALS' => scalar( @valued_dependents ), 'ATNMS' => "@valued_dependents" } );
					}
					if( scalar( @valued_dependents ) == 0 ) {
						if( $is_mandatory ) {
							my @possible_candidates = (@lits, @enums, @nrefs);
							$node.:_throw_error_message( 'SRT_N_ASDC_LATDP_ZERO_SET', 
								{ 'DEP_ON' => $dep_on_attr_nm, 'DEP_ON_VAL' => $dep_on_attr_val, 
								'ATNMS' => "@possible_candidates" } );
						}
					}
					# If we get here, the tests have passed concerning this $dependency.
				}
			}
		}
	}

	# This is the end of the tests that can be performed on "Alone" Nodes.
}

method :_assert_parent_ref_scope_deferrable_constraints( $node: ) {
	# Assertions in this method can only be performed on Nodes in "Well Known" status.
	my %type_info = %NODE_TYPES{$node.:node_type};

	# Assert that all non-PP Node-ref attributes of the current Node point to Nodes that 
	# are actually within the visible scope of the current Node.

	if( my %at_nrefs = %type_info{$TPI_AT_NREFS} ) {
		for %at_nrefs.keys -> $attr_name {
			my $given_p_node = $node.:at_nrefs.{$attr_name};
			if( $given_p_node.defined ) {
				my $given_p_node_si = $node.get_relative_surrogate_id( $attr_name );
				my $fetched_p_nodes = $node.find_node_by_surrogate_id( $attr_name, $given_p_node_si );
				unless( $fetched_p_nodes and $fetched_p_nodes.[0] eq $given_p_node ) {
					my $given_p_node_type = $given_p_node.:node_type;
					my $given_p_node_id = $given_p_node.:node_id;
					my $given_p_node_sidch = $given_p_node.get_surrogate_id_chain();
					$node.:_throw_error_message( 'SRT_N_ASDC_NREF_AT_NONEX_SID', 
						{ 'ATNM' => $attr_name, 'EXPNTYPES' => %at_nrefs.{$attr_name}, 
						'PNTYPE' => $given_p_node_type, 'PNID' => $given_p_node_id, 
						'PSIDCH' => $given_p_node_sidch, 'PSID' => $given_p_node_si, } );
				}
			}
		}
	}

	# This is the end of the searching tests.
}

method :_assert_child_comp_deferrable_constraints( $node_or_class: Str $pseudonode_name, SQL::Routine::Container $container ) {
	# Assertions in this method can only be performed on Nodes in "Well Known" status.
	my %type_info = ref($node_or_class) ?? 
		%NODE_TYPES{$node_or_class.:node_type} :: 
		%PSEUDONODE_TYPES{$pseudonode_name};

	# First, gather a child list.

	my @parent_node_types = ();
	my @child_nodes = ();
	if( ref($node_or_class) ) {
		my @parent_nodes = ($node_or_class);
		my $curr_node = $node_or_class;
		while( my $wr_atnm = %NODE_TYPES{$curr_node.:node_type}.{$TPI_WR_ATNM} ) {
			if( $curr_node = $curr_node.:at_nrefs.{$wr_atnm} ) {
				unshift( @parent_nodes, $curr_node );
			} else {
				last; # avoid undef warnings in while expr due to unset $curr_node
			}
		}
		for @parent_nodes -> $parent_node {
			push( @parent_node_types, $parent_node.:node_type );
			push( @child_nodes, @{$parent_node.:prim_child_nrefs} );
		}
	} else {
		push( @parent_node_types, $pseudonode_name );
		@child_nodes = @{$container.:pseudonodes.{$pseudonode_name}};
	}

	# 1: Now assert that the surrogate id (SI) of each child Node is distinct.

	my %type_child_si = map:{ (%{%TYPE_CHILD_SI_ATNMS{$_}||{}}) } @parent_node_types;
	# Note: %TYPE_CHILD_SI_ATNMS only contains keys for [pseudo-|]Node types that can have primary-child Nodes.
	if( scalar( %type_child_si.keys ) ) {
		my %examined_children = ();
		for @child_nodes -> $child_node {
			my $child_node_type = $child_node.:node_type;
			if( my $si_atnm = $type_child_si{$child_node_type} ) {
				my (undef, $lit, $enum, $nref) = @si_atnm;
				my $hash_key = 
					$lit ?? $child_node.:at_literals.{$lit} :: 
					$enum ?? $child_node.:at_enums.{$enum} :: 
					$nref ?? $child_node.:at_nrefs.{$nref} :: 
					$child_node.:node_id;
				$hash_key.defined or next; # An error, but let a different test flag it.
				ref($hash_key) and next; # some comps by target lit/enum are known to be false errors; todo, see if any legit errors
				if( %examined_children{$hash_key}.exists ) {
					# Multiple Nodes have the same primary-parent and surrogate id.
					my $child_node_id = $child_node.:node_id;
					my $matched_child_node = %examined_children{$hash_key};
					my $matched_child_node_type = $matched_child_node.:node_type;
					my $matched_child_node_id = $matched_child_node.:node_id;
					if( ref($node_or_class) ) {
						$node_or_class.:_throw_error_message( 'SRT_N_ASDC_SI_NON_DISTINCT', 
							{ 'VALUE' => $hash_key, 
							'C1NTYPE' => $child_node_type, 'C1NID' => $child_node_id, 
							'C2NTYPE' => $matched_child_node_type, 'C2NID' => $matched_child_node_id } );
					} else {
						$node_or_class.:_throw_error_message( 'SRT_N_ASDC_SI_NON_DISTINCT_PSN', 
							{ 'PSNTYPE' => $pseudonode_name, 'VALUE' => $hash_key, 
							'C1NTYPE' => $child_node_type, 'C1NID' => $child_node_id, 
							'C2NTYPE' => $matched_child_node_type, 'C2NID' => $matched_child_node_id } );
					}
				}
				%examined_children{$hash_key} = $child_node;
			}
		}
	}

	# 2: Now assert constraints associated with Node-type details given in each 
	# "Child Quantity List" section of Language.pod.

	if( my $child_quants = %type_info{$TPI_CHILD_QUANTS} ) {
		for @child_quants -> $child_quant {
			my ($child_node_type, $range_min, $range_max) = @child_quant;
			my $child_count = 0;
			for @child_nodes -> $child_node {
				$child_node.:node_type eq $child_node_type or next;
				$child_count ++;
			}
			if( $child_count < $range_min ) { 
				if( ref($node_or_class) ) {
					$node_or_class.:_throw_error_message( 'SRT_N_ASDC_CH_N_TOO_FEW_SET', 
						{ 'COUNT' => $child_count, 'CNTYPE' => $child_node_type, 'EXPNUM' => $range_min } );
				} else {
					$node_or_class.:_throw_error_message( 'SRT_N_ASDC_CH_N_TOO_FEW_SET_PSN', 
						{ 'COUNT' => $child_count, 'PSNTYPE' => $pseudonode_name, 'EXPNUM' => $range_min } );
				}
			}
			if( $range_max.defined and $child_count > $range_max ) {
				# SHORT CUT: We know that with all of our existing config data, 
				# there are no pseudo-Nodes that have a maximum limit for children.
				$node_or_class.:_throw_error_message( 'SRT_N_ASDC_CH_N_TOO_MANY_SET', 
					{ 'COUNT' => $child_count, 'CNTYPE' => $child_node_type, 'EXPNUM' => $range_max } );
			}
		}
	}

	# 3: Now assert constraints associated with Node-type details given in each 
	# "Distinct Child Groups List" section of Language.pod.

	if( my $mudi_atgps = %type_info{$TPI_MUDI_ATGPS} ) {
		for @mudi_atgps -> $mudi_atgp {
			my ($mudi_name, $mudi_atgp_subsets) = @mudi_atgp;
			my %examined_children = ();
			for @mudi_atgp_subsets -> $mudi_atgp_subset {
				my ($child_node_type, @lits, @enums, @nrefs) = @mudi_atgp_subset;
				CHILD: for @child_nodes -> $child_node {
					$child_node.:node_type eq $child_node_type or next CHILD;
					my $hash_key = ',';
					for @lits -> $attr_name {
						my $val = $child_node.:at_literals.{$attr_name};
						$val.defined or next CHILD; # null values are always distinct
						$val ~~ s|,|<comma>|g; # avoid problems from literals containing delim chars
						$hash_key ~= $val~',';
					}
					for @enums -> $attr_name {
						my $val = $child_node.:at_enums.{$attr_name};
						$val.defined or next CHILD; # null values are always distinct
						$hash_key ~= $val~',';
					}
					for @nrefs -> $attr_name {
						my $val = $child_node.:at_nrefs.{$attr_name};
						$val.defined or next CHILD; # null values are always distinct
						$hash_key ~= $val~','; # stringifies to likes of 'HASH(NNN)'
					}
					if( %examined_children{$hash_key}.exists ) {
						# Multiple Nodes in same group have the same hash key, which 
						# means they are identical by means of the compared attributes.
						my $child_node_id = $child_node.:node_id;
						my $matched_child_node = %examined_children{$hash_key};
						my $matched_child_node_type = $matched_child_node.:node_type;
						my $matched_child_node_id = $matched_child_node.:node_id;
						# SHORT CUT: We know that with all of our existing config data, 
						# there are no pseudo-Nodes with TPI_MUDI_ATGPS, only Nodes.
						$node_or_class.:_throw_error_message( 'SRT_N_ASDC_MUDI_NON_DISTINCT', 
							{ 'VALUES' => $hash_key, 'MUDI' => $mudi_name, 
							'C1NTYPE' => $child_node_type, 'C1NID' => $child_node_id, 
							'C2NTYPE' => $matched_child_node_type, 'C2NID' => $matched_child_node_id } );
					}
					%examined_children{$hash_key} = $child_node;
				}
			}
		}
	}

	# TODO: more tests that examine multiple nodes together ...

	# This is the end of the tests that can be performed only on "Well Known" Nodes.
}

######################################################################

method get_all_properties( $node: Bool ?$links_as_si, Bool ?$want_shortest ) {
	return $node.:_get_all_properties( $links_as_si, $want_shortest );
}

method :_get_all_properties( $node: Bool ?$links_as_si, Bool ?$want_shortest ) {
	my $at_nrefs_in = $node.:at_nrefs;
	return {
		$NAMED_NODE_TYPE => $node.:node_type,
		$NAMED_ATTRS => {
			$ATTR_ID => $node.:node_id,
			# Note: We do not output $ATTR_PP => $node.:pp_nref since it is redundant.
			%{$node.:at_literals},
			%{$node.:at_enums},
			(map:{ ( $_ => (
					$links_as_si ?? 
					$node.get_relative_surrogate_id( $_, $want_shortest ) :: 
					$at_nrefs_in.{$_}.:node_id
				) ) } 
				$at_nrefs_in.keys),
		},
		$NAMED_CHILDREN => [@{$node.:prim_child_nrefs}.map:{ $_.:_get_all_properties( $links_as_si, $want_shortest ) }],
	};
}

method get_all_properties_as_perl_str( $node: Bool ?$links_as_si, Bool ?$want_shortest ) {
	return $node.:_serialize_as_perl( $node.:_get_all_properties( $links_as_si, $want_shortest ) );
}

method get_all_properties_as_xml_str( $node: Bool ?$links_as_si, Bool ?$want_shortest ) {
	return '<?xml version="1.0" encoding="UTF-8"?>'~"\n"~
		$node.:_serialize_as_xml( $node.:_get_all_properties( $links_as_si, $want_shortest ) );
}

######################################################################

method build_node( $node: *@args ) {
	my $container = $node.get_container() or 
		$node.:_throw_error_message( 'SRT_N_BUILD_ND_NOT_IN_CONT' );
	return $container.build_node( @args );
}

multi method build_child_node( $node: $node_type, %attrs ) {
	my $container = $node.get_container() or 
		$node.:_throw_error_message( 'SRT_N_BUILD_CH_ND_NOT_IN_CONT' );
	return $container.:_build_node_is_child_or_not( $node_type, %attrs, $node );
}

multi method build_child_node( $node: %args ) {
	return $node.build_child_node_tree( %args{$NAMED_NODE_TYPE}, %args{$NAMED_ATTRS} );
}

method build_child_nodes( $node: @list ) {
	for @list -> $element {
		$node.build_child_node( $element );
	}
}

multi method build_child_node_tree( $node: $node_type, %attrs, @children ) {
	my $new_node = $node.build_child_node( $node_type, %attrs );
	$new_node.build_child_node_trees( @children );
	return $new_node;
}

multi method build_child_node_tree( $node: %args ) {
	return $node.build_child_node_tree( %args{$NAMED_NODE_TYPE}, %args{$NAMED_ATTRS}, %args{$NAMED_CHILDREN} );
}

method build_child_node_trees( $node: @list ) {
	for @list -> $element {
		$node.build_child_node_tree( $element );
	}
}

######################################################################

} # class SQL::Routine::Node

######################################################################
######################################################################

1;

=head1 SYNOPSIS

I<The previous SYNOPSIS was removed; a new one will be written later.>

=head1 DESCRIPTION

The SQL::Routine (SRT) Perl 6 module provides a container object that allows
you to create specifications for any type of database task or activity (eg:
queries, DML, DDL, connection management) that look like ordinary routines
(procedures or functions) to your programs; all routine arguments are named.

SQL::Routine is trivially easy to install, since it is written in pure Perl and
its whole dependency chain consists of just 1 other pure Perl module.

Typical usage of this module involves creating or loading a single
SQL::Routine::Container object when your program starts up; this Container
would hold a complete representation of each database catalog that your program
uses (including details of all schema objects), plus complete representations
of all database invocations by your program; your program then typically just
reads from the Container while active to help determine its actions.

SQL::Routine can broadly represent, as an abstract syntax tree (a
cross-referenced hierarchy of nodes), code for any programming language, but
many of its concepts are only applicable to relational databases, particularly
SQL understanding databases.  It is reasonable to expect that a SQL:2003
compliant database should be able to implement nearly all SQL::Routine concepts
in its SQL stored procedures and functions, though SQL:2003 specifies some of
these concepts as optional features rather than core features.

This module has a multi-layered API that lets you choose between writing fairly
verbose code that performs faster, or fairly terse code that performs slower.

SQL::Routine is intended to be used by an application in place of using actual
SQL strings (including support for placeholders).  You define any desired
actions by stuffing atomic values into SQL::Routine objects, and then pass
those objects to a compatible bridging engine that will compile and execute
those objects against one or more actual databases.  Said bridge would be
responsible for generating any SQL or Perl code necessary to implement the
given SRT routine specification, and returning the result of its execution. 

The 'Rosetta' database portability library (a Perl 6 module) is a database
bridge that takes its instructions as SQL::Routine objects.  There may be other
modules that use SQL::Routine for that or other purposes.

SQL::Routine is also intended to be used as an intermediate representation of
schema definitions or other SQL that is being translated from one database
product to another.

This module is loosely similar to SQL::Statement, and is intended to be used in
all of the same ways.  But SQL::Routine is a lot more powerful and capable than
that module, as I most recently understand it, and is suitable for many uses
that the other module isn't.

SQL::Routine does not parse or generate any code on its own, nor does it talk
to any databases; it is up to external code that uses it to do this.

I<To cut down on the size of the SQL::Routine module itself, most of the POD
documentation is in these other files: L<SQL::Routine::Details>,
L<SQL::Routine::Language>, L<SQL::Routine::EnumTypes>,
L<SQL::Routine::NodeTypes>.>

=head1 CLASSES IN THIS MODULE

This module is implemented by several object-oriented Perl 6 packages, each of
which is referred to as a class.  They are: B<SQL::Routine> (the module's
name-sake), B<SQL::Routine::Container> (aka B<Container>, aka B<Model>),
and B<SQL::Routine::Node> (aka B<Node>).

I<While all 3 of the above classes are implemented in one module for
convenience, you should consider all 3 names as being "in use"; do not create
any modules or packages yourself that have the same names.>

The Container and Node classes do most of the work and are what you mainly use.
The name-sake class mainly exists to guide CPAN in indexing the whole module,
but it also provides a set of stateless utility methods and constants that the
other two classes inherit, and it provides a few wrapper functions over the
other classes for your convenience; you never instantiate an object of
SQL::Routine itself.

=head1 BRIEF FUNCTION AND METHOD LIST

Here is a compact list of this module's functions and methods along with their 
arguments.  For full details on each one, please see L<SQL::Routine::Details>.

CONSTRUCTOR WRAPPER FUNCTIONS:

	new_container()
	new_node( NODE_TYPE )

CONTAINER CONSTRUCTOR FUNCTIONS AND METHODS:

	new()

CONTAINER OBJECT METHODS:

	destroy()
	auto_assert_deferrable_constraints([ NEW_VALUE ])
	auto_set_node_ids([ NEW_VALUE ])
	may_match_surrogate_node_ids([ NEW_VALUE ])
	get_child_nodes([ NODE_TYPE ])
	find_node_by_id( NODE_ID )
	find_child_node_by_surrogate_id( TARGET_ATTR_VALUE )
	get_next_free_node_id()
	deferrable_constraints_are_tested()
	assert_deferrable_constraints()

NODE CONSTRUCTOR FUNCTIONS AND METHODS:

	new( NODE_TYPE )

NODE OBJECT METHODS:

	delete_node()
	get_node_type()
	get_node_id()
	clear_node_id()
	set_node_id( NEW_ID )
	get_primary_parent_attribute()
	clear_primary_parent_attribute()
	set_primary_parent_attribute( ATTR_VALUE )
	get_literal_attribute( ATTR_NAME )
	get_literal_attributes()
	clear_literal_attribute( ATTR_NAME )
	clear_literal_attributes()
	set_literal_attribute( ATTR_NAME, ATTR_VALUE )
	set_literal_attributes( ATTRS )
	get_enumerated_attribute( ATTR_NAME )
	get_enumerated_attributes()
	clear_enumerated_attribute( ATTR_NAME )
	clear_enumerated_attributes()
	set_enumerated_attribute( ATTR_NAME, ATTR_VALUE )
	set_enumerated_attributes( ATTRS )
	get_node_ref_attribute( ATTR_NAME[, GET_TARGET_SI] )
	get_node_ref_attributes([ GET_TARGET_SI ])
	clear_node_ref_attribute( ATTR_NAME )
	clear_node_ref_attributes()
	set_node_ref_attribute( ATTR_NAME, ATTR_VALUE )
	set_node_ref_attributes( ATTRS )
	get_surrogate_id_attribute([ GET_TARGET_SI ])
	clear_surrogate_id_attribute()
	set_surrogate_id_attribute( ATTR_VALUE )
	get_attribute( ATTR_NAME[, GET_TARGET_SI] )
	get_attributes([ GET_TARGET_SI ])
	clear_attribute( ATTR_NAME )
	clear_attributes()
	set_attribute( ATTR_NAME, ATTR_VALUE )
	set_attributes( ATTRS )
	get_container()
	put_in_container( NEW_CONTAINER )
	take_from_container()
	move_before_sibling( SIBLING[, PARENT] )
	get_child_nodes([ NODE_TYPE ])
	add_child_node( NEW_CHILD )
	add_child_nodes( LIST )
	get_referencing_nodes([ NODE_TYPE ])
	get_surrogate_id_chain()
	find_node_by_surrogate_id( SELF_ATTR_NAME, TARGET_ATTR_VALUE )
	find_child_node_by_surrogate_id( TARGET_ATTR_VALUE )
	get_relative_surrogate_id( SELF_ATTR_NAME )
	assert_deferrable_constraints()

CONTAINER OR NODE METHODS FOR DEBUGGING:

	get_all_properties([ LINKS_AS_SI ])
	get_all_properties_as_perl_str([ LINKS_AS_SI ])
	get_all_properties_as_xml_str([ LINKS_AS_SI ])

CONTAINER OR NODE FUNCTIONS AND METHODS FOR RAPID DEVELOPMENT:

	build_lonely_node( NODE_TYPE[, ATTRS] )
	build_node( NODE_TYPE[, ATTRS] )
	build_child_node( NODE_TYPE[, ATTRS] )
	build_child_nodes( LIST )
	build_child_node_tree( NODE_TYPE[, ATTRS][, CHILDREN] )
	build_child_node_trees( LIST )
	build_container( LIST[, AUTO_ASSERT[, AUTO_IDS[, MATCH_SURR_IDS]]] )

INFORMATION FUNCTIONS AND METHODS:

	valid_enumerated_types([ ENUM_TYPE ])
	valid_enumerated_type_values( ENUM_TYPE[, ENUM_VALUE] )
	valid_node_types([ NODE_TYPE ])
	node_types_with_pseudonode_parents([ NODE_TYPE ])
	node_types_with_primary_parent_attributes([ NODE_TYPE ])
	valid_node_type_literal_attributes( NODE_TYPE[, ATTR_NAME] )
	valid_node_type_enumerated_attributes( NODE_TYPE[, ATTR_NAME] )
	valid_node_type_node_ref_attributes( NODE_TYPE[, ATTR_NAME] )
	valid_node_type_surrogate_id_attributes([ NODE_TYPE ])

=head1 BUGS

This module is currently in alpha development status, meaning that some parts of
it will be changed in the near future, some perhaps in incompatible ways;
however, I believe that any further incompatible changes will be small.  The
current state is analogous to 'developer releases' of operating systems; it is
reasonable to being writing code that uses this module now, but you should be
prepared to maintain it later in keeping with API changes.  This module also
does not yet have full code coverage in its tests, though the most commonly used
areas are covered.

=head1 CAVEATS

All SQL::Routine::Container objects contain circular references by design (or
more specifically, when 1 or more Node is in one).  When you are done with a
Container object, you should explicitly call its "destroy()" method prior to
letting your references to it go out of scope, or you will leak the memory it
used.  Note that some early versions of SQL::Routine had wrapped the actual
Container object in a second object that was auto-destroyed when it went out of
scope, but this cludge was later removed due to adding worse problems than it
solved, such as Containers being destroyed too early.  In the future, we may try
to use a "weak reference" adjunct feature to Perl to remove the need for
explicit destruction in a more elegant fashion, but not necessarily.

You can not use surrogate id values that look like valid Node ids (that are
positive integers) since some methods won't do what you expect when given such
values.  Nodes having such surrogate id values won't be matched by values
passed to set_node_ref_attribute(), directly or indirectly.  That method only
tries to lookup a Node by its surrogate id if its argument doesn't look like a
Node ref or a Node id.  Similarly, the build*() methods will decide whether to
interpret a defined but non-Node-ref ATTRS argument as a Node id or a surrogate
id based on its looking like a valid Node id or not.  You should rarely
encounter this caveat, though, since you would never use a number as a "SQL
identifier" in normal cases, and that is only technically possible with a
"delimited SQL identifier".

=head1 CREDITS

Besides myself as the creator ...

* 2004.05.20 - Thanks to Jarrell Dunson (jarrell_dunson@asburyseminary.edu) for
inspiring me to add some concrete SYNOPSIS documentation examples to this
module, which demonstrate actual SQL statements that can be generated from parts
of a model, when he wrote me asking for examples of how to use this module.

* 2005.03.21 - Thanks to Stevan Little (stevan@iinteractive.com) for feedback
towards improving this module's documentation, particularly towards using a much
shorter SYNOPSIS, so that it is easier for newcomers to understand the module at
a glance, and not be intimidated by large amounts of detailed information.

=head1 SEE ALSO

L<perl(1)>, L<SQL::Routine::L::en>, L<SQL::Routine::Details>,
L<SQL::Routine::Language>, L<SQL::Routine::EnumTypes>,
L<SQL::Routine::NodeTypes>, L<SQL::Routine::API_C>, L<Locale::KeyedText>,
L<Rosetta>, L<Rosetta::Engine::Generic>, L<SQL::Routine::SQLBuilder>,
L<SQL::Routine::SQLParser>, L<DBI>, L<SQL::Statement>, L<SQL::Translator>,
L<SQL::YASP>, L<SQL::Generator>, L<SQL::Schema>, L<SQL::Abstract>,
L<SQL::Snippet>, L<SQL::Catalog>, L<DB::Ent>, L<DBIx::Abstract>,
L<DBIx::AnyDBD>, L<DBIx::DBSchema>, L<DBIx::Namespace>, L<DBIx::SearchBuilder>,
L<TripleStore>, L<Data::Table>, and various other modules.

=cut
