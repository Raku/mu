
use v6-alpha;

class KindaPerl6::Visitor::Emit::AstYaml {

    method visit ( $node, $node_name ) {
	if $node_name eq 'CompUnit' {
	    return $node.emit_ast_yaml($.visitor_args{'secure'});
	} else {
	    return ' ';
	}
    };
}

class CompUnit {
    method emit_ast_yaml( $args_secure ) { 
	Main::emit_yaml(self);
    };
}
