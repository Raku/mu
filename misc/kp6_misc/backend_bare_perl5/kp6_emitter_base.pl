# Framework for creating kp6 ast based emitters.
require '../kp6_ast/kp6_ast_def.pl';
our @nodes = KP6_AST_Def::nodes();

package Kp6Emitter;
sub new {
    bless {
	inject_under => [],
	multis => [],
    },$_[0];
}

sub warning {
    <<"END";
# WARNING
# This file is written by $0.
# YOUR EDITS TO THIS FILE WILL BE LOST.

END
}

sub config_inject_under {
    my($self,$class_path_stem)=@_;
    push(@{$self->{inject_under}},$class_path_stem);
}
sub config_declare_multi {
    my($self,$multi_name)=@_;
    push(@{$self->{multis}},$multi_name);
}

sub def_inject {
    my($self,$method_name,$node_name,$emitter_code)=@_;
    my $code = '';
    $code .= 'sub '.$method_name." {\n".'    my($node)=@_;'."\n";
    $code .= $self->_def_method_helper($node,'$node',$emitter_code);
    my $all_code = '';
    for my $under (@{$self->{inject_under}}) {
	my $pkg = $under.'::'.$node_name;
	$all_code .= "{package $pkg;\n$code\n}\n";
    }
    $all_code;
}

sub def_multi {
    my($self,$multi_name,$node_name,$emitter_code)=@_;
    my $node = KP6_AST_Def->node_from_name($node_name);
    my $method_name = $self->multi_method_name($multi_name,$node_name);
    my $code = 'sub '.$method_name." {\n".'    my($self,$node)=@_;'."\n";
    $code .= $self->_def_method_helper($node,'$node',$emitter_code);
    $code;
}
sub multi_method_name {
    my($self,$multi_name,$node_name)=@_;
    $node_name =~ s/::/_/g;
    $multi_name.'_'.$node_name;
}
sub _def_method_helper {
    my($self,$node,$var_of_node,$emitter_code)=@_;
    my $code = '';
    my @fields = $node->fields;
    if(@fields) {
	$code .= '    my(';
	for my $field (@fields){ $code .= '$'.$field->identifier.','; }
	substr($code,-1) = ""; #clip the trailing comma
	$code .= ')=(';
	for my $field (@fields){ $code .= $var_of_node.'->{'.$field->identifier.'},'; }
	substr($code,-1) = ""; #clip the trailing comma
	$code .= ");\n";
    }
    if($emitter_code =~ /^{/) {
	$emitter_code =~ s/\n}\n?$/\n/
	    or die "missing close bracket: $name\n$emitter_code\n";
	$emitter_code =~ s/^{ *\n?//;
    }
    $code .= $emitter_code;
    $code .= "}\n";
    $code;
}

#----------------------------------------
sub configured_class_setup {
    my($self)=@_;

    # new
    my $code = <<'END';
use SimpleDispatchOnTypeSuffix;

sub new {
    my($cls)=@_;
    my $self = {
	dispatch => {}
    };
    bless $self,$cls;
    $self->initialize();
    $self;
}

END

    # multis - entry points
    for my $multi (@{$self->{multis}}) {
	my $fun = <<'END';
sub MULTI {
    my($self,$obj,@rest)=@_;
    my $handler = $self->{dispatch}{MULTI}->lookup($obj);
    die "No handler for $obj" if not $handler;
    my $result = $handler->($self,$obj,@rest);
    $result;
}
END
        $fun =~ s/MULTI/$multi/g;
        $code .= $fun;
    }

    # multis - dispatcher config
    $code .= <<'END';
sub initialize {
    my($self)=@_;
END

    for my $multi (@{$self->{multis}}) {
	$code .= "    \$self->{dispatch}{$multi} = SimpleDispatchOnTypeSuffix->new();\n";
	for my $node (@nodes) {
	    my $node_name = $node->name;
	    my $dispatch_name = '::'.$node_name; #XXX should be \b, not ::
	    my $method_name = $self->multi_method_name($multi,$node_name);
	    $code .= "    \$self->{dispatch}{$multi}->dispatch_type_to('$dispatch_name',\\\&$method_name);\n";
	}
    }

    $code .= <<'END';
}

END
    $code;
}
#----------------------------------------
sub setup_ast_inheritance {
    my($self,$ast_root)=@_;
    my $root_mixin = $ast_root.'::'.'AllNodes';
    my $code = "";
    $code .= "{package $root_mixin; }\n";
    for my $node (@nodes) {
	my $node_name = $node->name;
	my $pkg = $ast_root.'::'.$node_name;
	$code .= "{package ${pkg}; BEGIN{ push(\@${pkg}::ISA, qw{ $root_mixin }); }}\n";
    }
    $code;
}
# XXX once we know which fields contain nodes, add a only-node-fields helper.
sub setup_ast_helpers {
    my($self,$ast_root)=@_;
    my $code = "";
    for my $a_node (@nodes) {
	my $node_name = $a_node->name;
	my $pkg = $ast_root.'::'.$node_name;
        my $fields = join(",", map{"'".$_->identifier."'"} $a_node->fields);
	$code .= "{package ${pkg};\n";

	$code .= <<'END';
sub all_fields {
    my($node)=@_;
END
        if($fields ne "")  {
	    $code .= '    '.'@$node{'.$fields."};\n";
	} else {
	    $code .= '    '.'return()'.";\n";
	}
	$code .= <<'END';
}
END

	$code .= <<'END';
sub all_field_names {
    my($node)=@_;
END
        $code .= '    '."return($fields);\n";
	$code .= <<'END';
}
END

        $code .= "}\n";
    }
    $code;
}

1;
__END__
sub create_empty_emitter_template {
    for my $node (@nodes) {
	my $name = $node->name;
	print "node '$name',<<\'END\';\n{\n    # ";
	for my $f ($node->fields) {
	    print '$'.$f->identifier," ";
	}
	print "\n\n}\nEND\n"
    }
}
create_empty_emitter_template;
