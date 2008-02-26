# Framework for creating kp6 ast based emitters.
require '../kp6_ast/kp6_ast_def.pl';
our @nodes = KP6_AST_Def::nodes();

package Kp6Emitter;
sub new {
    bless {},$_[0];
}

sub warning {
    <<"END";
# WARNING
# This file is written by $0.
# YOUR EDITS TO THIS FILE WILL BE LOST.

END
}

#----------------------------------------
sub use_external_dispatch {
    my($self)=@_;
    my $code = <<'END';
use SimpleDispatchOnTypeSuffix;

sub new {
    my($cls)=@_;
    my $self = {
	dispatcher => SimpleDispatchOnTypeSuffix->new(),
    };
    bless $self,$cls;
    $self->initialize();
    $self;
}

sub dispatch_type_to {
    my($self,$type,$to)=@_;
    $self->{dispatcher}->dispatch_type_to($type,$to);
}

sub emit {
    my($self,$obj,@rest)=@_;
    my $handler = $self->{dispatcher}->lookup($obj);
    die "No handler for $obj" if not $handler;
    my $code = $handler->($self,$obj,@rest);
    $code;
}

sub initialize {
    my($self)=@_;
END

    for my $node (@nodes) {
	my $name = $node->name;
	my $dispatch_name = '::'.$name;
	my $method_name = $self->method_name($name);
	$code .= "    \$self->dispatch_type_to('$dispatch_name',\\\&$method_name);\n";
    }

    $code .= <<'END';
}

sub emit_ast {
    my($cls,$ast)=@_;
    $cls->new()->emit($ast);
}

END
    $code;
}

#----------------------------------------
sub use_injection_dispatch {
    my($self,$pkg)=@_;
    die "unimplemented";
    my $code = <<'END';

sub new {
    my($cls)=@_;
    my $self = {
    };
    bless $self,$cls;
    $self;
}
END

}


#----------------------------------------
sub method_name {
    my($self,$node_name)=@_;
    $node_name =~ s/::/_/g;
    'emit_kp6_'.$node_name;
}
sub node {
    my($self,$name,$emitter_code)=@_;
    my $node = KP6_AST_Def->node_from_name($name);
    my $method_name = $self->method_name($name);
    my $code = 'sub '.$method_name." {\n".'    my($self,$obj)=@_;'."\n";
    my @fields = $node->fields;
    if(@fields) {
	$code .= '    my(';
	for my $field (@fields){ $code .= '$'.$field->identifier.','; }
	substr($code,-1) = ""; #clip the trailing comma
	$code .= ')=(';
	for my $field (@fields){ $code .= '$obj->{'.$field->identifier.'},'; }
	substr($code,-1) = ""; #clip the trailing comma
	$code .= ");\n";
    }
    $code .= $emitter_code;
    $code .= "}\n";
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
