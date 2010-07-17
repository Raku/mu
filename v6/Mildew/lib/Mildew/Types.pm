use v5.10;
use Mildew::Emit::Yeast;
use MooseX::Declare;
class Mildew::TypeInfo {
    has type=>(is=>'rw',builder=>'infer_type',lazy=>1);
    has mold=>(is=>'rw',isa=>'AST::Block::SSA');
    has usage=>(is=>'ro',isa=>'ArrayRef[AST::Base]',default=>sub {[]});
    method add_usage($stmt) {
        push(@{$self->usage},$stmt);
    }
    method infer_type {
        Mildew::Type::Unknown->new();
    }
}

class Mildew::TypeInfo::FromAssignment extends Mildew::TypeInfo {
    has orgin=>(is=>'ro',isa=>'AST::Base',required=>1);
    method infer_type {
        $self->type(Mildew::Type::SelfRecursive->new());
        my $rvalue = $self->orgin->rvalue;
        if ($rvalue->isa('AST::Call')) {
            my $type = $rvalue->capture->invocant->type_info->type->method_call($self->orgin);
            $self->type($type);
            for my $usage (@{$self->usage}) {
                $type->add_usage($self->orgin->lvalue,$usage);
            }
            $type;
        } else {
            Mildew::Type::Unknown->new();
        }
    }
}
class Mildew::TypeInfo::Phi extends Mildew::TypeInfo {
}
class Mildew::TypeInfo::IntegerConstant extends Mildew::TypeInfo {
    method infer_type {
        Mildew::Type::IntegerConstant->new();
    }
}
class Mildew::TypeInfo::StringConstant extends Mildew::TypeInfo {
}
class Mildew::TypeInfo::External extends Mildew::TypeInfo {
}
class Mildew::Type {
    sub str {
        my ($value,$str) = @_;
        is_str($value) && $value->value;
    }
    sub is_str {
        my ($value) = @_;
        defined $value && $value->isa('AST::StringConstant');
    }
    method debug {
    }
    method method_call($call) {
        Mildew::Type::Unknown->new();
    }
    method emit_call($i,$stmt,$value) {
        my $list = sub {
            "(SMOP__Object*[]) {" . join(',',(map {"SMOP_REFERENCE(interpreter,".$value->($_).")"} @_),"NULL") . "}"
        };
        my $capture = $stmt->rvalue->capture;

        Mildew::Emit::Yeast::measure($stmt->id,"frame->pc = " . ($i+1) . ";\n" 
        . "frame->ret = &" . $value->($stmt->lvalue) . ";\n" 
        . Mildew::Emit::Yeast::assign($value->($stmt->lvalue),
            "SMOP_DISPATCH(\n" . "interpreter,\nSMOP_RI(" . $value->($capture->invocant) . "),\n"
            . $value->($stmt->rvalue->identifier)
            . ",\nSMOP__NATIVE__capture_create(interpreter," 
            . $list->($capture->invocant,@{$capture->positional})
            . ","
            . $list->(@{$capture->named})
            . ")\n" . ")")
        . "break;\n");
    }
    method emit_perlesque_call($stmt,$value) {
        my $list = sub {
            "[" . join(',',(map {$value->($_)} @_)) . "]"
        };
        my $capture = $stmt->rvalue->capture;
        my $perlesque_capture = AST::unique_id();

        my $add_named = '';
        my @named = @{$capture->named};
        use Devel::PartialDump qw(warn);
        while (@named) {
            warn("named:",\@named);
            $add_named .= "$perlesque_capture.add_named(" . $value->(shift @named) . "," .$value->(shift @named) . ")";
        }
        "my P6capture $perlesque_capture = P6capture.new();\n"
        . $add_named
        . join("",(map {"$perlesque_capture.add_positional(" . $value->($_) . ");\n"} ($capture->invocant,@{$capture->positional})))
        . $value->($stmt->lvalue) . " = " 
        . $value->($capture->invocant)
        . ".DISPATCH(" . $value->($stmt->rvalue->identifier) 
        . ",$perlesque_capture"
        #. ",p6capture("
        #. $list->($capture->invocant,@{$capture->positional})
        #. ','
        #. $list->(@{$capture->named})
        . ");"
    }
    method add_usage($reg,$stmt) {
    }
    method pretty() {
        ref $self;
    }
}
role Mildew::Type::FETCH {
    method method_call($stmt) {
        my $call = $stmt->rvalue;
        my $id = Mildew::Type::str($call->identifier);
        if ($id eq 'FETCH') {
            $self;
        } else {
            Mildew::Type::Unknown->new();
        }
    }
}
class Mildew::Type::IntegerConstant extends Mildew::Type with Mildew::Type::FETCH {
}
class Mildew::Type::Prototype extends Mildew::Type {
    has type=>(is=>'ro');
    method method_call($stmt) {
        my $call = $stmt->rvalue;
        my $id = Mildew::Type::str($call->identifier);
        if ($id eq 'new') {
            $self->type->();
        } elsif ($id eq 'FETCH') {
            $self;
        }
    }
}
class Mildew::Type::Scope extends Mildew::Type {
    has content=>(is=>'rw');
    has reg=>(is=>'rw',isa=>'AST::Reg');
    has outer=>(is=>'ro',isa=>'Mildew::Type');
    use Scalar::Util qw(refaddr);
    use Term::ANSIColor qw(:constants);

    # XXX do more genericly with add_usage
    method infer_lexicals {

        $self->debug("infering lexicals"); 
        #return undef unless $self->reg; 

        $self->content({});

        for my $stmt (@{$self->reg->type_info->usage}) {
            if ($stmt->rvalue->isa('AST::Call')) {
                my $call = $stmt->rvalue;
                if (refaddr($call->capture->invocant) == refaddr($self->reg)) {
                    if (
                        @{$call->capture->positional} == 1
                        && Mildew::Type::is_str($call->capture->positional->[0])
                    ) {
                        my $name = Mildew::Type::str($call->capture->positional->[0]);
                        if (Mildew::Type::str($call->identifier) eq 'postcircumfix:{ }') {
                            $self->debug("variable defined in scope: ",GREEN,$name,RESET);
                            $self->content->{$name} = Mildew::Type::Lexical->new();
                            $stmt->lvalue->type_info->type();
                        } elsif (Mildew::Type::str($call->identifier) eq 'lookup') {
                            $self->debug("variable used in scope:",GREEN,$name,RESET);
                            $stmt->lvalue->type_info->type();
                        } else {
                            $self->debug("unknown usage of scope:",RED,$stmt->pretty,RESET);
                        }
                    } else {
                        $self->debug("unknown usage of scope:",$stmt->pretty);
                    }
                }
            }
        }
        $self->debug("infered lexicals");
    }
    method lookup($varname) {
        $self->debug("looking up $varname");
        if (!defined $self->content) {
            return Mildew::Type::Unknown->new();
        }
        if (my $type = $self->content->{$varname}) {
            $type;
        } elsif ($self->outer) {
            $self->outer->lookup($varname);
        } else {
            Mildew::Type::Unknown->new();
        }
    }
    method method_call($stmt) {
        my $call = $stmt->rvalue;
        $self->reg($call->capture->invocant);
        if (!defined $self->content) {
            $self->infer_lexicals;
        }
        my $id = Mildew::Type::str($call->identifier);
        if (($id eq 'lookup' || $id eq 'postcircumfix:{ }')  && Mildew::Type::is_str($call->capture->positional->[0])) {
            $self->debug("handling method:",$id);
            $self->lookup(Mildew::Type::str($call->capture->positional->[0]));
        } else {
            $self->debug("not handling method:",$id);
            Mildew::Type::Unknown->new();
        }
    }
}
class Mildew::Type::Lexical extends Mildew::Type {
    use Term::ANSIColor qw(:constants);
    use Scalar::Util qw(refaddr);
    use Carp qw(cluck);
    has content=>(is=>'rw',isa=>'Mildew::Type',lazy_build=>1);
    has binds=>(is=>'ro',isa=>'ArrayRef[Type]',default=>sub {[]});
    has stores=>(is=>'ro',isa=>'ArrayRef[Type]',default=>sub {[]});
    method _build_content {
        $self->debug("infering content of lexical");
        my $container;
        if (@{$self->binds} == 1) {
            $container = $self->binds->[0];
            $self->debug("1 BIND: ",$self->binds->[0]);
        } else {
            $self->debug("many BINDs");
            $container = Mildew::Type::Scalar->new();
        }

        use Data::Dumper;
        $self->debug('stores = ',Dumper($self->stores));
        if ($container->can('add_store')) {
            $container->add_store($_) for @{$self->stores};
        }
        $self->debug("infered content of lexical");
        $container;
    }
    method method_call($call) {
        if (Mildew::Type::str($call->rvalue->identifier) eq 'FETCH') {
            $self->debug(RED,"called FETCH on lexical",RESET);
            $self->content->method_call($call);
        } elsif (Mildew::Type::str($call->rvalue->identifier) eq 'BIND') {
            $self;
        } else {
            Mildew::Type::Unknown->new();
        }
    }
    method add_usage($reg,$usage) {
        if ($usage->isa('AST::Assign')) {
            my $call = $usage->rvalue;
            if ($call->isa('AST::Call') && (refaddr $call->capture->invocant == refaddr $reg)) {
                my $id = Mildew::Type::str($call->identifier);
                if ($id eq 'BIND') {
                    push (@{$self->binds},$call->capture->positional->[0]->type_info->type);
                    $self->debug("propagating {");
                    $usage->lvalue->type_info->type();
                    $self->debug("}");
                    return;
                } elsif ($id eq 'STORE') {
                    $self->debug("STORE on lexicals {");
                    push (@{$self->stores},$call->capture->positional->[0]->type_info->type);
                    $self->debug("}");
                    return;
                } elsif ($id eq 'FETCH') {
                    return;
                }
            }
        }
        $self->debug(RED,"unknow usage of lexical ",$reg->pretty,": ",$usage->pretty,RESET);
    }
    method pretty {
        (ref $self) . " of " . $self->content->pretty;
    }    
}
class Mildew::Type::Scalar extends Mildew::Type {
    use Carp qw(cluck);
    use Scalar::Util qw(refaddr);
    has stores=>(is=>'ro',isa=>'ArrayRef[Type]',default=>sub {[]});
    has content=>(is=>'rw',builder=>'infer_content',lazy=>1,predicate=>'has_content');
    method add_store($content) {
        $self->debug("adding store to ",(refaddr $self));
        push(@{$self->stores},$content);
    }
    method infer_content {
        if (@{$self->stores} == 1) {
            $self->debug("just enough stores");
            $self->stores->[0];
        } else {
            use Data::Dumper;
            #cluck "wrong number of stores: ",(refaddr $self),Dumper($self->stores);
            Mildew::Type::Unknown->new();
        }
    }
    method method_call($call) {
        if (Mildew::Type::str($call->rvalue->identifier) eq 'FETCH') {
            $self->content;
        } else {
            Mildew::Type::Unknown->new();
        }
    }
    method pretty {
        'Mildew::Type::Scalar of ' . ($self->has_content ? $self->content->pretty : '(not calculated yet value)');
    }    
}
class Mildew::Type::Unknown extends Mildew::Type {
}

class Mildew::Type::SelfRecursive extends Mildew::Type::Unknown {
    method method_call($call) {
        $self->debug("method call on self recursive\n");
        Mildew::Type::SelfRecursive->new();
    }
}

class Mildew::Type::MildewSOLoader extends Mildew::Type {
}
$Mildew::LexicalPreludeType = Mildew::Type::Scope->new(
    content => {
        MildewSOLoader => Mildew::Type::Lexical->new(content=>Mildew::Type::MildewSOLoader->new()),
        Scalar => Mildew::Type::Prototype->new(type=>sub {Mildew::Type::Scalar->new()}),
    }
);
