use v5.10;
use Emit::Yeast;
use MooseX::Declare;
class TypeInfo {
    has type=>(is=>'rw',builder=>'infer_type',lazy=>1);
    has mold=>(is=>'rw',isa=>'AST::Block::SSA');
    has usage=>(is=>'ro',isa=>'ArrayRef[AST::Base]',default=>sub {[]});
    method add_usage($stmt) {
        push(@{$self->usage},$stmt);
    }
    method infer_type {
        Type::Unknown->new();
    }
}

class TypeInfo::FromAssignment extends TypeInfo {
    has orgin=>(is=>'ro',isa=>'AST::Base',required=>1);
    method infer_type {
        say "infering type for:",$self->orgin->pretty;
        my $rvalue = $self->orgin->rvalue;
        if ($rvalue->isa('AST::Call')) {
            my $type = $rvalue->capture->invocant->type_info->type->method_call($self->orgin);
            for my $usage (@{$self->usage}) {
                $type->add_usage($self->orgin->lvalue,$usage);
            }
            $type;
        } else {
            Type::Unknown->new();
        }
    }
}
class TypeInfo::Phi extends TypeInfo {
}
class TypeInfo::IntegerConstant extends TypeInfo {
    method infer_type {
        Type::IntegerConstant->new();
    }
}
class TypeInfo::StringConstant extends TypeInfo {
}
class TypeInfo::External extends TypeInfo {
}
class Type {
    sub str {
        my ($value,$str) = @_;
        is_str($value) && $value->value;
    }
    sub is_str {
        my ($value) = @_;
        defined $value && $value->isa('AST::StringConstant');
    }
    method method_call($call) {
        Type::Unknown->new();
    }
    method emit_call($i,$stmt,$value) {
        my $list = sub {
            "(SMOP__Object*[]) {" . join(',',(map {"SMOP_REFERENCE(interpreter,".$value->($_).")"} @_),"NULL") . "}"
        };
        my $capture = $stmt->rvalue->capture;

        Emit::Yeast::measure($stmt->id,"frame->pc = " . ($i+1) . ";\n" 
        . "frame->ret = &" . $value->($stmt->lvalue) . ";\n" 
        . Emit::Yeast::assign($value->($stmt->lvalue),
            "SMOP_DISPATCH(\n" . "interpreter,\nSMOP_RI(" . $value->($capture->invocant) . "),\n"
            . $value->($stmt->rvalue->identifier)
            . ",\nSMOP__NATIVE__capture_create(interpreter," 
            . $list->($capture->invocant,@{$capture->positional})
            . ","
            . $list->(@{$capture->named})
            . ")\n" . ")")
        . "break;\n");
    }
    method add_usage($reg,$stmt) {
    }
    method pretty() {
        ref $self;
    }
}
role Type::FETCH {
    method method_call($stmt) {
        my $call = $stmt->rvalue;
        my $id = Type::str($call->identifier);
        if ($id eq 'FETCH') {
            $self;
        } else {
            Type::Unknown->new();
        }
    }
}
class Type::IntegerConstant extends Type with Type::FETCH {
}
class Type::Prototype extends Type {
    has type=>(is=>'ro');
    method method_call($stmt) {
        my $call = $stmt->rvalue;
        my $id = Type::str($call->identifier);
        if ($id eq 'new') {
            $self->type;
        } elsif ($id eq 'FETCH') {
            $self;
        }
    }
}
class Type::Scope extends Type {
    has content=>(is=>'rw');
    has reg=>(is=>'rw',isa=>'AST::Reg');
    has outer=>(is=>'ro',isa=>'Type');
    use Scalar::Util qw(refaddr);
    use Term::ANSIColor qw(:constants);
    method infer_lexicals {

        
        #return undef unless $self->reg; 

        $self->content({});

        for my $stmt (@{$self->reg->type_info->usage}) {
            if ($stmt->rvalue->isa('AST::Call')) {
                my $call = $stmt->rvalue;
                if (refaddr($call->capture->invocant) == refaddr($self->reg)) {
                    if (
                        @{$call->capture->positional} == 1
                        && Type::is_str($call->capture->positional->[0])
                    ) {
                        my $name = Type::str($call->capture->positional->[0]);
                        if (Type::str($call->identifier) eq 'postcircumfix:{ }') {
                            say "variable defined in scope: ",GREEN,$name,RESET;
                            $self->content->{$name} = Type::Lexical->new();
                        } elsif (Type::str($call->identifier) eq 'lookup') {
                            say "variable used in scope:",GREEN,$name,RESET;
                        } else {
                            say "unknown usage of scope:",RED,$stmt->pretty,RESET;
                        }
                    } else {
                        say "unknown usage of scope:",$stmt->pretty;
                    }
                }
            }
        }
    }
    method lookup($varname) {
        if (!defined $self->content) {
            return Type::Unknown->new();
        }
        if (my $type = $self->content->{$varname}) {
            $type;
        } elsif ($self->outer) {
            $self->outer->lookup($varname);
        } else {
            Type::Unknown->new();
        }
    }
    method method_call($stmt) {
        my $call = $stmt->rvalue;
        $self->reg($call->capture->invocant);
        if (!defined $self->content) {
            $self->infer_lexicals;
        }
        my $id = Type::str($call->identifier);
        if (($id eq 'lookup' || $id eq 'postcircumfix:{ }')  && Type::is_str($call->capture->positional->[0])) {
            say "handling method:",$id;
            $self->lookup(Type::str($call->capture->positional->[0]));
        } else {
            say "not handling method:",$id;
            Type::Unknown->new();
        }
    }
}
class Type::Lexical extends Type {
    use Term::ANSIColor qw(:constants);
    use Scalar::Util qw(refaddr);
    use Carp qw(cluck);
    has content=>(is=>'rw',isa=>'Type',lazy_build=>1);
    has binds=>(is=>'ro',isa=>'ArrayRef[Type]',default=>sub {[]});
    has stores=>(is=>'ro',isa=>'ArrayRef[Type]',default=>sub {[]});
    method _build_content {
        cluck "infering content of lexical";
        my $container;
        if (@{$self->binds} == 1) {
            $container = $self->binds->[0];
            say "1 BIND: ",$self->binds->[0]->pretty;
        } else {
            say "many BINDs";
            $container = Type::Scalar->new();
        }

        use Data::Dumper;
        say 'stores = ',Dumper($self->stores);
        if ($container->can('add_store')) {
            $container->add_store($_) for @{$self->stores};
        }
        $container;
    }
    method method_call($call) {
        if (Type::str($call->rvalue->identifier) eq 'FETCH') {
            $self->content;
        } elsif (Type::str($call->rvalue->identifier) eq 'BIND') {
            $self;
        } else {
            Type::Unknown->new();
        }
    }
    method add_usage($reg,$usage) {
        if ($usage->isa('AST::Assign')) {
            my $call = $usage->rvalue;
            if ($call->isa('AST::Call') && refaddr $call->capture->invocant == refaddr $reg) {
                my $id = Type::str($call->identifier);
                if ($id eq 'BIND') {
                    say "BIND";
                    push (@{$self->binds},$call->capture->positional->[0]->type_info->type);
                    return;
                } elsif ($id eq 'STORE') {
                    cluck "STORE on lexicals";
                    push (@{$self->stores},$call->capture->positional->[0]->type_info->type);
                    return;
                }
            }
        }
        say RED,"usage ",$usage->pretty,RESET;
    }
    method pretty {
        (ref $self) . " of " . $self->content->pretty;
    }    
}
class Type::Scalar extends Type {
    has stores=>(is=>'ro',isa=>'ArrayRef[Type]',default=>sub {[]});
    has content=>(is=>'rw',builder=>'infer_content',lazy=>1);
    method add_store($content) {
        push(@{$self->stores},$content);
    }
    method infer_content {
        if (@{$self->stores} == 1) {
            $self->stores->[0];
        } else {
            Type::Unknown->new();
        }
    }
    method pretty {
        (ref $self) . " of " . $self->content->pretty;
    }    
}
class Type::Unknown extends Type {
}
class Type::MildewSOLoader extends Type {
}
$Mildew::LexicalPreludeType = Type::Scope->new(
    content => {
        MildewSOLoader => Type::Lexical->new(content=>Type::MildewSOLoader->new()),
        Scalar => Type::Prototype->new(type=>Type::Scalar->new()),
    }
);
