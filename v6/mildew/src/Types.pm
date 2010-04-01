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
        my $rvalue = $self->orgin->rvalue;
        if ($rvalue->isa('AST::Call')) {
            $rvalue->capture->invocant->type_info->type->method_call($self->orgin);
        } else {
            Type::Unknown->new();
        }
    }
}
class TypeInfo::Phi extends TypeInfo {
}
class TypeInfo::IntegerConstant extends TypeInfo {
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
    method add_usage($reg) {
    }
    method pretty() {
        ref $self;
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
                            my $var = $self->content->{$name} = Type::Lexical->new();
                            $var->add_usage($stmt->lvalue);
                        } elsif (Type::str($call->identifier) eq 'lookup') {
                            say "variable used in scope:",GREEN,$name,RESET;
                            $self->lookup($name)->add_usage($stmt->lvalue);
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
    has content=>(is=>'rw',isa=>'Type',lazy_build=>1);
    has binds=>(is=>'ro',isa=>'ArrayRef[Type]',default=>sub {[]});
    method _build_content {
        my $container;
        if (@{$self->binds} == 1) {
            $container = $self->binds->[0];
            say "1 BIND: ",$self->binds->[0]->pretty;
        } else {
            say "many BINDs";
            $container = Type::Scalar->new();
        }
        $container;
    }
#    method method_call($call) {
#        if (Type::str($call->rvalue->identifier) eq 'FETCH') {
#            $self->content;
#        } else {
#            Type::Unknown->new();
#        }
#    }
    method add_usage($reg) {
        for my $usage (@{$reg->type_info->usage}) {
            if ($usage->isa('AST::Assign')) {
                my $call = $usage->rvalue;
                if ($call->isa('AST::Call')) {
                    my $id = Type::str($call->identifier);
                    if ($id eq 'BIND') {
                        say "BIND";
                        push (@{$self->binds},$call->capture->positional->[0]->type_info->type);
                        next;
                    }
                }
            }
            say RED,"usage ",$usage->pretty,RESET;
        }
    }
    method pretty {
        (ref $self) . " of " . $self->content->pretty;
    }    
}
class Type::Scalar extends Type {
}
class Type::Unknown extends Type {
}
class Type::MildewSOLoader extends Type {
}
$Mildew::LexicalPreludeType = Type::Scope->new(
    content => {
        'MildewSOLoader' => Type::Lexical->new(content=>Type::MildewSOLoader->new()),
    }
);
