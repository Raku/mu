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
}
class Type::Scope extends Type {
    has content_types=>(is=>'ro',lazy=>1,builder=>'_build_content_types');
    has reg=>(is=>'rw',isa=>'AST::Reg');
    has outer=>(is=>'ro',isa=>'Type');
    use Scalar::Util qw(refaddr);
    method _build_content_types {
        say "_build_content_types";


        return undef unless $self->reg; 

        my %content;
        for my $stmt (@{$self->reg->type_info->usage}) {
            if ($stmt->rvalue->isa('AST::Call')) {
                my $call = $stmt->rvalue;
                if (refaddr($call->capture->invocant) == refaddr($self->reg)) {
                    if (
                        @{$call->capture->positional} == 1
                        && Type::is_str($call->capture->positional->[0])
                    ) {
                        my $var = Type::str($call->capture->positional->[0]);
                        if (Type::str($call->identifier) eq 'postcircumfix:{ }') {
                            say "variable defined in scope: ",$var;
                            for my $usage_of_lexical (@{$stmt->lvalue->type_info->usage}) {
                                say "usage ",$usage_of_lexical->pretty;
                            }
                        } elsif (Type::str($call->identifier) eq 'lookup') {
                            say "variable used in scope: $var";
                            for my $usage_of_lexical (@{$stmt->lvalue->type_info->usage}) {
                                say "usage ",$usage_of_lexical->pretty;
                            }
                        } else {
                            say "unknown usage of scope:",$stmt->pretty;
                        }
                    } else {
                        say "unknown usage of scope:",$stmt->pretty;
                    }
                }
            }
        }
#                        $content{$stmt} = ;
#                        say "postcircumfix ",$stmt->pretty," -> ";
#                    if (Type::str($call->identifier) eq 'lookup') {
#                        say "lookup ",$stmt->pretty," -> ";
#                        for my $usage_of_lexical (@{$stmt->lvalue->type_info->usage}) {
#                            say "  ",$usage_of_lexical->pretty;
#                        }
#                    } else {
#                    }
        \%content;
    }
    method lookup($varname) {
        if (!defined $self->content_types) {
            return Type::Unknown->new();
        }
        if (my $type = $self->content_types->{$varname}) {
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
        $self->content_types;
        if (Type::str($call->identifier) eq 'lookup' && Type::is_str($call->capture->positional->[0])) {
            $self->lookup(Type::str($call->capture->positional->[0]));
        } else {
            Type::Unknown->new();
        }
    }
}
class Type::Lexical extends Type {
    has content=>(is=>'ro',isa=>'Type');
    method method_call($call) {
        if (Type::str($call->rvalue->identifier) eq 'FETCH') {
            $self->content;
        } else {
            Type::Unknown->new();
        }
    }
}
class Type::Unknown extends Type {
}
class Type::MildewSOLoader extends Type {
}
$Mildew::LexicalPreludeType = Type::Scope->new(
    content_types => {
        'MildewSOLoader' => Type::Lexical->new(content=>Type::MildewSOLoader->new()),
    }
);
