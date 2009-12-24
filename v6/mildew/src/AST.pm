use v5.10;
use MooseX::Declare;
use utf8;
{
package AST;
sub unique_id {
    state $id = 0;
    '$id'.$id++;
}
sub unique_label {
    state $lab = 0;
    'lab'.$lab++;
}
sub indent {
    my $x = shift;
    my $i = shift || 1;
    my $s = '    ' x $i;
    $x =~ s/^/$s/mg;
    $x;
}
sub terminate_stmt {
    my $stmt = shift;
    return $stmt . ";\n" unless $stmt =~ /(\n|;|})$/;
    return $stmt;
}
}
class AST::Base {
    method m0ld($ret) {
        $self->m0ld($ret);
    }
    method pretty {
        use YAML::XS;
        Dump($self);
    }
}

class AST::Loop extends AST::Base {
    has 'code' => (is => 'ro');
    method m0ld($ret) {
        my $label = AST::unique_label;
        $label.':'.($self->code->m0ld($ret))."\n".
        'goto '.$label.';'."\n";
    }
    method pretty {
    return 'loop {'
        . AST::indent($self->code->pretty) . "\n"
        . "}\n";
    }
}

class AST::While extends AST::Base {
    has 'cond' => (is => 'ro');
    has 'body' => (is => 'ro');

    method m0ld {
        my $id_cond = AST::unique_id;
        my $start = AST::unique_label;
        my $label_start = AST::unique_label;
        my $label_end = AST::unique_label;
        my $label_body = AST::unique_label;
    
        $label_start . ":" . $self->cond->m0ld($id_cond) . "\n" .
        'my '.$id_cond.'_val = '.$id_cond.'."FETCH"();'."\n".
        'my '.$id_cond.'_bool = '.$id_cond.'_val."true"();'."\n".
        'if '.$id_cond.'_bool { goto '.$label_body.' } else { goto '.$label_end.' };'."\n".
        $label_body.": " .$self->body->m0ld('$void') ."\n".
        "goto $label_start;\n".
        $label_end.": noop;\n";
    }

    method pretty {
        'while ' . $self->cond->pretty . " {\n"
        . AST::indent($self->body->pretty) . "\n"
        . "}\n";
    }
}

class AST::If extends AST::Base {
    has 'cond' => (is => 'ro');
    has 'then' => (is => 'ro');
    has 'else' => (is => 'ro');
    has 'elsif' => (is => 'ro');
    method m0ld($ret) {
        my $id_cond = AST::unique_id;
        my $label_then = AST::unique_label;
        my $label_else = AST::unique_label;
        my $label_endif = AST::unique_label;
        my $cond = $self->cond->m0ld($id_cond);
        my $then = 'noop;';
        $then = $self->then->m0ld($ret) if $self->then;
        my $else = 'noop;';
        if ($self->else) {
            $else = $self->else->m0ld($ret);
        }
        my $elsifs = '';
        if ($self->elsif) {
            foreach my $part (@{$self->elsif}) {
                my $id_elsif_cond = AST::unique_id;
                my $label_elsif_then = AST::unique_label;
                my $label_elsif_else = AST::unique_label;
                my $elsif_cond = $part->cond->m0ld($id_elsif_cond);
                my $elsif_then = $part->then->m0ld($ret);
                $elsifs .= $elsif_cond."\n".
                  'my '.$id_elsif_cond.'_val = '.$id_elsif_cond.'."FETCH"();'."\n".
                  'my '.$id_elsif_cond.'_bool = '.$id_elsif_cond.'_val."true"();'."\n".
                  'if '.$id_elsif_cond.'_bool { goto '.$label_elsif_then.' } else { goto '.$label_elsif_else.' };'."\n".
                  $label_elsif_then.':'."\n".
                  $elsif_then."\n".
                  'goto '.$label_endif.';'."\n".
                  $label_elsif_else.': noop;'."\n"
            }
        }
    
        $cond."\n".
        'my '.$id_cond.'_val = '.$id_cond.'."FETCH"();'."\n".
        'my '.$id_cond.'_bool = '.$id_cond.'_val."true"();'."\n".
        'if '.$id_cond.'_bool { goto '.$label_then.' } else { goto '.$label_else.' };'."\n".
        $label_then.':'."\n".
        $then."\n".
        'goto '.$label_endif.';'."\n".
        $label_else.':'."\n".
        $elsifs.
        $else."\n".
        $label_endif.': noop;'."\n"
    }
    method pretty {
        my $code;
        if ($self->then) {
            $code =
                'if ' . $self->cond->pretty . " {\n"
                . AST::indent($self->then->pretty) . "\n"
                . "}\n";
        } else {
            $code =
                'unless ' . $self->cond->pretty . " {\n"
                . AST::indent($self->else->pretty) . "\n"
                . "}\n";
        }
        if ($self->elsif) {
            foreach my $part (@{$self->elsif}) {
                $code .=
                  'elsif '.$part->cond->pretty . " {\n"
                    . AST::indent($self->then->pretty). "\n"
                    . "}\n";
            }
        }
        if ($self->else) {
            $code .=
              "else {\n"
                . AST::indent($self->else->pretty). "\n"
                . "}\n";
        }
        $code;
    }
}

class AST::Block extends AST::Base {
    has 'stmts' => (is=>'ro');
    has 'regs' => (is=>'ro',default=>sub {[]});
    has 'hints' => (is=>'ro',default=>sub {{}});
    method m0ld($ret) {
        "my $ret = mold {\n"
            . join('',map {'my $'.$_.";\n"} @{$self->regs})
            . join('',map {'RI($'.$_.",\"".$self->hints->{$_}."\");\n"} keys %{$self->hints})
            . join("",map { $_->m0ld('$void') } @{$self->stmts})
        . "};\n";
    }
    method pretty {
        use Data::Dump::Streamer;
        "mold \{\n". AST::indent(
            join('',map {'my $'.$_.";\n"} @{$self->regs})
            . join("",map {AST::terminate_stmt  $_->pretty } @{$self->stmts})
            #. join("",map { blessed($_) ? terminate_stmt $_->pretty : confess("$_ is not a reference") } @{$self->stmts})
        ) . "\}"
    }
}

 class AST::Comment extends AST::Base {
    has 'comment' => (is=>'ro');
    method m0ld($ret) {
        join("",map {"#".$_."\n"} split(/\n/,$self->comment));
    }
}

class AST::Label extends AST::Base {
    has 'identifier' => (is => 'rw');
    has 'stmt' => (is => 'rw');
}

class AST::Let extends AST::Base {
    has 'block' => (is=>'ro');
    has 'value' => (is=>'ro');
    method m0ld($ret) {
        my $id = AST::unique_id;
        $self->value->m0ld($id) . $self->block->(AST::Reg->new(name => $id))->m0ld($ret);
    }
    method pretty {
        my $id = AST::unique_id;
        "do {\n". AST::indent('my ' . $id . ' = ' . $self->value->pretty . ";\n"
        . $self->block->(AST::Reg->new(name => $id))->pretty) . '}';
    }
}

class AST::Seq extends AST::Base {
    has 'stmts' => (is=>'ro');
    method pretty {
        join("",map {AST::terminate_stmt $_->pretty} @{$self->stmts});
    }
    method m0ld($ret) {
        my @stmts = @{$self->stmts};
        my $last = pop @stmts;
        my $m0ld = join('',map {$_->m0ld(AST::unique_id)} @stmts);
        $m0ld = $m0ld . $last->m0ld($ret) if $last;
        return $m0ld;
    }
}

class AST::Call extends AST::Base {
    use namespace::autoclean;
    use AST::Helpers qw(YYY);
    has 'capture' => (is=>'ro');
    has 'identifier' => (is=>'ro');
    method arguments {
        my @args = @{$self->capture->positional};
        my @named = @{$self->capture->named};
        while (@named) {
            push (@args,AST::Named->new(key=>shift @named,value=>shift @named));
        }
        @args;
    }
    method m0ld($ret) {
        if ($self->capture->isa("AST::Capture")) {
            my $invocant = AST::unique_id;
            my $identifier = AST::unique_id;
    
            my $args = "";
    
            my @args = map {
                my $id = AST::unique_id;
                $args .= $_->m0ld($id);
                $id
            } @{$self->capture->positional};
    
            my @named = @{$self->capture->named};
            while (@named) {
                my $key = AST::unique_id;
                my $value =  AST::unique_id;
                $args .= (shift @named)->m0ld($key);
                $args .= (shift @named)->m0ld($value);
                push(@args,":".$key."(".$value.")");
            }
    
            $self->capture->invocant->m0ld($invocant)
            . $self->identifier->m0ld($identifier)
            . $args 
            . "my $ret = "
            . $invocant . "." . $identifier
            . "(" . join(',',@args) . ")" . ";\n";
        } else {
            die 'unimplemented';
        }
    }
    method pretty {
    
        my $identifier;
        if ($self->identifier->isa("AST::StringConstant")) {
            $identifier = $self->identifier->value;
        } else {
            $identifier = $self->identifier->pretty;
        }
    
        my $args = '';
        my @args = map {$_->pretty} @{$self->capture->positional};
        my @named = @{$self->capture->named};
        while (@named) {
            push(@args,":".(shift @named)->pretty." => ".(shift @named)->pretty);
        }
    
        if ($self->capture->isa("AST::Capture")) {
            YYY($self) unless $self->capture->invocant;
            $self->capture->invocant->pretty . "." . $identifier . (@args ? '(' . join(',',@args) . ')' : '');
        } else {
            $self->SUPER::pretty;
        }
    }
}
class AST::Pair extends AST::Base {
    has 'key' => (is=>'ro');
    has 'value' => (is=>'ro');
    method m0ld {
        die('Pairs are here just to be seen as named arguments, for now.');
    }
    method pretty {
        return ':'.$self->key->pretty.'('.$self->value->pretty.')';
    }
}

class AST::IntegerConstant extends AST::Base {
    has 'value' => (is=>'ro');
    method m0ld($ret) {
        "my $ret = ".$self->value.";\n";
    }
    method pretty {
        $self->value
    }
 }

class AST::StringConstant extends AST::Base {;
    has 'value' => (is=>'ro');
    method m0ld($ret) {
        #XXX metachars
        my $str = $self->value;
        $str =~ s/\\/\\\\/g;
        $str =~ s/"/\\"/g;
        $str =~ s/\n/\\n/g;
        "my $ret = \"".$str."\";\n";
    }
    method pretty {
        #XXX metachars
        '"' . $self->value . '"'
    }
}

class AST::Reg extends AST::Base {
    has 'name' => (is=>'ro');
    method m0ld($ret) {
        "my $ret = ".$self->name.";\n";
    }
    method pretty {
        #XXX metachars
        $self->name;
    }
}

class AST::Capture extends AST::Base {
    has 'invocant' => (is=>'ro');
    has 'positional' => (is=>'ro',default=>sub {[]},isa=>'ArrayRef[AST::Base]');
    has 'named' => (is=>'ro',default=>sub {[]});
    has 'ctx' => (is=>'ro');
}
1;
