use v5.10;
use MooseX::Declare;
use utf8;
{
package AST;
sub unique_id {
    state $id = 0;
    '$id'.$id++;
}
sub unique_reg {
    AST::Reg->new(name=>unique_id);
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
    if ($stmt =~ /;|}$/) {
        $stmt . "\n";
    } elsif ($stmt =~ /\n$/) {
        $stmt;
    } else {
        $stmt . ";\n";
    }
}
}
class AST::Base {
    use YAML::XS;
    method pretty {
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
    method simplified {
        use AST::Helpers;
        use Scalar::Util qw(weaken);
        my $goto = AST::Goto->new();
        my $block = AST::Seq->new(id=>AST::unique_label,stmts=>[$self->code->simplified,$goto]);
        $goto->block($block);
    }
    method pretty {
        return "loop {\n"
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


class AST::Block extends AST::Base {
    has 'stmts' => (is=>'ro');
    has 'regs' => (is=>'ro',default=>sub {[]});
    #has 'hints' => (is=>'ro',default=>sub {{}});
    method m0ld($ret) {
        "my $ret = mold {\n"
            . join('',map {'my $'.$_.";\n"} @{$self->regs})
#            . join('',map {'RI($'.$_.",\"".$self->hints->{$_}."\");\n"} keys %{$self->hints})
            . join("",map { $_->m0ld('$void') } @{$self->stmts})
        . "};\n";
    }
    method pretty {
        "mold \{\n". AST::indent(
            join('',map {'my $'.$_.";\n"} @{$self->regs})
            . join("",map {AST::terminate_stmt  $_->pretty } @{$self->stmts})
        ) . "\}"
    }
    method simplified {
        my @stmts;
        my $value;
        for (@{$self->stmts}) {
            my @side_effects;
            ($value,@side_effects) = $_->simplified;
            push (@stmts,@side_effects);
        }
        AST::Block->new(regs=>$self->regs,stmts=>[@stmts,$value]);
    }
}

 class AST::Comment extends AST::Base {
    has 'comment' => (is=>'ro');
    method m0ld($ret) {
        join("",map {"#".$_."\n"} split(/\n/,$self->comment));
    }
}



class AST::Seq extends AST::Base {
    has 'stmts' => (is=>'ro');
    has 'id' => (is=>'ro');
    method pretty {
        (defined($self->id) ? $self->id . ": " : '')
        . join("",map {AST::terminate_stmt $_->pretty} @{$self->stmts});
    }
    method m0ld($ret) {
        my @stmts = @{$self->stmts};
        my $last = pop @stmts;
        my $m0ld = join('',map {$_->m0ld(AST::unique_id)} @stmts);
        $m0ld = $m0ld . $last->m0ld($ret) if $last;
        return $m0ld;
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
    method simplified {
        $self;
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
    method simplified {
        $self;
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
    method simplified {
        $self;
    }
}

class AST::Capture extends AST::Base {
    has 'invocant' => (is=>'ro');
    has 'positional' => (is=>'ro',default=>sub {[]},isa=>'ArrayRef[AST::Base]');
    has 'named' => (is=>'ro',default=>sub {[]});
    has 'ctx' => (is=>'ro');
}

# lowlevel AST nodes

class AST::Goto extends AST::Base {
    has 'block' => (is => 'rw');
    method pretty {
        "goto ".$self->block->id;
    }
}
class AST::Branch extends AST::Base {
    has 'cond' => (is=>'ro');
    has 'then' => (is=>'ro');
    has 'else' => (is=>'ro');
    method pretty {
        "if "
        . $self->cond->pretty
        . " {goto " . $self->then->id . "} else {goto " . $self->else->id . "}";
    }
}
1;
