{
package AST;
my $id=0;
sub unique_id {
    '$id'.$id++;
}
my $lab = 0;
sub unique_label {
    'lab'.$lab++;
}
sub indent {
    my $x = shift;
    my $i = shift || 1;
    my $s = '    ' x $i;
    $x =~ s/^/$s/mg;
    $x;
}
}
package AST::Base;
use Moose;
sub emit {
    my $self = shift;
    my $id = AST::unique_id;
    $AST::CODE .= do {local $AST::CODE='';$AST::CODE . $self->m0ld($id)};
    return $id;

}
sub emit_ {
    local $AST::CODE = '';
    my ($self,$ret) = @_;
    my $mold = $self->m0ld($ret);
    $AST::CODE . $mold;
}
sub pretty {
    use YAML::XS;
    my $yaml = Dump($_[0]);
    $yaml;
}

package AST::If;
use Moose;
extends 'AST::Base';
has 'cond' => (is => 'ro');
has 'then' => (is => 'ro');
has 'else' => (is => 'ro');
has 'elsif' => (is => 'ro');
sub m0ld {
    my ($self) = @_;
    my $id_cond = AST::unique_id;
    my $id_then = AST::unique_id;
    my $id_else = AST::unique_id;
    my $label_then = AST::unique_label;
    my $label_else = AST::unique_label;
    my $label_endif = AST::unique_label;
    my $cond = $self->cond->m0ld($id_cond);
    my $then = $self->then->m0ld($id_then);
    my $else = 'noop;';
    if ($self->else) {
        $else = $self->else->m0ld($id_else);
    }
    my $elsifs = '';
    if ($self->elsif) {
        foreach my $part (@{$self->{elsif}}) {
            my $id_elsif_cond = AST::unique_id;
            my $id_elsif_then = AST::unique_id;
            my $label_elsif_then = AST::unique_label;
            my $label_elsif_else = AST::unique_label;
            my $elsif_cond = $part->cond->m0ld($id_elsif_cond);
            my $elsif_then = $part->then->m0ld($id_elsif_then);
            $elsifs .= $elsif_cond.$/.
              'my '.$id_elsif_cond.'_val = '.$id_elsif_cond.'."FETCH"();'.$/.
              'my '.$id_elsif_cond.'_bool = '.$id_elsif_cond.'_val."true"();'.$/.
              'if '.$id_elsif_cond.'_bool { goto '.$label_elsif_then.' } else { goto '.$label_elsif_else.' };'.$/.
              $label_elsif_then.':'.$/.
              $elsif_then.$/.
              'goto '.$label_endif.';'.$/.
              $label_elsif_else.': noop;'.$/
        }
    }

    $cond.$/.
    'my '.$id_cond.'_val = '.$id_cond.'."FETCH"();'.$/.
    'my '.$id_cond.'_bool = '.$id_cond.'_val."true"();'.$/.
    'if '.$id_cond.'_bool { goto '.$label_then.' } else { goto '.$label_else.' };'.$/.
    $label_then.':'.$/.
    $then.$/.
    'goto '.$label_endif.';'.$/.
    $label_else.':'.$/.
    $elsifs.
    $else.$/.
    $label_endif.': noop;'.$/
}
sub pretty {
    my ($self) = @_;
    'if ' . $self->cond->pretty . " {\n"
        . AST::indent($self->then->pretty) . "\n"
    . "}\n"
}

package AST::Block;
use Moose;
extends 'AST::Base';
has 'stmts' => (is=>'ro');
has 'regs' => (is=>'ro',default=>sub {[]});
sub m0ld {
    my ($self,$ret) = @_;
    "my $ret = mold {\n"
        . join('',map {'my $'.$_.";\n"} @{$self->regs})
        . join("",map { $_->emit_('$void') } @{$self->stmts})
    . "};\n";
}
sub terminate_stmt {
    my $stmt = shift;
    return $stmt . ";\n" unless $stmt =~ /(\n|;|})$/;
    return $stmt;
}
sub pretty {
    use Data::Dump::Streamer;
    my $self = shift;
    "mold \{\n". AST::indent(
        join('',map {'my $'.$_.";\n"} @{$self->regs})
        . join("",map { terminate_stmt $_->pretty } @{$self->stmts})
    ) . "\}"
}

package AST::Comment;
use Moose;
extends 'AST::Base';
has 'comment' => (is=>'ro');
sub m0ld {
    my ($self,$ret) = @_;
    join("",map {"#".$_."\n"} split(/\n/,$self->comment));
}

package AST::Label;
use Moose;
extends 'AST::Base';
has 'identifier';
has 'stmt';

package AST::List;
use Moose;
extends 'AST::Base';
has 'elements' => (is=>'ro');

sub m0ld {
    my ($self, $ret) = @_;
    my @args;
    my $code;
    for (@{$self->elements}) {
        my $id = AST::unique_id();
        $code .= $_->m0ld($id);
        push @args, $id;
    }
    $code .= 'my '.$ret.' = ?SMOP__S1P__Array."new"('.join(',',@args).');'.$/;
}

package AST::Named;
use Moose;
extends 'AST::Base';
has 'key' => (is=>'ro');
has 'value' => (is=>'ro');

sub emit {
    my $self = shift;
    return ":".$self->key->emit."(".$self->value->emit.")";
}
sub m0ld {
    die "method m0ld is not supported on AST::Named\n"
}

sub pretty {
    my $self = shift;
    $self->key->pretty . " => " . $self->value->pretty;
}

package AST::Call;
use Moose;
extends 'AST::Base';
has 'capture' => (is=>'ro');
has 'identifier' => (is=>'ro');
sub arguments {
    my $self = shift;
    my @args = @{$self->capture->positional};
    my @named = @{$self->capture->named};
    while (@named) {
        push (@args,AST::Named->new(key=>shift @named,value=>shift @named));
    }
    @args;
}
sub m0ld {
    my ($self,$ret) = @_;
    if ($self->capture->isa("AST::Capture")) {
        "my $ret = "
        . $self->capture->invocant->emit
        . "." . $self->identifier->emit
        . "(" . join(',', map {$_->emit} $self->arguments) . ")" . ";\n";
    } else {
        die 'unimplemented';
    }
}
sub pretty {
    my $self = shift;

    my $identifier;
    if ($self->identifier->isa("AST::StringConstant")) {
        $identifier = $self->identifier->value;
    } else {
        $identifier = $self->identifier->pretty;
    }

    my $arguments = '';
    if (my @arguments = $self->arguments) {
        $arguments = "(" . join(',',map {$_->pretty} $self->arguments) . ")";
    }

    if ($self->capture->isa("AST::Capture")) {
        $self->capture->invocant->pretty . "." . $identifier .  $arguments;
    } else {
        $self->SUPER::pretty;
    }

}

package AST::IntegerConstant;
use Moose;
has 'value' => (is=>'ro');
extends 'AST::Base';
sub m0ld {
    my ($self,$ret) = @_;
    "my $ret = ".$self->value.";\n";
}
sub pretty {
    my $self = shift;
    $self->value
}

package AST::StringConstant;
use Moose;
extends 'AST::Base';
has 'value' => (is=>'ro');
sub m0ld {
    my ($self,$ret) = @_;
    #XXX metachars
    "my $ret = \"".$self->value."\";\n";
}
sub pretty {
    #XXX metachars
    my $self = shift;
    '"' . $self->value . '"'
}

package AST::Reg;
use Moose;
extends 'AST::Base';
has 'name' => (is=>'ro');
sub emit {
    my $self = shift;
    $self->name;
}
sub m0ld {
    my ($self,$ret) = @_;
    "my $ret = ".$self->name.";\n";
}
sub pretty {
    #XXX metachars
    my $self = shift;
    $self->name;
}

package AST::Capture;
use Moose;
has 'invocant' => (is=>'ro');
has 'positional' => (is=>'ro',default=>sub {[]});
has 'named' => (is=>'ro',default=>sub {[]});
has 'ctx' => (is=>'ro');
1;
