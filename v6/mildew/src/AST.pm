{
package AST;
my $id=0;
sub unique_id {
    '$id'.$id++;
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
package AST::Block;
use Moose;
extends 'AST::Base';
has 'stmts' => (is=>'ro');
has 'regs' => (is=>'ro',default=>sub {[]});
sub m0ld {
    my ($self,$ret) = @_;
    use YAML::XS;
    "my $ret = mold {\n"
        . join('',map {'my $'.$_.";\n"} @{$self->regs})
        . join("",map { $_->emit_('$void') } @{$self->stmts})
    . "};\n";
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

=for comment
class Call is Emit {
    has $.capture;
    has $.identifier;
    method m0ld($ret) {
        if $.capture.isa(Capture) {
            $.capture.invocant.emit ~ '.' ~ $.identifier.emit ~ '()' 
        }
    }
}
=cut

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

package AST::Call;
use Moose;
extends 'AST::Base';
has 'capture' => (is=>'ro');
has 'identifier' => (is=>'ro');
sub m0ld {
    my ($self,$ret) = @_;
    if ($self->capture->isa("AST::Capture")) {
        my @args = @{$self->capture->positional};
        my @named = @{$self->capture->named};
        while (@named) {
            push (@args,AST::Named->new(key=>shift @named,value=>shift @named));
        }
        "my $ret = "
        . $self->capture->invocant->emit
        . "." . $self->identifier->emit
        . "(" . join('', map {$_->emit} @args) . ");\n";
    } else {
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

package AST::StringConstant;
use Moose;
extends 'AST::Base';
has 'value' => (is=>'ro');
sub m0ld {
    my ($self,$ret) = @_;
    "my $ret = \"".$self->value."\";\n";
}

package AST::Reg;
use Moose;
extends 'AST::Base';
has 'name' => (is=>'ro');
sub emit {
    my $self = shift;
    return '$'.$self->name;
}
sub m0ld {
    die "method m0ld is not supported on AST::Reg, m0ld doesn't support register aliasing\n"
}

package AST::Capture;
use Moose;
has 'invocant' => (is=>'ro');
has 'positional' => (is=>'ro',default=>sub {[]});
has 'named' => (is=>'ro',default=>sub {[]});
has 'ctx' => (is=>'ro');
1;
