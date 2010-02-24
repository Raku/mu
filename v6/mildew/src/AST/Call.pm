use v5.10;
use MooseX::Declare;
class AST::Call extends AST::Base {
    use namespace::autoclean;
    use AST::Helpers qw(YYY);
    has 'capture' => (is=>'ro');
    has 'identifier' => (is=>'ro');
    #TODO delete
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
    method simplified {
        if ($self->capture->isa("AST::Capture")) {

            my @setup_args;
            my @simplified_pos = map {
                my ($pos,@pos_setup) = $_->simplified;
                push(@setup_args,@pos_setup);
                $pos;
            } @{$self->capture->positional};
    
            my @simplified_named;
            my @named = @{$self->capture->named};
            while (@named) {
                my ($key,@setup_key) = (shift @named)->simplified;
                my ($value,@setup_value) = (shift @named)->simplified;
                push(@setup_args,@setup_key,@setup_value);
                push(@simplified_named,$key,$value);
            }

            my ($invocant,@invocant_setup) = $self->capture->invocant->simplified;
            my ($identifier,@identifier_setup) = $self->identifier->simplified; 
            my $reg = AST::unique_reg;
            ($reg,@invocant_setup,@identifier_setup,@setup_args,AST::Assign->new(lvalue=>$reg,rvalue=>
                AST::Call->new(identifier=>$identifier,capture=>AST::Capture->new(
                    invocant=>$invocant,
                    positional=>[@simplified_pos],
                    named=>[@simplified_named]
                )),
            ));
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
    method forest {
        Forest::Tree->new(node=>$self->pretty,children=>[map {$_->forest} ($self->capture->invocant,$self->identifier,@{$self->capture->positional},@{$self->capture->named})]);
    }
}
