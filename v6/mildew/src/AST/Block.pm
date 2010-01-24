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
