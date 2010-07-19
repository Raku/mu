use v5.10;
use MooseX::Declare;
use Regexp::Grammars;
use Mildew::AST;
class Mildew::Frontend::M0ld {
    # $REGS needs to be a global variable for local to work
    our $REGS = [];

    # converts an arrayref of statements to a Mildew::AST::Block
    sub stmts_to_block {
        my ($stmts) = @_;
        my @stmts;

        my %seqs;
        for my $stmt_label (@{$stmts}) {
            for my $label (@{$stmt_label->[0]}) {
                if ($seqs{$label}) {
                    die "Duplicate label $label.\n";
                }
                $seqs{$label} = Mildew::AST::Seq->new(stmts=>[],id=>$label);
            }
        }
        my @seqs = ();
        for my $stmt_label (@{$stmts}) {
            my $labels = $stmt_label->[0];
            for my $label (@{$stmt_label->[0]}) {
                push(@seqs,$seqs{$label});
            }
            my $stmt = $stmt_label->[1];
            unless (@seqs) {
                push(@seqs,Mildew::AST::Seq->new(stmts=>[])) if $stmt;
            }
            if (!defined $stmt) {
            } elsif ($stmt->{goto}) {
                push(@{$seqs[-1]->stmts},Mildew::AST::Goto->new(block=>$seqs{$stmt->{goto}}));
            } elsif ($stmt->{cond}) {
                push(@{$seqs[-1]->stmts},Mildew::AST::Branch->new(
                    cond=>$stmt->{cond},
                    then=>$seqs{$stmt->{then}},
                    else=>$seqs{$stmt->{else}}
                ));
            } elsif ($stmt) {
                use Data::Dumper;
                push(@{$seqs[-1]->stmts},$stmt);
            }
        }
        Mildew::AST::Block::Simplified->new(stmts=>\@seqs,regs=>$REGS);
    }


    method parse($source) {
        # working around a bug in Regexp::Grammars by creating a new parser every time
    my $parser = qr/
    ^<top>$
    <rule: top>
    (?{ local $REGS=[] })
    (?: <[stmt_with_labels]> ; )*
    (?{ $MATCH = stmts_to_block($MATCH{stmt_with_labels}) })
    
    <token: stmt_with_labels>
    (<[label]> <.ws>? \: <.ws>?)* <stmt>
    (?{ $MATCH = [$MATCH{label},$MATCH{stmt}] })

    <token: ws>
    (?> (?: \s+ | \#[^\n]* )*)
    
    <token: stmt>
    (?: <MATCH=goto>|<MATCH=br>|<MATCH=assign>|<MATCH=decl>|<MATCH=noop>)
    
    <rule: decl>
    my <register>
    (?{ my $reg = substr($MATCH{register}->name,1);push(@{$REGS},$reg);$MATCH = undef })
    
    <rule: call>
    <invocant=value> \.  <identifier=value>
    \(
    (?:<[argument]> ** ,)?
    \)
    (?{ 
        $MATCH = Mildew::AST::Call->new(
            identifier=>$MATCH{identifier},
            capture=>Mildew::AST::Capture->new(
                invocant => $MATCH{invocant},
                positional => [grep { ! $_->isa('Mildew::AST::Pair') } @{$MATCH{argument}}],
                named => [map { $_->key, $_->value } grep { $_->isa('Mildew::AST::Pair') } @{$MATCH{argument}}]
            )
        )
    })
    
    <rule: assign>
    (?:my)? <register> = <rvalue>
    (?{ $MATCH = Mildew::AST::Assign->new(lvalue=>$MATCH{register},rvalue=>$MATCH{rvalue}) })
    <token: rvalue>
    <MATCH=call> | <MATCH=value>
    
    <token: goto>
    goto \s+ <label>
    (?{$MATCH = {goto=>$MATCH{label}}})
    
    <token: argument>
    (?: <MATCH=named_argument> | <MATCH=value> )
    
    <rule: named_argument>
    (?: \: <key=value> \( <val=value> \) )
    (?{$MATCH = Mildew::AST::Pair->new(key=>$MATCH{key},value=>$MATCH{val})})
    
    
    <token: noop>
    noop
    (?{$MATCH = undef})
    
    <rule: br>
    if <value> <then=branch> else <else=branch>
    (?{$MATCH = {cond=>$MATCH{value},then=>$MATCH{then},else=>$MATCH{else}}})
    
    <rule: branch>
    \{ goto <label> ;? \}
    (?{$MATCH = $MATCH{label}})
    
    <token: label>
    \w+
    
    <token: value>
    (?: <MATCH=integer>  | <MATCH=register> | <MATCH=string> | <MATCH=submold>)
    
    <token: integer>
    (\d+)
    (?{$MATCH = Mildew::AST::IntegerConstant->new(value=>$+)})
    
    <token: register>
    ((?> (?: \$ | \? | ¢) \p{IsAlpha} \w*))
    (?{$MATCH = Mildew::AST::Reg->new(name=>$+)})
    
    <token: string_part>
    (?:
    \\(.) (?{my %meta = (n=>"\n");$MATCH = ($meta{$^N} || $^N)}) |
    ([^\\"])  (?{$MATCH = $^N}) )
    <token: string>
    " (<[string_part]>*) "
    (?{$MATCH = Mildew::AST::StringConstant->new(value=>join('',@{$MATCH{string_part}}))})
    
    <rule: submold>
    (?{local $REGS = []})
    mold \{
    (?: <[stmt_with_labels]> ; )*
    \}
    (?{$MATCH = stmts_to_block($MATCH{stmt_with_labels});})
    /x;
        unless ($source =~ $parser) {
            die "Can't parse m0ld code";
        }
        $/{top};
    }
}
