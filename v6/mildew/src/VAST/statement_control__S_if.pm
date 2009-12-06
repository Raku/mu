use v5.10;
use MooseX::Declare;
class VAST::statement_control__S_if {
    use AST::Helpers;
    method emit_m0ld {
        my $then = call 'postcircumfix:( )' => code($self->{xblock}{pblock}{blockoid}),[capturize];
        my $else = lookupf("False");
        if (ref $self->{else} eq 'ARRAY' &&
            blessed $self->{else}[0] &&
            ref $self->{else}[0] &&
            ref $self->{else}[0]{blockoid}) {

            $else = call 'postcircumfix:( )' => code($self->{else}[0]{blockoid}),[capturize];
        }

        my @elsif;
        if (ref $self->{elsif} eq 'ARRAY') {
            foreach my $elsif_part (@{$self->{elsif}}) {

                my $elsif = call 'postcircumfix:( )' => code($elsif_part->{pblock}{blockoid}),[capturize];

                push @elsif, AST::If->new
                  ( cond => $elsif_part->{EXPR}->emit_m0ld,
                    then => $elsif );
            }
        }

        AST::If->new
            ( cond => $self->{xblock}{EXPR}->emit_m0ld,
              then => $then,
              else => $else,
              elsif => \@elsif );
    }
}
