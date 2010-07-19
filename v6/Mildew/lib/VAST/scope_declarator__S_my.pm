use v5.10;
use MooseX::Declare;
class VAST::scope_declarator__S_my {
    use Mildew::AST::Helpers;

    method emit_m0ld {
        #XXX refactor
        if (my $decl = $self->{scoped}{declarator}) {
            if (my $var_decl = $decl->{variable_declarator}) {

                # so we can compare output with the prerefactor version
                let call(new => FETCH(lookup 'Scalar')),sub {
                    call(BIND => curlies(varname($var_decl->{variable}))
                    ,[$_[0]]);
                };


                # the proper way
                #call(BIND => curlies(varname($var_decl->{variable}))
                #    ,[call(new => FETCH(lookup 'Scalar'))]);

            } elsif (my $routine_decl = $decl->{routine_declarator}) {
		$routine_decl->{routine_def}->emit_m0ld('my');
            } else {
                use YAML::XS;
                XXX('unknown scope declarator');
            }
        } elsif (my $multi = $self->{scoped}{multi_declarator}) {
	    $multi->emit_m0ld('my');
        } elsif ($self->{scoped}{package_declarator}) {
            $self->{scoped}{package_declarator}->emit_m0ld;
        } else {
            XXX('scoped declarator without a recognized declarator');
        }
    }
}
