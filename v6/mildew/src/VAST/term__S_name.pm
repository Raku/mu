use v5.10;
use MooseX::Declare;
use utf8;
class VAST::term__S_name {
    use AST::Helpers;
    method emit_m0ld {
        # unify name handling
	my $name = $self->{longname}->{name};
	if ($name->{morename} and !$name->{identifier}) {
            my @name = map {$_->{identifier}[0]{TEXT}} @{$name->{morename}};
            if (scalar @name > 1) {
                my $name = lookup($name[0].'::');
                for my $part (@name[1..-2]) {
                    $name = call('postcircumfix:{ }'=>FETCH($name),[string($part.'::')]);
                }
                 call('postcircumfix:{ }'=>FETCH($name),[string($name[-1])]);
            } else {
                lookup($name[-1]);
            }
        }
    }
}
