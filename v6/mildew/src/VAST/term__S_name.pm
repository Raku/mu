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
        } elsif ($name->{identifier}{TEXT} eq 'CALLER') {
	    call new => reg '¢SMOP__S1P__FlattenedScope',
	      [ call lexical => (call back => (call continuation => reg '$interpreter')) ];
        } elsif ($name->{identifier}{TEXT} eq 'MY') {
	    call new => reg '¢SMOP__S1P__FlattenedScope',
	      [ reg '$scope' ];

        } elsif ($self->{args}) {
            my @name = name_components($self->{longname}{name});
            use YAML::XS;
            #die Dump(@name);
            my $func;
            if (scalar @name > 1) {
                $func = lookup($name[0].'::');
                for my $part (@name[1..-2]) {
                    $func = call('postcircumfix:{ }'=>FETCH($func),[string($part.'::')]);
                }
                $func = call('postcircumfix:{ }'=>FETCH($func),[string($name[-1])]);
            } else {
                $func = lookup($name[-1]);
            }
            #$func;
            fcall $func => named_and_positional($self->{args}->emit_m0ld);
        } else {
            XXX;
        }
    }
}
