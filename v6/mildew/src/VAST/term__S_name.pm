use v5.10;
use MooseX::Declare;
use utf8;
class VAST::term__S_name {
    use AST::Helpers;
    method emit_m0ld {


	my $name = $self->{longname}->{name};
	if ($name->{morename} and !$name->{identifier}) {
            my @name = map {$_->{identifier}[0]{TEXT}} @{$name->{morename}};
            my $thing;
            if (scalar @name > 1) {
                my $name = pop(@name);
                call('postcircumfix:{ }'=>FETCH(lookup_package(@name)),[string($name)]);
            } else {
                lookup($name[-1]);
            }

        } elsif ($self->{args}) {
            my @name = $self->{longname}->components;
            my $func;
            if (scalar @name > 1) {
                my $var = pop(@name);
                $func = call('postcircumfix:{ }'=>FETCH(lookup_package(@name)),[string('&'.$var)]);
            } else {
                $func = lookup('&'.$name[-1]);
            }
            fcall FETCH($func) => named_and_positional($self->{args}->emit_m0ld);
        } else {
            my $pkg;
            if ($name->{identifier}{TEXT} eq 'CALLER') {
                $pkg = call new => reg '¢SMOP__S1P__FlattenedScope',
                [ call lexical => (call back => (call continuation => reg '$interpreter')) ];
            } elsif ($name->{identifier}{TEXT} eq 'MY') {
                $pkg = call new => reg '¢SMOP__S1P__FlattenedScope',
                [ reg '$scope' ];
            } else {
                XXX;
            }
            if ($self->{postcircumfix} && @{$self->{postcircumfix}}) {
                call('postcircumfix:{ }'=>FETCH($pkg),[string($self->{postcircumfix}[0]{nibble}->Str)]);
            } else {
                $pkg;
            }
        } 
    }
}
