use MooseX::Declare;
class VAST::variable {
    use utf8;
    use AST::Helpers;
    method emit_m0ld {
        my $m = shift;
        use YAML::XS;
        my $twigil = $m->{twigil}[0]{SYM} || '';
        if ($twigil eq '!') {
    	call('postcircumfix:{ }'=> FETCH(call('^!instance_storage'=>
    				   FETCH(lookup('$¿self')))),
                 [call 'postcircumfix:( )' => FETCH(call('postcircumfix:{ }' => lookupf('PRIMITIVES::'),[string '&storage_name'])) => [capturize([lookupf('$?CLASS'),string varname($m)])]]);
        } elsif ($twigil eq '.') {
            call($m->{desigilname}{longname}->canonical,FETCH(lookup('$¿self')));
        } elsif ($self->{desigilname}{longname}) {
            my @name = $self->{desigilname}{longname}->components;
            my $name = pop(@name);
            $name = ($self->{sigil}{TEXT} || '').($self->{twigil}[0]{TEXT} || '') .$name;
            if (scalar @name) {
                $name = call('postcircumfix:{ }'=>FETCH(lookup_package(@name)),[string($name)]);
            } else {
                lookup($name);
            }
        } elsif ($self->{sublongname}) {
            my ($name) = $self->{sublongname}->components;
            lookup(($self->{sigil}{TEXT} || '').($self->{twigil}[0]{TEXT} || '').$name);
        } else {
            use YAML::XS;
            die Dump($self);

            #XXX;
        }
    }
}

1;
