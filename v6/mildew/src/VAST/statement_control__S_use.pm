use MooseX::Declare;
class VAST::statement_control__S_use {
    use AST::Helpers;
    method emit_m0ld {
        my $module = $self->{module_name}{longname};
        if ($self->{version}) {
            # use v6
        } elsif ($module
                 && $module->{colonpair}[0]
                 && $module->{colonpair}[0]{identifier}{TEXT} eq 'from'
                 && $module->{colonpair}[0]{postcircumfix}{nibble}{nibbles}[0] eq 'perl5') {
            my $name = join '::',$module->{name}{identifier}{TEXT},map {
                $_->{identifier}[0]{TEXT}
            } @{$module->{name}{morename}};

            call('BIND', call('postcircumfix:{ }' => reg '$scope', [string $name]),
                 [ call('postcircumfix:( )' =>
                    FETCH(call('postcircumfix:{ }' => FETCH(lookup('EXTERNAL::')), [string '&use_from_perl5'])),
                    [capturize([string $name])]) ]);
        } elsif ($module) {
            my $name = $module->{name}{identifier}{TEXT};
            call(EXPORTALL => FETCH(call('BIND'=> curlies($name.'::'),[call lookup=>FETCH(call(load => FETCH(call(new => lookupf('ModuleLoader'))),[string ($name),lookupf('$LexicalPrelude')])),[string($name.'::')]])),[reg '$scope']);
        } else {
            XXX;
        }
    }
}
