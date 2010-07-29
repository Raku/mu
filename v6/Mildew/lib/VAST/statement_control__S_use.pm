use utf8;
use v5.10;
use MooseX::Declare;
class VAST::statement_control__S_use {
    use Mildew::AST::Helpers;
    method emit_m0ld {
        use YAML::XS;
        my $module = $self->{module_name}{longname};
        if ($module->{name}{identifier}{TEXT} eq 'adhoc-signatures') {
            $Mildew::adhoc_sig = 1; 
            ();
        } elsif ($module->{name}{identifier}{TEXT} eq 'v6-mildew') {
            ();
        } elsif ($self->{version}) {
            # use v6
        } elsif ($module
                 && $module->{colonpair}[0]
                 && $module->{colonpair}[0]{k} eq 'from'
                 && $module->{colonpair}[0]{v}{nibble}->Str eq 'perl5') {
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
