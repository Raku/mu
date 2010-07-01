use v5.10;
use lib '../../src/perl6';
use MooseX::Declare;
BEGIN {
    $ENV{'PERL6LIB'} = '../../src/perl6/';
}
{
    # STD needs to be important from the main package
    package main;
    use STD;
}
BEGIN {package main;do 'viv';die $@ if $@};
class Mildew::Frontend::STD {
    use Getopt::Long qw(GetOptionsFromArray);
    has options=>(is=>'ro',default=>sub {{}});
    has debug=>(is=>'rw',default=>0);
    method BUILD {
        my ($trace,$dump,$cflags,$ld_library_path);
        my $debug;
        GetOptionsFromArray(
            ($self->options->{FRONTEND} // []),
            'debug' => \$debug,
        );
        $self->debug($debug);
    }
    use Scalar::Util qw(reftype);
    sub prune {
        my $what = shift;
    
        state %pruned;
        return unless defined $what;
        return if $pruned{$what}++;
    
        if (reftype $what and reftype $what eq 'HASH') {
            delete $what->{'.'};
            delete $what->{BEG};
            delete $what->{END};
            delete $what->{WS};
            delete $what->{MATCH};
            for my $key (keys %{$what}) {
                prune($what->{$key});
            }
        } elsif (reftype $what and reftype $what eq 'ARRAY') {
            for (@{$what}) {
                prune($_);
            }
        } else {
        }
    }
    method parse($source) {
        VIV::SET_OPT('match'=>1,'pos'=>1);
        $ENV{'STD5PREFIX'} = '../../src/perl6/';
        my $m = STD->parse($source, actions=>'Actions');
        if ($self->debug) {
            require YAML::XS;
            prune($m);
            die YAML::XS::Dump($m->{'_ast'}); 
        }
        $m->{'_ast'}->emit_m0ld;
    }
}
