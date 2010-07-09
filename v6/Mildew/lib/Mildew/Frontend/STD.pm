use v5.10;
use MooseX::Declare;
{
    # STD needs to be important from the main package
    package main;
    use STD;
}
BEGIN {package main;require 'viv'};
class Mildew::Frontend::STD {
    use Getopt::Long qw(GetOptionsFromArray);
    use Mildew::Setting::SMOP;
    use Digest::MD4 qw(md4_hex);
    has options=>(is=>'ro',default=>sub {{}});
    has debug=>(is=>'rw',default=>0);
    has setting=>(is=>'rw',default=>'MildewCORE');
    has tmp=>(is=>'rw',default=>'/tmp/mildew/');
    method BUILD {
        my ($debug,$setting,$tmp);
        GetOptionsFromArray(
            ($self->options->{FRONTEND} // []),
            'debug' => \$debug,
            'setting=s' => \$setting,
            'tmp=s' => \$tmp,
        );
        $self->debug($debug);
        $self->setting($setting) if $setting;
        $self->tmp($tmp.'/') if $tmp;
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
        $::ORIG = $source;
        mkdir($self->tmp);
        my $m = STD->parse($source, actions=>'Actions',tmp_prefix=>$self->tmp,syml_search_path=>[$self->tmp,Mildew::Setting::SMOP::std_tmp_files_path.'/'],setting=>$self->setting,filename=>md4_hex($source));

        if ($self->debug) {
            require YAML::XS;
            prune($m);
            die YAML::XS::Dump($m->{'_ast'}); 
        }
        $m->{'_ast'}->emit_m0ld;
    }
}
