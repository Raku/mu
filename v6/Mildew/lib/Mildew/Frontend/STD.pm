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
    use Digest::MD4 qw(md4_hex);
    has options=>(is=>'ro',default=>sub {{}});
    has debug=>(is=>'rw',default=>0);
    has setting=>(is=>'rw',default=>'MildewCORE');
    has tmp=>(is=>'rw',default=>'/tmp/mildew/');
    has syml_search_path=>(lazy_build=>1,is=>'rw');
    has PERL6LIB=>(lazy_build=>1,is=>'rw');
    method BUILD {
        my ($debug,$setting,$tmp,$syml_search_path,$PERL6LIB);
        GetOptionsFromArray(
            ($self->options->{FRONTEND} // []),
            'debug' => \$debug,
            'setting=s' => \$setting,
            'tmp=s' => \$tmp,
            'syml-search-path=s' => \$syml_search_path,
            'PERL6LIB=s' => \$PERL6LIB,
        );
        $self->debug($debug);
        $self->setting($setting) if $setting;
        $self->tmp($tmp.'/') if $tmp;
        $self->syml_search_path([map {$_.'/'} split(':',$syml_search_path)]) if $syml_search_path;
        $self->PERL6LIB($PERL6LIB) if $PERL6LIB;
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
    method _build_syml_search_path {
        require Mildew::Setting::SMOP;
        [$self->tmp,Mildew::Setting::SMOP::std_tmp_files_path().'/']
    }
    method _build_syml_search_path {
        require Mildew::Setting::SMOP;
        [$self->tmp,Mildew::Setting::SMOP::std_tmp_files_path().'/']
    }
    method _build_PERL6LIB {
        require Mildew::Setting::SMOP;
        Mildew::Setting::SMOP::standard_libs();
    }
    method parse($source) {
        $::ORIG = $source;
        local $ENV{'PERL6LIB'} = $self->PERL6LIB; 
        mkdir($self->tmp);
        my $m = STD->parse($source, actions=>'Actions',tmp_prefix=>$self->tmp,syml_search_path=>$self->syml_search_path,setting=>$self->setting,filename=>md4_hex($source));


        if ($self->debug) {
            require YAML::XS;
            prune($m);
            die YAML::XS::Dump($m->{'_ast'}); 
        }
        $m->{'_ast'}->emit_m0ld;
    }
}
