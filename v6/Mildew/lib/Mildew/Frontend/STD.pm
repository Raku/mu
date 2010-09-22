use v5.10;
use MooseX::Declare;
{
    # STD needs to be important from the main package
    package main;
    use STD;
    use STD::Actions;
}
{
    package
        CursorBase;
    no warnings 'redefine';
    BEGIN { *sys_real_load_modinfo = *sys_load_modinfo; }
    sub sys_load_modinfo {
        if ($_[1] eq 'adhoc-signatures') { return undef; }
        goto &sys_real_load_modinfo;
    }

    # Possibly worth folding back
    sub STD::Actions::gen_class {
        my $class = shift;
        my $base = shift;
        #say $class;
        no strict 'refs';
        if ($STD::Actions::GENCLASS{$class}) {
            return;
        }
        if (@{$class . '::ISA'} && join('|', @{$class . '::ISA'}) ne 'Moose::Object' ) {
            $STD::Actions::GENCLASS{$class} = 1;
            return;
        }
        if (!$base && $class =~ /(.*)__S_/) {
            $base = $1;
            STD::Actions::gen_class($base);
        } elsif ($base) {
            STD::Actions::gen_class($base);
        } else {
            $base = 'VAST::Base';
        }
        #say "using $base";
        $STD::Actions::GENCLASS{$class} = $base;
        @{$class . '::ISA'} = $base;
    }

    { package VAST::Additive; }
    { package VAST::Autoincrement; }
    { package VAST::Base; }
    { package VAST::Chaining; }
    { package VAST::Comma; }
    { package VAST::Concatenation; }
    { package VAST::Conditional; }
    { package VAST::Exponentiation; }
    { package VAST::Item_assignment; }
    { package VAST::Junctive_and; }
    { package VAST::Junctive_or; }
    { package VAST::List_assignment; }
    { package VAST::List_infix; }
    { package VAST::List_prefix; }
    { package VAST::Loose_and; }
    { package VAST::Loose_or; }
    { package VAST::Loose_unary; }
    { package VAST::Methodcall; }
    { package VAST::Multiplicative; }
    { package VAST::Named_unary; }
    { package VAST::Replication; }
    { package VAST::Sequencer; }
    { package VAST::Structural_infix; }
    { package VAST::Symbolic_unary; }
    { package VAST::Term; }
    { package VAST::Tight_and; }
    { package VAST::Tight_or; }
}
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
    method _build_PERL6LIB {
        require Mildew::Setting::SMOP;
        Mildew::Setting::SMOP::standard_libs();
    }
    method parse($source) {
        $::ORIG = $source;
        local $ENV{'PERL6LIB'} = $self->PERL6LIB; 
        mkdir($self->tmp);
        my $m = STD->parse($source, actions=>'STD::Actions',tmp_prefix=>$self->tmp,syml_search_path=>$self->syml_search_path,setting=>$self->setting,filename=>md4_hex($source));
        local $ENV{'DEFAULT_SETTING_FOR_MODULES'} = 'MildewCORE';


        if ($self->debug) {
            require YAML::XS;
            prune($m);
            die YAML::XS::Dump($m->{'_ast'}); 
        }
        $m->{'_ast'}->emit_m0ld;
    }
}
