package MyBuilder;
use base 'Module::Build';

use warnings;
use strict;

use Config;
use Carp;


use File::Spec::Functions qw(catdir catfile);
use File::Path qw(make_path);
use TAP::Harness;


use v5.10;


my $BUILDDIR = 'build';


sub new {
    my ($self,@args) = @_;
    $self->SUPER::new(share_dir=>['tmp','compiled'],@args);
}


sub ACTION_code {
    my $self = shift;
    $self->SUPER::ACTION_code;

    use STD;
    mkdir('tmp');

    my $settings = ['MildewCORE.setting','DefinedBySMOP.setting'];
    my $symls = [map {catfile('tmp','syml',$_.'.syml')} 'MildewCORE','DefinedBySMOP'];

    if (!$self->up_to_date($settings,$symls)) {
        say STDERR "Feeding settings to STD\n";
        STD->parsefile('DefinedBySMOP.setting',setting=>'NULL',tmp_prefix=>'tmp/');
        STD->parsefile('MildewCORE.setting',setting=>'DefinedBySMOP',tmp_prefix=>'tmp/');
    }


    mkdir('compiled');

    if (!$self->up_to_date($settings,'compiled/MildewCORE.setting.so')) {
        say STDERR "Building MildewCORE.setting.so\n";
        system('mildew','-C','Cso',
            '++BACKEND','--no-setting','++/BACKEND',
            '++FRONTEND',
                '--tmp','tmp',
                '--setting','DefinedBySMOP',
                '--syml-search-path','tmp',
            '++/FRONTEND',
            '-o','compiled/MildewCORE.setting.so',
            'MildewCORE.setting'
        );
    }
}

sub ACTION_test {
    my $self = shift;

    $self->dispatch("code");
    my $harness = TAP::Harness->new({ exec=>sub {
        my ($harness,$file) = @_;
        warn $file;
        if ($file =~ /^t\/p6\//) {
            ["mildew",$file];
        } else {
            undef;
        }
    }});
    die "Some tests failed.\n" unless $harness->runtests(
        glob("t/p5/*")
    )->all_passed;
}



1;
