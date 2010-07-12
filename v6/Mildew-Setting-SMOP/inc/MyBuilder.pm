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
    mkdir('compiled');
    STD->parsefile('DefinedBySMOP.setting',setting=>'NULL',tmp_prefix=>'tmp/');
    STD->parsefile('MildewCORE.setting',setting=>'DefinedBySMOP',tmp_prefix=>'tmp/');

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



1;
