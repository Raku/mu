package MyBuilder;
use base 'Module::Build';

use warnings;
use strict;

use Config;
use Carp;

use Config::AutoConf;
use Config::AutoConf::Linker;

use ExtUtils::ParseXS;
use ExtUtils::Mkbootstrap;

use File::Spec::Functions qw(catdir catfile);
use File::Path qw(make_path);
use TAP::Harness;

use ExtUtils::Embed qw(ldopts ccopts);

use v5.10;


my $BUILDDIR = 'build';


sub new {
    my ($self,@args) = @_;
    $self->SUPER::new(@args);
    # share_dir=>[...]
}


sub ACTION_code {
    my $self = shift;
    $self->SUPER::ACTION_code;

    use STD;
    mkdir('tmp');
    STD->parsefile('DefinedBySMOP.setting',setting=>'NULL',tmp_prefix=>'tmp/');
    STD->parsefile('MildewCORE.setting',setting=>'DefinedBySMOP',tmp_prefix=>'tmp/');
}



1;
