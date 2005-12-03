package JIB::Config;

use strict;
use warnings;
use Path::Class;
use base 'Class::Singleton';

my %config;

$config{'meta'}             = dir('meta');
$config{'jib_dir'}          = dir('_jib');
$config{'build_dir'}        = $config{'jib_dir'}->subdir('build');
$config{'control'}          = $config{'meta'}->subdir('control');
$config{'available'}        = $config{'meta'}->file('available');
$config{'registered_alternatives'} 
                            = $config{'meta'}->file('registered-alternatives');
$config{'alternatives'}     = $config{'meta'}->subdir('alternatives');

$config{'meta_ext'}         = '.info';
$config{'meta_file'}        = 'META' . $config{'meta_ext'};

$config{'archive_data'}     = 'data.tar.gz';
$config{'archive_control'}  = 'control.tar.gz';
$config{'archive_ext'}      = '.jib';

$config{'preinst'}          = 'PREINST.pl';
$config{'postinst'}         = 'POSTINST.pl';
$config{'prerm'}            = 'PRERM.pl';
$config{'postrm'}           = 'POSTRM.pl';

sub new { shift->instance };

for my $sym (keys %config) {
    {
        no strict 'refs';
        *$sym = sub {
            return $config{$sym}
        };
    }
}

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
