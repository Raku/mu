package JIB::Config;

use strict;
use warnings;
use Cwd;
use Config;
use Path::Class ();
use base 'Object::Accessor';

my $Cwd = Path::Class::dir(cwd());

my %config;

### XXX dirs should be configurable during run time, and all subdirs
### should adjust

### base dirs
### XXX touch the 'root' entry and the tests will run somewhere quite different!
$config{'root'}             = $Cwd->subdir('fakeroot');
$config{'perl_site_dir'}    = $config{'root'}->subdir($Config{installsitelib});
$config{'temp_dir'}         = $config{'root'}->subdir('tmp');
$config{'bin_dir'}          = $config{'root'}->subdir( $Config{bin} );
$config{'compile_dir'}      = $config{'temp_dir'}->subdir('_builddir');

### XXX missing the /jibs and the /jibs/index dir for repos

### meta dirs
$config{'meta_dir'}         = $config{'perl_site_dir'}->subdir('_jib');
$config{'control'}          = $config{'meta_dir'}->subdir('control');
$config{'available'}        = $config{'meta_dir'}->file('available');
$config{'registered_alternatives'} 
                            = $config{'meta_dir'}->
                                    file('registered-alternatives');
$config{'alternatives'}     = $config{'meta_dir'}->subdir('alternatives');
$config{'files_list'}       = $config{'meta_dir'}->file('files.list');

### source package dirs
$config{'jib_dir'}          = Path::Class::dir('_jib');
### XXX need cp -R functionality fixed proper to use this ;(
#$config{'build_dir'}        = $config{'jib_dir'}->subdir('build');
$config{'build_dir'}        = Path::Class::dir('root-');

### package files/extensions
$config{'meta_ext'}         = '.info';
$config{'meta_file'}        = 'META' . $config{'meta_ext'};

$config{'archive_data'}     = 'data.tgz';
$config{'archive_control'}  = 'control.tgz';
$config{'archive_ext'}      = '.jib';

$config{'preinst'}          = 'PREINST.pl';
$config{'postinst'}         = 'POSTINST.pl';
$config{'prerm'}            = 'PRERM.pl';
$config{'postrm'}           = 'POSTRM.pl';

{   my $obj;
    sub new {
        return $obj if $obj;
        
        my $self = shift;
        $obj     = $self->SUPER::new();
 
        ### XXX allow handlers
        $obj->mk_accessors( keys %config );
        $obj->$_( $config{$_} ) for keys %config;
        
        return $obj;
    }
}

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
