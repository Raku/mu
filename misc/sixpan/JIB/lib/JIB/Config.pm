package JIB::Config;

use strict;
use warnings;
use Path::Class;
use base 'Class::Singleton';

my %config;

$config{'Meta'}             = dir('meta');
$config{'Control'}          = $config{'Meta'}->subdir('control');
$config{'Available'}        = $config{'Meta'}->file('available');
$config{'RegisteredAlternatives'} 
                            = $config{'Meta'}->file('registered-alternatives');
$config{'Alternatives'}     = $config{'Meta'}->subdir('alternatives');

$config{'MetaExt'}          = '.info';
$config{'MetaFile'}         = 'META' . $config{'MetaExt'};

$config{'ArchiveData'}      = 'data.tar.gz';
$config{'ArchiveControl'}   = 'control.tar.gz';
$config{'ArchiveExt'}       = '.jib';

$config{'Preinst'}          = 'PREINST.pl';
$config{'Postinst'}         = 'POSTINST.pl';
$config{'Prerm'}            = 'PRERM.pl';
$config{'Postrm'}           = 'POSTRM.pl';

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
