package JIB::Config;

use strict;
use warnings;
use Path::Class;
use base 'Exporter';

use vars qw(
		$Meta
		$Control
		$Available
		$RegisteredAlternatives
		$Alternatives

		$MetaExt
		$MetaFile

		$ArchiveData
		$ArchiveControl
		$ArchiveExt

		$Preinst
		$Postinst
		$Prerm
		$Postrm
);

$Meta = dir('meta');
$Control = $Meta->dir('control');
$Available = $Meta->file('available');
$RegisteredAlternatives = $Meta->file('registered-alternatives');
$Alternatives = $Meta->dir('alternatives');

$MetaExt = '.info';
$MetaFile = 'INFO'.$MetaExt;

$ArchiveData = 'data.tar.gz';
$ArchiveControl = 'control.tar.gz';
$ArchiveExt = '.jib';

$Preinst = 'PREINST.pl';
$Postinst = 'POSTINST.pl';
$Prerm = 'PRERM.pl';
$Postrm = 'POSTRM.pl';

1;
