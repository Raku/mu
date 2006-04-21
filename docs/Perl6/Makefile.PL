use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';
use File::Path;

name       ('Perl6-Doc');
version    (0.01);
license    ('perl');


my $svn_path = "/usr/bin/svn";
my $location = "http://svn.perl.org/perl6/doc/trunk/design/syn";
my $synopsis_svn = "synopsis-svn";

my %commands = ( 'checkout' => "$svn_path co $location $synopsis_svn",
		 'update'   => "$svn_path update $synopsis_svn"
		);

my %table = ( 'S01' => 'Overview',
	      'S02' => 'Syntax',
	      'S03' => 'Operator',
	      'S04' => 'Block',
	      'S05' => 'Rule',
	      'S06' => 'Subroutine',
#	      'S07' => '',
#	      'S08' => '',
	      'S09' => 'Structure',
	      'S10' => 'Package',
	      'S11' => 'Module',
	      'S12' => 'Object',
	      'S13' => 'Overload',
	      'S17' => 'Threads',
	      'S29' => 'Functions'
	      );

print "Checkout the newest Synopsis from $location\n";
if ( -e $svn_path ) {
	if( -d $synopsis_svn && ! -f $synopsis_svn) {
#		system( $commands{'update'} );
	} else {
		system( $commands{'checkout'} );
	}
}
print "Moving newest synopsis to Spec\n";
for (keys %table) {
	rename "$synopsis_svn/$_.pod", "Spec/$table{$_}.pod";
}
print "Removing temporary download directory.\n";
rmtree $synopsis_svn, 0, 0;


install_script( 'p6doc' );
makemaker_args( PMLIBDIRS => [ grep { -d } glob("[A-Z]*") ]);

WritePugs  (5);
