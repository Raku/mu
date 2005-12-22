use strict;

use JIB::Config;
use Path::Class ();
use Cwd;
use Config;

use vars qw[$INSTALLATION_DIR];

my $config  = JIB::Config->new;
### new fakeroot dir
$config->root( Path::Class::dir( cwd() )->subdir( 'fakeroot' ) );

$INSTALLATION_DIR = $config->root->subdir( $Config{installsitelib} );

system( qw[rm -rf], $config->root, $config->build_dir .'*' )   and die $?;

### set up the 'fakeroot'
### XXX might need to be a bit more elegant, and not blow it away every go
{   system( qw[rm -rf], $config->root )                         and die $?;
    
    ### XXX missing repodirs/files
    for ( (map { $config->$_ } qw[ root temp_dir bin_dir compile_dir ]),
          $INSTALLATION_DIR  
    ) {
        print "Creating dir: $_\n";
        system( qw[mkdir -p], $_ )                              and die $?;
    }
}


1;

