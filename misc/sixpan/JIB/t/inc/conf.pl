use strict;

use JIB::Config;
use JIB::Installation;
use Path::Class ();
use Cwd;

my $config  = JIB::Config->new;

### new fakeroot dir
$config->root( Path::Class::dir( cwd() )->subdir( 'fakeroot' ) );

system( qw[rm -rf], $config->root, $config->build_dir .'*' )   and die $?;

### set up the 'fakeroot'
### XXX might need to be a bit more elegant, and not blow it away every go
{   system( qw[rm -rf], $config->root )                         and die $?;
    
    ### XXX missing repodirs/files
    for ( map { $config->$_ } qw[ root perl_site_dir temp_dir bin_dir 
                                 compile_dir ]
    ) {
        print "Creating dir: $_\n";
        system( qw[mkdir -p], $_ )                              and die $?;
    }

    ### XXX do it like this, as an installation needs an existing dir,
    ### so we need to make them first
    for ( map { $config->perl_site_dir->subdir( $config->$_ ) } 
            qw[ _meta_dir _alternatives _control]
    ) {            
        print "Creating dir: $_\n";
        system( qw[mkdir -p], $_ )                              and die $?;
    }

    ### XXX need to touch these first, or the JIB::Installation->new will fail
    #for ( map { $inst->$_ } qw[available registered_alternatives] ) {
    #    system( q[touch], $_ )                                  and die $?;
    #}
    for ( qw[_available _registered_alternatives] ) {
        my $file = $config->perl_site_dir
                          ->file( $config->$_ );
        print "Creating file: $file\n";
        system( qw[touch], $file )                              and die $?;
    }


#     my $inst    = JIB::Installation->new( dir => $config->perl_site_dir );
#     for ( map { $inst->$_ } qw[ meta_dir alternatives_dir control_dir ] ) {
#         system( qw[mkdir -p], $_ )                              and die $?;
#     }
    
    
}


1;

