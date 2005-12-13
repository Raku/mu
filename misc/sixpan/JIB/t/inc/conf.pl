use JIB::Config;
use File::Spec;

my $config = JIB::Config->new;

system( qw[rm -rf], $config->root, $config->build_dir .'*' )   and die $?;

### set up the 'fakeroot'
### XXX might need to be a bit more elegant, and not blow it away every go
{   system( qw[rm -rf], $config->root )                         and die $?;
    
    ### XXX missing repodirs/files
    for ( qw[root perl_site_dir temp_dir bin_dir compile_dir meta_dir 
            alternatives ] 
    ) {
        system( qw[mkdir -p], $config->$_ )                     and die $?;
    }
    
    
    for ( qw[available registered_alternatives ] ) {
        system( q[touch], $config->$_ )                         and die $?;
    }
}


1;
