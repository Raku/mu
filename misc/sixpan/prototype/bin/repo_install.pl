use strict;
BEGIN {
    use lib         qw[bin];
    require '_inc.pl';
    protoconf->import();
}

use Data::Dumper;
use File::Basename;
use YAML            qw[LoadFile Dump];

@ARGV or die 'Need at least one package';

my @List = LoadFile( $Repoindex );

for my $package (@ARGV ) {
    
    my $tried;
    for my $entry ( @List ) {

        ### find the entry to install
        next unless $entry->{package} eq $package;

        ### and install it -- mark we tried
        $tried++;
        install( $entry );
    }
    
    warn "No packaged found matching '$package'\n" unless $tried;
}                        

### XXX merge with pp_deps.pl            
sub _recurse_resolve {
    my @deps = @_;    
    
    ### we need at least the deps provided, plus whatever /they/ need
    my @rv = @deps;
    for my $dep ( @deps ) {
        ### unshift, as they are prereqs for THIS dependency, so they
        ### must be installed first
        unshift @rv, _recurse_resolve( list_dependencies( $dep ) );
    }
    
    return @rv;
}    


sub install {
    my $entry = shift;
    
    my @list = _recurse_resolve( $entry );
    
    ### find what dependencies we have 
    for my $depends ( @list ) {
        print "\n*** $entry->{package} depends on $depends->{package}...\n";
    
        system(qq[$^X bin/install.pl $Repodir/$depends->{location}]) and die $?;
    }
}   

