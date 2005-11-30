use strict;
use YAML            qw[LoadFile DumpFile];
use Data::Dumper;
use File::Basename;
use Cwd;
use Config;

BEGIN {
    use lib         qw[bin];
    require '_inc.pl';
    protoconf->import();
}

@ARGV or die 'Need at least one package';

for my $pkg (@ARGV) {
    my $meta = $Metactrl .'/'. $pkg;
    
    die "$pkg not installed -- dir '$meta' does not exist\n" unless -d $meta;
    
    my @list        = LoadFile( $Available ); 
    my @uninstalled = grep { $_->{package} ne $pkg } @list;
    
    ### check if we're even allowed to delete this, due to depends
    {   my $info = LoadFile( $meta .'/'. $Metafile );

        my $delete_ok = 1;

        for my $entry ( @list ) {
            for my $depends ( list_dependencies( $entry ) ) {
                
                ### check if this entry depends on /any/ of the items
                ### we provide
                if( dependency_satisfied_by( $depends, $info ) ) {

                    ### if the dependency is also sastisfied by /another/
                    ### package, it's still safe to delete us, otherwise not
                    if( !dependency_satisfied( $depends, \@uninstalled ) ) {

                        warn "\t*** $entry->{package} depends on $pkg ***\n";
                        $delete_ok = 0;
                    }
                }
            }      
        }
        
        die "Not allowed to delete '$pkg'\n" unless $delete_ok;
    }
    
    ### uninstall the files
    ### XXX check dependencies
    open my $fh, $meta .'/'. $Fileslist                         or die $!;
    
    my $prerm = $meta . '/' . $Prerm;
    if( -e $prerm && -s _ ) {
        system( qq[ $^X $prerm ] )                              and die $?;
    }
    
    while( <$fh> ) {
        chomp;
        -e $_ && system(qq[rm -rf $_])                          and die $?;
        
        die "File '$_' not removed" if -e $_;
    }
    close $fh;


    ### XXX need status dir like dpkg
    my $postrm = $meta . '/' . $Postrm;
    if( -e $postrm && -s _ ) {
        system( qq[ $^X $postrm ] )                             and die $?;
    }

    ### remove alternatives and relink if needed
    ### XXX doesn't do manpages yet
    ### XXX doens't check the AUTO flag yet for link management
    LINKING: {   
        ### load in the alternatives collection
        my $href = LoadFile( $Altfile );

        ### this package didn't provide any alternatives
        last LINKING unless $href->{$pkg};
        
        ### XXX this should probably be done in one go, so we don't
        ### have a situation where no 'script' is available
        
        ### unlink all the script files
        for my $script ( @{ $href->{$pkg}->{bin} || [] } ) {
            1 while unlink "$Bindir/$script";
            1 while unlink "$Alternatives/$script";
        }      
        
        ### see if there's any other package that's now the default
        ### for this module
        ### make sure we dont see ourselves again, so grep that out
        my $new_alt;
        {   my $wanted = join '-',  package_prefix(   $pkg ),
                                    package_name(     $pkg );

            ### find all packages that provide: a <prefix>-<name>
            ### implementation;
            my @list = LoadFile( $Available );
            
            my @maybe;
            for my $test ( grep { $_->{package} ne $pkg } @list ) {
                push @maybe, $test if grep {
                        $_ eq $wanted
                    } @{ $test->{provides} || [] };
            }
        
            ### find the alternative with the highest version
            ### XXX this should be policy based!
            ($new_alt) = sort { $b->{version} <=> $a->{version } } @maybe;
        }

        ### no alt? bail!
        last LINKING unless $new_alt;
    
        my $my_bindir = "$Site/$new_alt->{package}/bin";
    
        my @bins;
        print "\nRelinking scripts/manpages to $new_alt->{package}...\n";
        for ( qx[find $my_bindir -type f] ) {
            chomp; 
            
            ### link from altdir to install dir
            ### then from pathdir, to altdir
            my $script = basename($_);
            system( qq[ln -fs $_ $Alternatives/$script] )       and die $?;
            system( qq[ln -fs $Alternatives/$script $Bindir/$script ] )
                                                                and die $?;
            push @bins, $script;
        }
        
        ### add this package as being authorative for these links ###
        $href->{ $pkg } = { bin => \@bins };
            
        ### dump out alternatives again
        DumpFile( $Altfile, $href );
    }

    ### remove this package from the available list
    ### XXX temp file, then mv
    DumpFile( $Available, @uninstalled );
    
    ### unisntall metadata
    system(qq[rm -rf $meta])                                    and die $?;

    print "\n\tPackage '$pkg' and associated metadata removed\n";
}
