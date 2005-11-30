use strict;
use YAML            qw[LoadFile DumpFile];
use Data::Dumper;
use File::Basename;
use Config;

BEGIN {
    use lib         qw[bin];
    require '_inc.pl';
    protoconf->import();
}

@ARGV or die 'Need at least one archive';

INSTALL: for my $archive (@ARGV) {

    ### the path to install to... distilled from the archive name
    ### XXX maybe metainfo name later? requires peeking in, might be a
    ### catch22 if we don't do it from repository metadata
    my $path = basename($archive);
    $path    =~ s/$Ext$//;

    ### check if it is already installed
    {   my @list = LoadFile( $Available );
        if ( grep { $_->{package} eq $path } @list ) {
            warn "$path is already installed"; 
            next INSTALL;
        }
    }

    ### install the archive
    {   ### extract to a temp dir
        my $my_tmp_dir = $Tmpdir . "/$$";
        system( qq[mkdir -p $my_tmp_dir] )                  and die $?;
        
        ### extract the archive to the temp dir
        system( qq[tar -f $archive -C $my_tmp_dir -xz] )    and die $?;
    
        my $meta_dir = $Metactrl . "/$path";
        ### extract the meta info
        ### XXX extract to $Builddir first, THEN copy later if all goes well
        {   print "Writing meta data...\n";
            system( qq[mkdir -p $meta_dir] )                and die $?;
            ### XXX need status dir like dpkg
            system( qq[tar -f $my_tmp_dir/$Control -C $meta_dir -xz] )
                                                            and die $?;
            print "Writing .packlist equiv...\n";
            ### write a .packlist equiv
            system( qq[tar -f $my_tmp_dir/$Data -C $meta_dir -tz |] .
                    qq[xargs -I % echo $Site/% >> $meta_dir/$Fileslist] )   
                                                            and die $?;
            
            ### dependencies satisfied?
            {   my $info = LoadFile( $meta_dir .'/'. $Metafile );
                
                my %avail = map { $_->{package} => $_ } LoadFile( $Available );
                for my $depends ( list_dependencies( $info ) ) {
                    ### XXX split depends: lines and objectified dependencies
                    ### for better diagnostics
                    die "Dependency '$depends->{package}' not satisfied " .
                        "for '$path'" unless $avail{ $depends->{package} };
                }

                ### write the data into the available list
                ### XXX temp file, then mv
                my @list = LoadFile( $Available );
                push @list, $info;                
                DumpFile( $Available, @list );
            }
        }
    
        ### extract the code
        {   ### XXX we should *build* things here too
            print "Unpacking code...\n";
            system( qq[tar -f $my_tmp_dir/$Data -C $Builddir -xz] );
        
            ### preinst hook
            my $preinst = $meta_dir . '/' . $Preinst;
            if( -e $preinst && -s _ ) {
                system( qq[ $^X $preinst ] )                and die $?;
            }
        
            print "Installing code...\n";
            system( qq[cp -R $Builddir/$path $Site] )       and die $?;     
            
            ### link files to $PATH/$MANPATH
            ### XXX symlink the manpages
            LINKING: {   
                my $my_bindir = "$Site/$path/bin";
                last LINKING unless -d $my_bindir;

                ### load in the alternatives collection
                my $href = LoadFile( $Altfile );
                
                ### check if we're the 'prefered' package
                my $link_this   = 1;
                my $unlink_this = '';
                {   my $prefix  = package_prefix(   $path );
                    my $package = package_name(     $path );
                    my $version = package_version(  $path );
    
                    for my $test ( keys %$href ) {
                        if( $prefix     eq package_prefix(  $test ) and
                            $package    eq package_name(    $test ) and 
                            ### XXX this should be a policy test!
                            $version    <= package_version( $test )
                        ) {
                            $link_this      = 0;  
                            $unlink_this    = delete $href->{$test};
                            last;
                        }
                    }
                    
                    ### XXX clean up links from $unlink_this
                }      

                last LINKING unless $link_this;

                my @bins;
                print "Linking scripts/manpages...\n";
                for ( qx[find $my_bindir -type f] ) {
                    chomp; 
                    
                    ### link from altdir to install dir
                    ### then from pathdir, to altdir
                    my $script = basename($_);
                    system( qq[ln -fs $_ $Alternatives/$script] )   and die $?;
                    system( qq[ln -fs $Alternatives/$script $Bindir/$script ] )
                                                                    and die $?;
                    push @bins, $script;
                }
                
                ### add this package as being authorative for these links ###
                $href->{ $path } = { bin => \@bins, auto => 1 };
                    
                ### dump out alternatives again
                DumpFile( $Altfile, $href );

                my $postinst = $meta_dir . '/'. $Postinst;
                if( -e $postinst && -s _ ) {
                    system( qq[ $^X $postinst ] )               and die $?;
                }    
            }
        }
        
        ### clean up the temp dir
        print "Cleaning up...\n";
        system( qq[rm -rf $my_tmp_dir] )                    and die $?;
    
        my $final_dir = $Site .'/'. $path;
        print -d  $final_dir
            ? "\n\tInstalled '$archive' into '$final_dir'\n"
            : "\n\tFailed to install '$archive' into '$final_dir'\n"
            ;
    
    }
}
