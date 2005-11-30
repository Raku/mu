use strict;
BEGIN {
    use lib         qw[bin];
    require '_inc.pl';
    protoconf->import();
}

use File::Basename;
use YAML            qw[LoadFile DumpFile];


@ARGV or die 'Need at least one source dir';

for my $srcdir (@ARGV ) {
    
    ### copy all the relevant files to the repo
    for my $archive ( qx[find root-* -type f | grep '$Ext\$'] ) {
        chomp $archive;
        my $file = basename($archive);

        print "Copying '$archive'\n";
    
        ### get the metadata out
        {   ### extract to a temp dir
            my $my_tmp_dir = $Tmpdir . "/$$";
            system( qq[mkdir -p $my_tmp_dir] )                  and die $?;
            
            ### extract the archive to the temp dir
            system( qq[tar -f $archive -C $my_tmp_dir -xz] )    and die $?;
    
            ### got 2 .tgz files in the tmp dir now
            ### get the meta.info out of hte control.tgz
            system( qq[tar -f $my_tmp_dir/$Control -C $my_tmp_dir ] .
                    qq[-xz ./$Metafile] )                       and die $?;
            ### rename it to <package>.info in the repodir
            my $meta_info = $file;
            $meta_info =~ s/$Ext$/$Metaext/;
            ### XXX no hierarchy yet
            system( qq[mv $my_tmp_dir/$Metafile $Repodir/$meta_info] )
                                                                and die $?;
            system( qq[rm -rf $my_tmp_dir] )                    and die $?;
        }
        
        ### copy the .jib there too
        system( qq[cp -f $archive $Repodir/$file] )             and die $?;
    }

    print "\n\n\n";

    ### start aggregating their metainfo to one file
    my @metalist;
    for my $meta ( qx[find $Repodir -type f | grep '$Metaext\$'] ) {
        chomp $meta;
        
        print "Indexing '" . basename($meta) ."'\n";
        
        my $href = LoadFile( $meta );    
        
        ### add location to the metadata
        ### location of .jib is in same dir as $meta
        ### XXX add other found data to the metadata as well
        my $archive = $meta;
        $archive =~ s|^$Repodir|.|;
        $archive =~ s/$Metaext$/$Ext/;
        
        
        $href->{location} = $archive;
        push @metalist, $href;
    }
    
    DumpFile( $Repoindex, @metalist );
    
    print "\n\n\nIndex written to: '$Repoindex'\n\n";
    
}
