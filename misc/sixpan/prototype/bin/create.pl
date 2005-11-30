use strict;
use YAML            qw[LoadFile];
use File::Basename  qw[basename];
use Data::Dumper;
use Cwd;

BEGIN {
    use lib         qw[bin];
    require '_inc.pl';
    protoconf->import();
}

@ARGV or die 'Need at least one source dir';

for my $srcdir (@ARGV ) {
    ### XXX can be custom file & nicer object & error checking
    my $struct  = LoadFile( "$srcdir/$Pms/$Metafile" ) 
                        or die "Could not read $Metafile";

    my $path    = $struct->{package};
    
    ### create a buildroot
    my $builddir    = $Build_prefix . basename( $srcdir );
    {   ### copy all the stuff over to another dir
        
        ### toss out old stuff
        system( qq[ rm -rf $builddir ] )    and die "$?";

        ### XXX instead of cp -R, we can read manifest/metafile
        system( qq[mkdir -p $builddir/$path] )          and die "$?";
        system( qq[ cp -R $srcdir/* $builddir/$path ] ) and die "$?";
        chdir $builddir or die "Could not chdir to $builddir: $!";
    }

    ### build an archive file
    my $archive = $path . $Ext;
    {   system( qq[tar --exclude $Pms -czf $Data $path] )
            and die $?;
        system( qq[tar -f $Control -C $path/$Pms -cz .] )   and die $?;
        system( qq[tar -czf $archive $Control $Data] )      and die $?;
        
        1 while unlink $Data;
        1 while unlink $Control
    }
    
    print -e $archive && -s _ 
        ?   "\n\tCreated '$builddir/$archive' from sourcedir '$srcdir'\n"
        :   "\n\tFailed to created '$builddir/$archive' from '$srcdir'\n"
        ;

    chdir $Cwd or die "Could not chdir back to '$Cwd'\n";
}    

    
