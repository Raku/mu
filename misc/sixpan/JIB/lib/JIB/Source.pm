package JIB::Source;

use strict;
use warnings;

use JIB::Meta;
use JIB::Config;
use JIB::Utils;
use JIB::Package;

use File::chdir;
use File::Basename          qw[basename];
use File::Temp              qw[tempdir];
use Params::Check           qw[check];
use Log::Message::Simple    qw[:STD];

use base 'Object::Accessor';

=head1 ACCESSORS

=over 4

=item dir

=item meta

=item config

=back

=head1 METHODS

=head2 $src = JIB::Source->new( dir => /path/to/source )

=cut

sub new { 
    my $class   = shift;
    my %hash    = @_;
    
    my $dir;
    my $tmpl = {
        dir     => { required => 1, allow => sub { -d shift() }, 
                     store => \$dir },
    };
    
    my $args = check( $tmpl, \%hash ) 
                    or ( error( Params::Check->last_error), return );

    my $obj = $class->SUPER::new;
    ### XXX add allow handlers
    $obj->mk_accessors( qw[meta config dir] );

    ### get a config and the meta object
    {   my $config  = JIB::Config->new;
        my $meta    = JIB::Meta->new( 
                            file => File::Spec->catfile( 
                                $dir, $config->jib_dir, $config->meta_file ) 
                        ) or return;

        $obj->config( $config   );
        $obj->meta(   $meta     );
        $obj->dir(    $dir      );
    }        
    
    return $obj;
}

=head2 $file = $src->build( ... )

=cut

### XXX need some good path manipulation tools.. this is too much work :(
#     
#     ### clean out the builddir 
#     {   if( -d $builddir ) {
#             JIB::Utils->_rmdir( dir => $builddir )
#                 or error( "Could not clean out '$builddir'" ), return;
#         }                
#         JIB::Utils->_mkdir( dir => $builddir )
#             or error( "Could not create clean '$builddir'" ), return;
#     }
#     
#     ### XXX do this in one go
#     {   my $tmpdir  = tempdir( CLEANUP => 1 );
#         my $dirname = basename( $self->dir );
#         my $tmpsrc  = File::Spec->catdir( $tmpdir, $dirname );
# 
#         
#         ### copy over sources to a temp dir
#         JIB::Utils->_copy( file => $self->dir, to => $tmpdir ) 
#             or error( "Could not copy sources to '$tmpdir'" ), return;
#             
#         ### toss out the meta dir            
#         JIB::Utils->_rmdir( 
#             dir => File::Spec->catfile( $tmpsrc, $self->config->build_dir ) )
#                 or error( "Could not remove metadir from '$tmpdir" ), return;
# 
#         ### copy these sources to a build dir
#         JIB::Utils->_copy( file => $tmpsrc, to => $builddir )
#             or error( "Could not copy sources to '$builddir'" ), return;
#     }
# 
#     my $cwd = JIB::Utils->_chdir( dir => $builddir ) 
#         or error( "Could not chdir to '$builddir'" ), return;

### XXX perl-ify
sub build {
    my $self = shift;
    my $conf = $self->config;

    ### XXX move to builddir under pkg_dir/_jib/build
    my $path     = $self->meta->package;
    my $builddir = $conf->build_dir . $path;
    my $srcdir   = $self->dir;

    {   ### copy all the stuff over to another dir
        
        ### toss out old stuff
        system( qq[ rm -rf $builddir ] )                and error($?), return;   

        ### XXX instead of cp -R, we can read manifest/metafile
        system( qq[mkdir -p $builddir] )                and error($?), return;   
        system( qq[mkdir -p $builddir/$path] )          and error($?), return;
        system( qq[ cp -R $srcdir/* $builddir/$path ] ) and error($?), return;
    }        

    ### XXX make me more robust
    local $CWD = $builddir;
    
    ### XXX invoke build code here

    ### build an archive file
    my $archive = $path . $conf->archive_ext;
    {   my $data    = $conf->archive_data;
        my $control = $conf->archive_control;
        my $jibdir  = $conf->jib_dir;
    
        system( qq[tar --exclude $jibdir -czf $data $path] )
                                                        and error($?), return;
        system( qq[tar -f $control -C $path/$jibdir -cz .] )
                                                        and error($?), return;
        system( qq[tar -czf $archive $control $data] )  and error($?), return;
        
        1 while unlink $data;
        1 while unlink $control
    }

    return JIB::Package->new( 
                file => File::Spec->rel2abs(
                    File::Spec->catfile( $archive )
                ),
                meta => $self->meta,
            );                
}



1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
