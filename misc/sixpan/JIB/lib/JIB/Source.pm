package JIB::Source;

use strict;
use warnings;

use JIB::Meta;
use JIB::Config;
use JIB::Utils;

use File::Basename          qw[dirname];
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

sub build {
    my $self = shift;

    my $builddir = File::Spec->catdir( $self->dir, $self->config->build_dir );
    
    ### XXX do this in one go
    {   my $tmpdir  = tempdir( CLEANUP => 1 );
        my $dirname = dirname( $self->dir );
        my $tmpsrc  = File::Spec->catdir( $tmpdir, $dirname );

        
        ### copy over sources to a temp dir
        JIB::Utils->_copy( file => $self->dir, to => $tmpdir ) 
            or error( "Could not copy sources to '$tmpdir'" ), return;
            
        ### toss out the meta dir            
        JIB::Utils->_rmdir( dir => File::Spec->catfile( $tmpsrc ) )
            or error( "Could not remove metadir from '$tmpdir" ), return;
        
        ### copy these sources to a build dir
        JIB::Utils->_copy( file => $tmpsrc, to => $builddir )
            or error( "Could not copy sources to '$builddir'" ), return;
    }

    return $builddir;
}


1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
