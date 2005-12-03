package JIB::Source;

use strict;
use warnings;

use JIB::Meta;
use JIB::Config;

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

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
