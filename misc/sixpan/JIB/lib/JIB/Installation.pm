package JIB::Installation;

use strict;
use warnings;

use JIB::Constants;
use JIB::Package;
use JIB::Config;
use JIB::Utils;
use JIB::Meta;
use JIB::Alternative;

use YAML                    qw[LoadFile DumpFile];
use Params::Check           qw[check];
use Log::Message::Simple    qw[:STD];
use File::Basename          qw[basename];
use Data::Dumper;

use base 'Object::Accessor';

=head1 ACCESSORS

=head1 METHODS

=head2 $inst = JIB::Installation->new;

=cut

### XXX do some smart caching here -- but can't just make it a singleton
### due to stacked installations
{   my $config  = JIB::Config->new;

    my %cache = ();

    sub new { 
        my $class   = shift;
        my %hash    = @_;
    
        my $dir;
        my $tmpl    = {
            dir => { required => 1, allow => DIR_EXISTS, store => \$dir },
        };
    
        check( $tmpl, \%hash ) or error( Params::Check->last_error ), return;
    
        ### make the path absolute
        $dir = File::Spec->rel2abs( $dir );

        ### singleton
        return $cache{$dir} if $cache{$dir};
    
        my $obj     = $class->SUPER::new;
    
        ### XXX tidy up
        {   $obj->mk_accessors( qw[available registered_alternatives config 
                                   dir] 
                            );
            
            $obj->dir( $dir );
            
            ### do alts first, package;:installed uses them
            my @alts = eval { 
                map { JIB::Alternative->new_from_struct( struct => $_ ) }
                    LoadFile( $config->registered_alternatives ) 
            };
            $@ and error( "Could not load registered alts file: $@" ), return;

            $obj->registered_alternatives( \@alts );

            my @avail = eval { 
                map { JIB::Package->new( meta => $_, installation => $obj) }
                map { JIB::Meta->new_from_struct( struct => $_ ) }
                    LoadFile( $config->available ) 
            };
            $@ and error( "Could not load available file: $@" ), return;
            $obj->available( \@avail );

        
            $obj->config( $config );
        }
  
        return $obj;
    }
}

=head2 .... = $inst->is_installed( package => $package )

=cut

### XXX package name?
sub is_installed {
    my $self = shift;
    my %hash = @_;
    
    my $pkg;
    my $tmpl = {
        package => { required => 1, store => \$pkg, 
                     allow => sub { UNIVERSAL::isa(shift(), 'JIB::Package') } },
    };
    
    check( $tmpl, \%hash ) or error( Params::Check->last_error ), return;

    for my $inst ( @{ $self->available } ) {
        return $inst if $inst->package eq $pkg->package;
    }
    
    return;
}    

=head2 .... = $inst->register( package => $package )

=cut

sub register {
    my $self = shift;
    my $conf = $self->config;
    my %hash = @_;
    
    my $pkg;
    my $tmpl = {
        package => { required => 1, store => \$pkg, 
                     allow => sub { UNIVERSAL::isa(shift(), 'JIB::Package') } },
    };
    
    check( $tmpl, \%hash ) or error( Params::Check->last_error ), return;

    push @{ $self->available }, $pkg;
 
    LINKING: { 
        ### XXX config!
        my $my_bindir = File::Spec->catdir( 
                            $self->dir, $pkg->package, 'bin' );

        last LINKING unless -d $my_bindir;

        ### load in the alternatives collection
        my @alts = @{ $self->registered_alternatives };
        
        ### check if we're the 'prefered' package
        my $link_this   = 1;
        my $unlink_this = '';
        
        for my $test ( @alts ) {
            ### XXX this should be a policy test!
            if( $pkg->prefix    eq $test->prefix    and
                $pkg->name      eq $test->name      and
                $pkg->version   <= $test->version
            ) {
                $link_this      = 0;  
                $unlink_this    = $test;
                last;
            }
            
            ### XXX clean up links from $unlink_this
        }      

        last LINKING unless $link_this;

        my @bins;
        
        msg("Linking scripts/manpages...");
        for ( qx[find $my_bindir -type f] ) {
            chomp; 
            
            ### link from altdir to install dir
            ### then from pathdir, to altdir
            my $script = basename($_);
        
            my $alt = File::Spec->catfile( $conf->alternatives, $script );
            my $bin = File::Spec->catfile( $conf->bin_dir, $script );
            system( qq[ln -fs $_ $alt] )                    and die $?;
            system( qq[ln -fs $alt $bin ] )                 and die $?;
            push @bins, $script;
        }
        
        ### XXX this can be done more efficiently i'm sure
        ### remove the replaced object from the alts list
        if( $unlink_this ) {
            @alts = grep { $_ eq $unlink_this } @alts;
        }
        
        push @alts, JIB::Alternative->new_from_struct( struct => 
                    { bin => \@bins, auto => 1, package => $pkg->package } );
            
        ### dump out alternatives again
        $self->registered_alternatives( \@alts );
    }

    return $self->write;

}

=head2 .... = $inst->write;

=cut

sub write {
    my $self = shift;
    my $conf = $self->config;
    
    ### XXX use tempfiles

    my @avail = map { $_->meta->to_struct } @{ $self->available };
    eval { DumpFile( $conf->available, @avail ) };
    $@ and error( "Could not write available file: $@" ), return;
    
    my @alts = map { $_->to_struct } @{ $self->registered_alternatives };
    eval { DumpFile( $conf->registered_alternatives, @alts ) };
    $@ and error( "Could not write alternatives file: $@" ), return;

    return 1;
}

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:

1;

















