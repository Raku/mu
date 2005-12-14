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
use Path::Class             ();
use Data::Dumper;

use base 'Object::Accessor';

=head1 ACCESSORS

=head2 $dir = $inst->meta_dir

=cut

sub meta_dir {
    my $self = shift;
    my $dir  = $self->dir or return;

    return $dir->subdir( $self->config->_meta_dir);
}

=head2 $dir = $inst->control_dir

=cut

sub control_dir {
    my $self = shift;
    my $pkg  = shift        or return;
    my $dir  = $self->dir or return;

    return $dir->subdir( $self->config->_control )->subdir( $pkg );
}


=head2 $dir = $inst->alternatives_dir

=cut

sub alternatives_dir {
    my $self = shift;
    my $dir  = $self->dir or return;

    return $dir->subdir( $self->config->_alternatives );
}

=head2 $file = $inst->registered_alternatives_file

=cut

sub registered_alternatives_file {
    my $self = shift;
    my $dir  = $self->dir or return;

    return $dir->file( $self->config->_registered_alternatives );
}

=head2 $file = $inst->available_file

=cut

sub available_file {
    my $self = shift;
    my $dir  = $self->dir or return;

    return $dir->file( $self->config->_available );
}

=head2 $file = $inst->files_list( $pkg_name )

=cut

sub files_list {
    my $self = shift;
    my $pkg  = shift        or return;
    my $dir  = $self->dir   or return;

    return $self->control_dir( $pkg )
                ->file( $self->config->_files_list );
}

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
        ### XXX path::class doesn't seem to have a rel2abs
        $dir = Path::Class::dir( File::Spec->rel2abs( $dir ) );

        ### singleton
        return $cache{$dir} if $cache{$dir};
    
        my $obj     = $class->SUPER::new;
    
        ### XXX tidy up
        {   $obj->mk_accessors( qw[available registered_alternatives config 
                                   dir] 
                            );
            
            ### set these 2 first, the other methods rely on it
            $obj->dir( $dir );
            $obj->config( $config );
            
            ### do alts first, package;:installed uses them
            my @alts = eval { 
                map { JIB::Alternative->new_from_struct( struct => $_ ) }
                    LoadFile( $obj->registered_alternatives_file ) 
            };
            $@ and error( "Could not load registered alts file: $@" ), return;

            $obj->registered_alternatives( \@alts );

            my @avail = eval { 
                map { JIB::Package->new( meta => $_, installation => $obj) }
                map { JIB::Meta->new_from_struct( struct => $_ ) }
                    LoadFile( $obj->available_file ) 
            };
            $@ and error( "Could not load available file: $@" ), return;
            $obj->available( \@avail );

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
        package => { required => 1, store => \$pkg, allow => ISA_JIB_PACKAGE },
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
        package => { required => 1, store => \$pkg, allow => ISA_JIB_PACKAGE },
    };
    
    check( $tmpl, \%hash ) or error( Params::Check->last_error ), return;
 
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
        
            my $alt = $self->alternatives_dir->file( $script );
            my $bin = $conf->bin_dir->file( $script );
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

    my $inst_pkg = JIB::Package->new(meta => $pkg->meta, installation => $self)
        or error("Could not create installed package"), return;

    ### update the available files
    $self->available( [ @{ $self->available }, $inst_pkg ] );
        
    $self->write or return;
    
    return $inst_pkg;

}

=head2 .... = $inst->write;

=cut

sub write {
    my $self = shift;
    my $conf = $self->config;
    
    ### XXX use tempfiles

    my @avail = map { $_->meta->to_struct } @{ $self->available };
    eval { DumpFile( $self->available_file, @avail ) };
    $@ and error( "Could not write available file: $@" ), return;
    
    my @alts = map { $_->to_struct } @{ $self->registered_alternatives };
    eval { DumpFile( $self->registered_alternatives_file, @alts ) };
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

















