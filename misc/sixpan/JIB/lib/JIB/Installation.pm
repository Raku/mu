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

=head2 $dir = $inst->control_dir( $pkg_name )

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

=head2 $inst_pkg = $inst->is_installed( package => $package )

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

=head2 $inst_pkg = $inst->register( package => $package )

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

    my $inst_pkg = JIB::Package->new(meta => $pkg->meta, installation => $self)
        or error("Could not create installed package"), return;
 
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

        ### remove the registered alternative
        $self->remove_registered_alternative( package => $unlink_this )
            if $unlink_this;

        ### link the files
        return unless $self->link_files( package => $inst_pkg );

    }

    ### update the available files
    $self->available( [ @{ $self->available }, $inst_pkg ] );
        
    $self->write or return;
    
    return $inst_pkg;

}

=head2 $bool = $inst->unregister( package => $inst_pkg );

=cut

sub unregister {
    my $self = shift;
    my $conf = $self->config;
    my %hash = @_;
    
    
    my $inst_pkg;
    my $tmpl = {
        package => { required => 1, store => \$inst_pkg, 
                        allow => ISA_JIB_PACKAGE_INSTALLED },
    };
    
    check( $tmpl, \%hash ) or error( Params::Check->last_error ), return;

=pod
    
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
=cut
    
    ### uninstall the files
    ### XXX check dependencies
    {   open my $fh, $self->files_list( $inst_pkg->package )        or die $!;
    
        my $prerm = $self->control_dir( $inst_pkg->package )
                         ->file( $conf->prerm );
        if( -e $prerm && -s _ ) {
            system( qq[ $^X $prerm ] )                              and die $?;
        }
        
        while( <$fh> ) {
            chomp;
            msg("Removing file '$_'", 1);
            -e $_ && system(qq[rm -rf $_])                          and die $?;
            
            die "File '$_' not removed" if -e $_;
        }
        close $fh;
    
        ### XXX need status dir like dpkg
        my $postrm = $self->control_dir( $inst_pkg->package )
                          ->file( $conf->postrm );
        if( -e $postrm && -s _ ) {
            system( qq[ $^X $postrm ] )                             and die $?;
        }
    }


    ### remove alternatives and relink if needed
    ### XXX doesn't do manpages yet
    ### XXX doens't check the AUTO flag yet for link management
    LINKING: {   
        my $alt = $self->remove_registered_alternative( package => $inst_pkg );

        ### this package didn't provide any alternatives
        last LINKING unless $alt;
        
        ### XXX this should probably be done in one go, so we don't
        ### have a situation where no 'script' is available
        
        ### unlink all the script files
        for my $script ( @{ $alt->bin || [] } ) {
            1 while unlink $conf->bin_dir->file( $script );
            1 while unlink $self->alternatives_dir->file( $script ); 
        }      
        
        ### see if there's any other package that's now the default
        ### for this module
        ### make sure we dont see ourselves again, so grep that out
        my $new_alt;
        {   my $wanted = join '-',  $inst_pkg->prefix, $inst_pkg->name;

            ### find all packages that provide: a <prefix>-<name>
            ### implementation;
            my @list = @{ $self->available };
            
            my @maybe;
            for my $test ( grep { $_->package ne $inst_pkg->package } @list ) {
                push @maybe, $test if grep {
                        $_ eq $wanted
                    } @{ $test->provides || [] };
            }
        
            ### find the alternative with the highest version
            ### XXX this should be policy based!
            ($new_alt) = sort { $b->version <=> $a->version } @maybe;
        }

        ### no alt? bail!
        last LINKING unless $new_alt;
    
        ### link the files
        return unless $self->link_files( package => $new_alt );
    }

    ### remove this package from the available list
    $self->remove_available( package => $inst_pkg );
    
    ### move this all to Installation.pm
    $self->write or return;
    
    ### unisntall metadata
    system( qq[rm -rf ] . $self->control_dir( $inst_pkg->package ) ) and die $?;

    msg( "Package '".$inst_pkg->package."' and associated metadata removed");

    return 1;
}

=head2 $alt = $inst->remove_registered_alternative( package => $inst_pkg )

=cut

sub remove_registered_alternative {
    my $self = shift;
    my %hash = @_;
    
    my $inst_pkg;
    my $tmpl = {
        package => { required => 1, store => \$inst_pkg, 
                        allow => ISA_JIB_PACKAGE_INSTALLED },
    };
    
    check( $tmpl, \%hash ) or error( Params::Check->last_error ), return;

    my $alt; 
    my @alts = map  { $_->[0] } # return the object
               grep { $_->[1] } # flag toggled?
               map  { $_->package eq $inst_pkg->package
                        ? do { $alt = $_; [ $_, 0 ] }
                        : [ $_, 1 ]
                } @{ $self->registered_alternatives }; 

    $self->registered_alternatives( \@alts ) if $alt;

    return $alt if $alt;
    return;
}    

=head2 $bool = $inst->remove_available( package => $inst_pkg );

=cut

sub remove_available {
    my $self = shift;
    my %hash = @_;
    
    my $inst_pkg;
    my $tmpl = {
        package => { required => 1, store => \$inst_pkg, 
                        allow => ISA_JIB_PACKAGE_INSTALLED },
    };
    
    check( $tmpl, \%hash ) or error( Params::Check->last_error ), return;

    ### remove this package from the available list
    ### XXX temp file, then mv
    my @prev  = @{ $self->available };
    my @avail = grep { $_->package ne $inst_pkg->package } @prev;
    
    ### no change!
    return if @avail == @prev;
    
    $self->available( \@avail );
    return 1;
}

=head2 $bool|$alt = $inst->link_files( package => $inst_pkg );

=cut

### XXX no manpages yet
sub link_files {
    my $self = shift;
    my $conf = $self->config;
    my %hash = @_;
    
    my $inst_pkg;
    my $tmpl = {
        package => { required => 1, store => \$inst_pkg, 
                        allow => ISA_JIB_PACKAGE_INSTALLED },
    };
    
    check( $tmpl, \%hash ) or error( Params::Check->last_error ), return;

    my $my_bindir =  $self->dir->subdir( $inst_pkg->package )->subdir('bin');

    ### XXX doesn't return an object...
    return 1 unless -d $my_bindir;

    my @bins;
    msg( "Linking scripts/manpages to " . $inst_pkg->package, 1 );
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

    my $alt = JIB::Alternative->new_from_struct( struct => 
                    { bin => \@bins, auto => 1, package => $inst_pkg->package } 
                ) or return;

    ### dump out alternatives again
    ### XXX pretty method?
    $self->registered_alternatives([ @{$self->registered_alternatives}, $alt ]);

    return $alt;
}

=head2 $bool = $inst->write;

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

















