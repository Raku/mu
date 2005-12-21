package JIB::Package::Source;

use strict;
use warnings;

use base 'JIB::Package';

use JIB::Constants;
use JIB::Config;
use JIB::Installation;


use File::Spec;
use File::Basename          qw[basename];
use Params::Check           qw[check];
use Log::Message::Simple    qw[:STD];
use YAML                    qw[LoadFile];

use Data::Dumper;
$Data::Dumper::Indent = 1;

### only called from JIB::Package
sub new {
    my $self    = shift;
    my %hash    = @_;
    
    my $file; my $meta;
    my $tmpl = {
        file    => { required => 1, allow => FILE_EXISTS, store => \$file },
        meta    => { store => \$meta, 
                     allow => sub { UNIVERSAL::isa(shift(), 'JIB::Meta') } },
    };
    my $args = check( $tmpl, \%hash ) 
                or error( Params::Check->last_error ), return;

    while( my($acc,$val) = each %$args ) {
        $self->$acc( $val );
    }

    unless( $meta or $meta = $self->extract_meta_object ) {
        return;
    }
    
    $self->package( $self->meta->package );
    
    return $self;
}

=head2 $pkg->install( installation => INSTALLATION_OBJECT )

=cut

### XXX perl-ify
sub install {
    my $self = shift;
    my $conf = $self->config;
    my %hash = @_;
    
    my $inst;
    my $tmpl = {
        installation => { 
            required => 1, store => \$inst,
            allow => sub { UNIVERSAL::isa( shift(), 'JIB::Installation' ) } },
    };
    
    check( $tmpl, \%hash ) or error( Params::Check->last_error ), return;
    
    ### install check
    if( $inst->is_installed( package => $self ) ) {
        error("Package '". $self->package ."' is already installed --skipping");
        return 1;
    }        
    
    ### install the archive
    my $inst_pkg;
    {   ### extract to a temp dir
        my $my_tmp_dir = File::Spec->catdir( $conf->temp_dir . "$$" );
        system( qq[mkdir -p $my_tmp_dir] )                  and die $?;
        
        
        ### extract the archive to the temp dir
        system( qq[tar -f ] . $self->file . qq[ -C $my_tmp_dir -xz]) and die $?;
    
        my $meta_dir = $inst->control_dir( $self->package );
        my $data     = $conf->archive_data;
        my $control  = $conf->archive_control;

        ### extract the meta info
        ### XXX extract to $Builddir first, THEN copy later if all goes well
        {   
            my $packlist    = $inst->files_list( $self->package );
        
            system( qq[mkdir -p $meta_dir] )                and die $?;
            ### XXX need status dir like dpkg
            system( qq[tar -f $my_tmp_dir/$control -C $meta_dir -xz] )
                                                            and die $?;
            ### write a .packlist equiv
            system( qq[tar -f $my_tmp_dir/$data -C $meta_dir -tz |] .
                    qq[xargs -I % echo ] . $inst->dir . qq[/%] . 
                    qq[ >> $packlist] )   
                                                            and die $?;
        }

=begin comment

        {   
            ### dependencies satisfied?
            my $info = LoadFile( 
                File::Spec->catdir( $meta_dir, $config->meta_file ) );
            
            my %avail = map { $_->{package} => $_ } LoadFile( $Available );
            for my $depends ( list_dependencies( $info ) ) {
                ### XXX split depends: lines and objectified dependencies
                ### for better diagnostics
                die "Dependency '$depends->{package}' not satisfied " .
                    "for '$path'" unless $avail{ $depends->{package} };
            }
        }

=cut

        ### extract the code
        {   ### XXX we should *build* things here too
            system( qq[tar -f $my_tmp_dir/$data -C ] . 
                        $conf->compile_dir . q[ -xz] );

            ### preinst hook
            my $preinst = $meta_dir->file( $conf->preinst );
            if( -e $preinst && -s _ ) {
                system( qq[ $^X $preinst ] )                    and die $?;
            }
            
            ### XXX we should build a binary package here, instead of
            ### just doing a cp -R
            my $src = $conf->compile_dir->subdir( $self->package );
            system( qq[cp -R $src ]. $inst->dir )     and die $?;     
            
            ### register the installation
            ### do this AFTER installing, duh!
            $inst_pkg = $inst->register( package => $self ) 
                or error("Could not register package"), return;
            
            
            my $postinst = $meta_dir->file( $conf->postinst );
            if( -e $postinst && -s _ ) {
                system( qq[ $^X $postinst ] )                   and die $?;
            }    

            ### clean up the builddir
            system( qq[rm -rf $src] )                           and die $?;

        }

        ### clean up the temp dir 
        system( qq[rm -rf $my_tmp_dir] )                        and die $?;
    }
    
    return $inst_pkg;
}

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
