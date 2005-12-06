package JIB::Package;

use strict;
use warnings;
use base 'Object::Accessor';

use JIB::Constants;
use JIB::Config;
use JIB::Installation;


use File::Spec;
use File::Basename          qw[basename];
use Params::Check           qw[check];
use Log::Message::Simple    qw[:STD];
use YAML                    qw[LoadFile];

my $Package_re = qr/^(\w+)     - # prefix
                    ([\w-]+?)  - # package name
                    ([\d.]+)   - # version
                    (\w+\+\S+) $ # authority
                /smx;

=head1 ACCESSORS 

=head2 package

Set the name of the full package package. For example:

    p5-foo-1-cpan+kane

=head1 METHODS

=head2 $pkg = JIB::Package->new( file => PACKAGE_NAME )

=cut

{   my $config = JIB::Config->new;

    my %acc = (
        package => $Package_re,
        file    => FILE_EXISTS,
        config  => sub { UNIVERSAL::isa( shift(), 'JIB::Config' ) },
        meta    => sub { UNIVERSAL::isa( shift(), 'JIB::Meta' ) },
    );        

    sub new {
        my $class   = shift;
        my %hash    = @_;
        
        my $file; my $meta;
        my $tmpl = {
            file    => { required => 1, allow => $acc{file}, store => \$file },
            config  => { no_override => 1, default => $config },
            meta    => { allow => $acc{meta}, store => \$meta },
        };
        
        my $args = check( $tmpl, \%hash ) 
                    or error( Params::Check->last_error ), return;

        ### set up the object + accessors        
        my $self = $class->SUPER::new;
        $self->mk_accessors( \%acc );
        
        while( my($acc,$val) = each %$args ) {
            $self->$acc( $val );
        }

        ### if we didn't get a meta object, we'll fetch it from the .jib
        unless( $meta ) {
            ### extract to a temp dir
            my $my_tmp_dir = File::Spec->catdir( $config->temp_dir . "$$" );
            system( qq[mkdir -p $my_tmp_dir] )                      and die $?;
            
            ### extract the archive to the temp dir
            system( qq[tar -f ] . $self->file . qq[ -C $my_tmp_dir -xz]) 
                                                                    and die $?;

            ### extract the meta info
            my $control  = $config->archive_control;
            system( qq[tar -f $my_tmp_dir/$control -C $my_tmp_dir -xz] )
                                                                    and die $?;
                  
            $meta = eval { LoadFile( File::Spec->catfile( 
                                        $my_tmp_dir,
                                        $config->meta_file )
                    ) };
            $@ and error( "Could not load meta file from archive: $@" ), return;
        
            $self->meta(JIB::Meta->new_from_struct(struct => $meta)) or return;
            system( "rm -rf $my_tmp_dir" )                          and die $?;
        }
        
        $self->package( $self->meta->package );
        
        return $self;
    }
}


=head2 prefix

=head2 name

=head2 version

=head2 authority

=cut

### XXX could autogenerate
{   

    sub prefix {
        return $1 if shift->package() =~ $Package_re;
    }

    sub name {
        return $2 if shift->package() =~ $Package_re;
    }

    sub version {
        return $3 if shift->package() =~ $Package_re;
    }
    
    sub authority {
        return $4 if shift->package() =~ $Package_re;
    }
}    

=head2 $pkg->install( ... )

=cut

### XXX perl-ify
sub install {
    my $self = shift;
    my $conf = $self->config;
    my $inst = JIB::Installation->new;

    ### install check
    if( $inst->is_installed( package => $self ) ) {
        error("Package '". $self->package ."' is already installed --skipping");
        return 1;
    }        
    
    ### install the archive
    {   ### extract to a temp dir
        my $my_tmp_dir = File::Spec->catdir( $conf->temp_dir . "$$" );
        system( qq[mkdir -p $my_tmp_dir] )                  and die $?;
        
        
        ### extract the archive to the temp dir
        system( qq[tar -f ] . $self->file . qq[ -C $my_tmp_dir -xz]) and die $?;
    
        my $meta_dir = File::Spec->catdir( $conf->control . $self->package );
        my $data     = $conf->archive_data;
        my $control  = $conf->archive_control;

        ### extract the meta info
        ### XXX extract to $Builddir first, THEN copy later if all goes well
        {   
            my $packlist    = $conf->files_list;
        
            system( qq[mkdir -p $meta_dir] )                and die $?;
            ### XXX need status dir like dpkg
            system( qq[tar -f $my_tmp_dir/$control -C $meta_dir -xz] )
                                                            and die $?;
            ### write a .packlist equiv
            system( qq[tar -f $my_tmp_dir/$data -C $meta_dir -tz |] .
                    qq[xargs -I % echo ] . $conf->perl_site_dir . 
                    qq[ >> $meta_dir/packlist] )   
                                                            and die $?;
            
            {   
=pod                            
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
=cut

                $inst->register( package => $self ) 
                    or error("Could not register package"), return;
            }

        }


    
        ### extract the code
        {   ### XXX we should *build* things here too
            system( qq[tar -f $my_tmp_dir/$data -C ] . 
                        $conf->compile_dir .q[ -xz] );
        
            ### preinst hook
            my $preinst = File::Spec->catfile( $meta_dir, $conf->preinst );
            if( -e $preinst && -s _ ) {
                system( qq[ $^X $preinst ] )                    and die $?;
            }
        
            ### XXX we should build a binary package here, instead of
            ### just doing a cp -R
            my $src = File::Spec->catdir( $conf->compile_dir, $self->package );
            system( qq[cp -R $src ]. $conf->perl_site_dir )     and die $?;     

            ### link files to $PATH/$MANPATH
            ### XXX symlink the manpages
            LINKING: {   

=pod            
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
=cut

                my $postinst = File::Spec->catfile($meta_dir, $conf->postinst);
                if( -e $postinst && -s _ ) {
                    system( qq[ $^X $postinst ] )               and die $?;
                }    
            }

        }


        ### clean up the temp dir
        system( qq[rm -rf $my_tmp_dir] )                    and die $?;
    }
    
    return 1;
}



1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
