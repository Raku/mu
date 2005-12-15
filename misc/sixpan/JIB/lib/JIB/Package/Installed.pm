package JIB::Package::Installed;

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



### only called form JIB::Package
sub new {
    my $self    = shift;
    my $conf    = $self->config;
    my %hash    = @_;
    
    my $meta; my $inst;
    my $tmpl = {
        meta    => { required => 1, store => \$meta, allow => ISA_JIB_META },
        installation
                => { required => 1, store => \$inst,
                    allow => ISA_JIB_INSTALLATION 
        },
    };
    
    my $args = check( $tmpl, \%hash ) 
                or error( Params::Check->last_error ), return;

    $self->mk_accessors( qw[files alternative installation] );
    
    while( my($acc,$val) = each %$args ) {
        $self->$acc( $val );
    }
    
    $self->package( $self->meta->package );
    
    ### XXX flesh out this format better
    {   my @files = eval {
            ### XXX CONFIG!!!!
            ### XXX this should be INSTALLATION SPECIFIC!!!
            my $file = $inst->files_list( $meta->package );
            open my $fh, $file or die "Could not open '$file': $!";
            do { local $/; <$fh> }
        };
        $@ and error( "Could not load files list: $@" ), return;
        $self->files( \@files );
        
        my ($alt) = grep { $_->package eq $meta->package }
                        @{ $inst->registered_alternatives };
        $self->alternative( $alt );
    }
    
    return $self;
}

=head2 $pkg->install( ... )

=cut

sub install {
    my $self = shift;
    msg("Package already installed");
    return $self;
}    

sub uninstall {
    my $self = shift;
    my $inst = $self->installation;
    my $conf = $self->config;

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
    {   open my $fh, $inst->files_list( $self->package )            or die $!;
    
        my $prerm = $inst->control_dir( $self->package )
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
        my $postrm = $inst->control_dir( $self->package )
                          ->file( $conf->postrm );
        if( -e $postrm && -s _ ) {
            system( qq[ $^X $postrm ] )                             and die $?;
        }
    }


    ### remove alternatives and relink if needed
    ### XXX doesn't do manpages yet
    ### XXX doens't check the AUTO flag yet for link management
    LINKING: {   
    
        ### XXX make simple Installation-> method
        my $alt; 
        my @alts = map  { $_->[0] } # return the object
                   grep { $_->[1] } # flag toggled?
                   map  { $_->package eq $self->package
                            ? do { $alt = $_; [ $_, 0 ] }
                            : [ $_, 1 ]
                    } @{ $inst->registered_alternatives }; 
        $inst->registered_alternatives( \@alts );

        ### this package didn't provide any alternatives
        last LINKING unless $alt;
        
        ### XXX this should probably be done in one go, so we don't
        ### have a situation where no 'script' is available
        
        ### unlink all the script files
        for my $script ( @{ $alt->bin || [] } ) {
            1 while unlink $conf->bin_dir->file( $script );
            1 while unlink $inst->alternatives_dir->file( $script ); 
        }      
        
        ### see if there's any other package that's now the default
        ### for this module
        ### make sure we dont see ourselves again, so grep that out
        my $new_alt;
        {   my $wanted = join '-',  $self->prefix, $self->name;

            ### find all packages that provide: a <prefix>-<name>
            ### implementation;
            my @list = @{ $inst->available };
            
            my @maybe;
            for my $test ( grep { $_->package ne $self->package } @list ) {
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
    
        ### XXX code duplication from installation->register
        my $my_bindir =  $inst->dir->subdir( $new_alt->package )->file('bin');

        last LINKING unless -d $my_bindir;

        my @bins;
        msg( "Relinking scripts/manpages to " . $new_alt->package, 1 );
        for ( qx[find $my_bindir -type f] ) {
            
            ### link from altdir to install dir
            ### then from pathdir, to altdir
            my $script = basename($_);
        
            my $alt = $inst->alternatives_dir->file( $script );
            my $bin = $conf->bin_dir->file( $script );
            system( qq[ln -fs $_ $alt] )                    and die $?;
            system( qq[ln -fs $alt $bin ] )                 and die $?;
            push @bins, $script;
        }
        
        
        push @alts, JIB::Alternative->new_from_struct( struct => 
                    { bin => \@bins, auto => 1, package => $self->package } );
            
        ### dump out alternatives again
        $inst->registered_alternatives( \@alts );
    }

    ### remove this package from the available list
    ### XXX temp file, then mv
    my @avail = grep { $_->package ne $self->package } @{ $inst->available };
    $inst->available( \@avail );
    
    ### move this all to Installation.pm
    $inst->write or return;
    
    
    ### unisntall metadata
    system( qq[rm -rf ] . $inst->control_dir( $self->package ) )    and die $?;

    msg( "Package '".$self->package."' and associated metadata removed");

    return 1;
}

1;


# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
