package JIB::Package;

use strict;
use warnings;
use base 'Object::Accessor';

use JIB::Constants;
use JIB::Config;

use JIB::Package::Source;
use JIB::Package::Binary;
use JIB::Package::Installed;

use File::Spec;
use File::Basename          qw[basename];
use Params::Check           qw[check];
use Log::Message::Simple    qw[:STD];
use YAML                    qw[LoadFile];

use Data::Dumper;

$Data::Dumper::Indent = 1;

my $Package_re = qr/^(\w+)     - # prefix
                    ([\w-]+?)  - # package name
                    ([\d.]+)   - # version
                    (\w+\+\S+) $ # authority
                /smx;

=head1 NAME

    JIB::Package

=head1 DESCRIPTION

Base class for:

    JIB::Package::Source
    JIB::Package::Binary
    JIB::Package::Installed

=head1 ACCESSORS 

=head2 package

Set the name of the full package package. For example:

    p5-foo-1-cpan+kane

=head1 METHODS

=head2 $pkg = JIB::Package->new( file => /path/to/jib | meta => META_OBJ );

Returns a JIB::Package::Source on a jib file, and a JIB::Package::Installed
on a META_OBJ.

XXX needs verification
XXX needs binary package recognition

=cut

{   my $tmpl = {
        file    => { allow => FILE_EXISTS },
        meta    => { allow => sub { UNIVERSAL::isa(shift(), 'JIB::Meta') } },
        config  => { allow => sub { UNIVERSAL::isa(shift(), 'JIB::Config') } },
        package => { allow => $Package_re, no_override => 1 }
    };


    sub new {
        my $class = shift;
        my %hash  = @_;
        
        my $args = check( $tmpl, \%hash ) 
                        or error( Params::Check->last_error ), return;
        
        ### XXX need better checks
        
        ### XXX create an object::accessor object, blessed in the right class
        my $obj = $args->{file}
            ? JIB::Package::Source->Object::Accessor::new
            : JIB::Package::Installed->Object::Accessor::new;
    
        return unless $obj;

        ### create accessors
        my %acc = map { $_ => $tmpl->{$_}->{allow} } keys %$tmpl;
        $obj->mk_accessors( \%acc );
        
        ### set the config
        $obj->config( JIB::Config->new );
        
        ### call the objects new method and return it
        return $obj->new( %hash );
        
    }
}    

=head2 $meta = $pkg->extract_meta_object;

=cut

sub extract_meta_object {
    my $self = shift;
    my $conf = $self->config;
    my $meta = $self->meta;

    ### if we didn't get a meta object, we'll fetch it from the .jib
    unless( $meta ) {
        
        ### installed packages don't have a 'file' method, but they /should/
        ### have a meta object at all times
        unless( $self->can('file') ) {
            error("No file associated with this object -- " .
                  "can not extract meta object" );
            return;
        }
        
        ### extract to a temp dir
        my $my_tmp_dir = File::Spec->catdir( $conf->temp_dir . "$$" );
        system( qq[mkdir -p $my_tmp_dir] )                      and die $?;
        
        ### extract the archive to the temp dir
        system( qq[tar -f ] . $self->file . qq[ -C $my_tmp_dir -xz]) 
                                                                and die $?;

        ### extract the meta info
        my $control  = $conf->archive_control;
        system( qq[tar -f $my_tmp_dir/$control -C $my_tmp_dir -xz] )
                                                                and die $?;
              
        $meta = eval { LoadFile( File::Spec->catfile( 
                                    $my_tmp_dir,
                                    $conf->meta_file )
                ) };
        $@ and error( "Could not load meta file from archive: $@" ), return;
    
        $self->meta(JIB::Meta->new_from_struct(struct => $meta)) or return;
        system( "rm -rf $my_tmp_dir" )                          and die $?;
    }

    return $meta;
}

=head2 package_re

=head2 prefix

=head2 name

=head2 version

=head2 authority

=cut

### XXX could autogenerate
{   
    sub package_re { $Package_re };

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

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
