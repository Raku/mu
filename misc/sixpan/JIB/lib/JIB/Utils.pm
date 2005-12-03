package JIB::Utils;

use strict;
use JIB::Constants;

use File::Copy              ();
use Params::Check           qw[check];
use Log::Message::Simple    qw[:STD];

=pod

=head1 NAME

JIB::Utils

=head1 METHODS

=head2 JIB::Utils->_mkdir( dir => '/some/dir' )

C<_mkdir> creates a full path to a directory.

Returns true on success, false on failure.

=cut

sub _mkdir {
    my $self = shift;

    my %hash = @_;

    my $tmpl = {
        dir     => { required => 1 },
    };

    my $args = check( $tmpl, \%hash ) or return;

    unless( can_load( modules => { 'File::Path' => 0.0 } ) ) {
        error( "Could not use File::Path! This module should be core!" );
        return;
    }

    eval { File::Path::mkpath($args->{dir}) };

    if($@) {
        chomp($@);
        error( qq[Could not create directory '%1': %2], $args->{dir}, $@ );
        return;
    }

    return 1;
}

=pod

=head2 JIB::Utils->_chdir( dir => '/some/dir' )

C<_chdir> changes directory to a dir.

Returns true on success, false on failure.

=cut

sub _chdir {
    my $self = shift;
    my %hash = @_;

    my $tmpl = {
        dir     => { required => 1, allow => DIR_EXISTS },
    };

    my $args = check( $tmpl, \%hash ) or return;

    unless( chdir $args->{dir} ) {
        error( q[Could not chdir into '%1'], $args->{dir} );
        return;
    }

    return 1;
}

=pod

=head2 JIB::Utils->_rmdir( dir => '/some/dir' );

Removes a directory completely, even if it is non-empty.

Returns true on success, false on failure.

=cut

sub _rmdir {
    my $self = shift;
    my %hash = @_;

    my $tmpl = {
        dir     => { required => 1, allow => IS_DIR },
    };

    my $args = check( $tmpl, \%hash ) or return;

    unless( can_load( modules => { 'File::Path' => 0.0 } ) ) {
        error( "Could not use File::Path! This module should be core!" );
        return;
    }

    eval { File::Path::rmtree($args->{dir}) };

    if($@) {
        chomp($@);
        error(qq[Could not delete directory '%1': %2], $args->{dir}, $@ );
        return;
    }

    return 1;
}

=pod

=head2 JIB::Utils->_perl_version ( perl => 'some/perl/binary' );

C<_perl_version> returns the version of a certain perl binary.
It does this by actually running a command.

Returns the perl version on success and false on failure.

=cut

sub _perl_version {
    my $self = shift;
    my %hash = @_;

    my $perl;
    my $tmpl = {
        perl    => { required => 1, store => \$perl },
    };

    check( $tmpl, \%hash ) or return;
    
    my $perl_version;
    ### special perl, or the one we are running under?
    if( $perl eq $^X ) {
        ### just load the config        
        require Config;
        $perl_version = $Config::Config{version};
        
    } else {
        my $cmd  = $perl .
                ' -MConfig -eprint+Config::config_vars+version';
        ($perl_version) = (`$cmd` =~ /version='(.*)'/);
    }
    
    return $perl_version if defined $perl_version;
    return;
}

=pod

=head2 JIB::Utils->_version_to_number( version => $version );

Returns a proper module version, or '0.0' if none was available.

=cut

sub _version_to_number {
    my $self = shift;
    my %hash = @_;

    my $version;
    my $tmpl = {
        version => { default => '0.0', store => \$version },
    };

    check( $tmpl, \%hash ) or return;

    return $version if $version =~ /^\.?\d/;
    return '0.0';
}

=pod

=head2 JIB::Utils->_whoami

Returns the name of the subroutine you're currently in.

=cut

sub _whoami { my $name = (caller 1)[3]; $name =~ s/.+:://; $name }

=pod

=head2 _get_file_contents( file => $file );

Returns the contents of a file

=cut

sub _get_file_contents {
    my $self = shift;
    my %hash = @_;

    my $file;
    my $tmpl = {
        file => { required => 1, store => \$file }
    };

    check( $tmpl, \%hash ) or return;

    my $fh = OPEN_FILE->($file) or return;
    my $contents = do { local $/; <$fh> };

    return $contents;
}

=pod JIB::Utils->_move( from => $file|$dir, to => $target );

Moves a file or directory to the target.

Returns true on success, false on failure.

=cut

sub _move {
    my $self = shift;
    my %hash = @_;

    my $from; my $to;
    my $tmpl = {
        file    => { required => 1, allow => [IS_FILE,IS_DIR],
                        store => \$from },
        to      => { required => 1, store => \$to }
    };

    check( $tmpl, \%hash ) or return;

    if( File::Copy::move( $from, $to ) ) {
        return 1;
    } else {
        error("Failed to move '%1' to '%2': %3", $from, $to, $!);
        return;
    }
}

=pod JIB::Utils->_copy( from => $file|$dir, to => $target );

Moves a file or directory to the target.

Returns true on success, false on failure.

=cut

sub _copy {
    my $self = shift;
    my %hash = @_;
    
    my($from,$to);
    my $tmpl = {
        file    =>{ required => 1, allow => [IS_FILE,IS_DIR],
                        store => \$from },
        to      => { required => 1, store => \$to }
    };

    check( $tmpl, \%hash ) or return;

    if( File::Copy::copy( $from, $to ) ) {
        return 1;
    } else {
        error("Failed to copy '%1' to '%2': %3", $from, $to, $!);
        return;
    }
}

=head2 JIB::Utils->_mode_plus_w( file => '/path/to/file' );

Sets the +w bit for the file.

Returns true on success, false on failure.

=cut

sub _mode_plus_w {
    my $self = shift;
    my %hash = @_;
    
    require File::stat;
    
    my $file;
    my $tmpl = {
        file    => { required => 1, allow => IS_FILE, store => \$file },
    };
    
    check( $tmpl, \%hash ) or return;
    
    ### set the mode to +w for a file and +wx for a dir
    my $x       = File::stat::stat( $file );
    my $mask    = -d $file ? 0100 : 0200;
    
    if( $x and chmod( $x->mode|$mask, $file ) ) {
        return 1;

    } else {        
        error("Failed to '%1' '%2': '%3'", 'chmod +w', $file, $!);
        return;
    }
}    

=head2 JIB::Utils->_vcmp( VERSION, VERSION );

Normalizes the versions passed and does a '<=>' on them, returning the result.

=cut

sub _vcmp {
    my $self = shift;
    my ($x, $y) = @_;
    
    s/_//g foreach $x, $y;

    return $x <=> $y;
}

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
