package JIB::Constants;

use strict;

use Package::Constants;
use File::Spec;
use Log::Message::Simple        qw[:STD];

use vars qw[@EXPORT];
use base 'Exporter';

@EXPORT  = Package::Constants->list( __PACKAGE__ );

sub constants { @EXPORT };

use constant IS_CODEREF     => sub { ref $_[-1] eq 'CODE' };

use constant IS_FILE        => sub { return 1 if -e $_[-1] };                                            

use constant FILE_EXISTS    => sub {  
                                    my $file = $_[-1];
                                    return 1 if IS_FILE->($file);
                                    local $Carp::CarpLevel = 
                                            $Carp::CarpLevel+2;
                                    error(qq[File '$file' does not exist]);
                                    return;
                            };    

use constant FILE_READABLE  => sub {  
                                    my $file = $_[-1];
                                    return 1 if -e $file && -r _;
                                    local $Carp::CarpLevel = 
                                            $Carp::CarpLevel+2;
                                    error(qq[File '$file' is not readable ].
                                           q[or does not exist]);
                                    return;
                            };    
use constant IS_DIR         => sub { return 1 if -d $_[-1] };

use constant DIR_EXISTS     => sub { 
                                    my $dir = $_[-1];
                                    return 1 if IS_DIR->($dir);
                                    local $Carp::CarpLevel = 
                                            $Carp::CarpLevel+2;                                    
                                    error(q[Dir '$dir' does not exist]);
                                    return;
                            };   

use constant OPEN_FILE      => sub {
                                    my($file, $mode) = (@_, '');
                                    my $fh;
                                    open $fh, "$mode" . $file
                                        or error(
                                            "Could not open file '$file': $!");
                                    return $fh if $fh;
                                    return;
                            };      

use constant ISA_JIB_META   => sub { UNIVERSAL::isa(shift(), 'JIB::Meta') }; 
use constant ISA_JIB_CONFIG => sub { UNIVERSAL::isa(shift(), 'JIB::Config') };
use constant ISA_JIB_INSTALLATION
                            => sub { UNIVERSAL::isa( shift(),
                                                    'JIB::Installation') }; 
use constant ISA_JIB_PACKAGE
                            => sub { UNIVERSAL::isa(shift(), 'JIB::Package') }; 
use constant ISA_JIB_PACKAGE_SOURCE
                            => sub { UNIVERSAL::isa(shift(), 
                                                    'JIB::Package::Source') }; 
use constant ISA_JIB_PACKAGE_BINARY
                            => sub { UNIVERSAL::isa(shift(), 
                                                    'JIB::Package::Binary') }; 
use constant ISA_JIB_PACKAGE_INSTALLED
                            => sub { UNIVERSAL::isa(shift(), 
                                                    'JIB::Package::Installed')}; 

                            
1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
