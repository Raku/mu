package JIB::Constants;

use strict;

use Package::Constants;
use File::Spec;

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
                                    error(loc(  q[File '%1' does not exist],
                                                $file));
                                    return;
                            };    

use constant FILE_READABLE  => sub {  
                                    my $file = $_[-1];
                                    return 1 if -e $file && -r _;
                                    local $Carp::CarpLevel = 
                                            $Carp::CarpLevel+2;
                                    error( loc( q[File '%1' is not readable ].
                                                q[or does not exist], $file));
                                    return;
                            };    
use constant IS_DIR         => sub { return 1 if -d $_[-1] };

use constant DIR_EXISTS     => sub { 
                                    my $dir = $_[-1];
                                    return 1 if IS_DIR->($dir);
                                    local $Carp::CarpLevel = 
                                            $Carp::CarpLevel+2;                                    
                                    error(loc(q[Dir '%1' does not exist],
                                            $dir));
                                    return;
                            };   

use constant OPEN_FILE      => sub {
                                    my($file, $mode) = (@_, '');
                                    my $fh;
                                    open $fh, "$mode" . $file
                                        or error(loc(
                                            "Could not open file '%1': %2",
                                             $file, $!));
                                    return $fh if $fh;
                                    return;
                            };      
                            
1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
