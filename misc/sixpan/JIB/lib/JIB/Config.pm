package JIB::Config;

use strict;
use warnings;
use Cwd;
use Config;
use Path::Class ();
use base 'Object::Accessor';

my %config;
my $Cwd = Path::Class::dir(cwd());

use constant SUBDIR_OF_ROOT     => sub { shift()->root->subdir(    shift() )  };
use constant SUBDIR_OF_META     => sub { shift()->_meta_dir->subdir(shift() ) };
use constant SUBFILE_OF_META    => sub { shift()->_meta_dir->file(  shift() ) };

### %config is initialized with a [ default_value => return value ] pair
### the default_value is the value of initalization, the return value is
### a subroutine that build the return value based on the objects /current/
### value.. this allows subdirs to 'change' magically if a rootdir gets changed

### base dirs
### XXX touch the 'root' entry and the tests will run somewhere quite different!
$config{'root'}             = [ $Cwd->subdir('fr') ];
$config{'temp_dir'}         = [ tmp                     => SUBDIR_OF_ROOT ];
$config{'bin_dir'}          = [ $Config{bin}            => SUBDIR_OF_ROOT ];
$config{'compile_dir'}      = [ _builddir               => 
                                sub { shift()->temp_dir->subdir( shift() ) }
                            ];

### repository dirs/files
$config{'repo_pool'}        = [ Path::Class::dir('jibs')    ];
$config{'repo_index'}       = [ Path::Class::dir('dists')   ];
$config{'repo_index_file'}  = [ Path::Class::file('index')  ];
$config{'repo_index_groups'}= [ [map {Path::Class::dir($_)} 
                                    qw(author name prefix)] ];


### meta dirs/files
### These are dependant on the installation, so they are 
### marked privata.. their public equivalents live in Installation.pm
$config{'_meta_dir'}        = [ Path::Class::dir('_jib')            ];
$config{'_files_list'}      = [ Path::Class::file('files.list')     ];
$config{'_control'}         = [ control         => SUBDIR_OF_META   ];
$config{'_alternatives'}    = [ alternatives    => SUBDIR_OF_META   ];
$config{'_available'}       = [ available       => SUBFILE_OF_META  ];
$config{'_registered_alternatives'} 
                            = [ 'registered-alternatives' 
                                                => SUBFILE_OF_META  ];

### source package dirs
$config{'jib_dir'}          = [ Path::Class::dir('_jib') ];
### XXX need cp -R functionality fixed proper to use this ;(
#$config{'build_dir'}        = $config{'jib_dir'}->subdir('build');
$config{'build_dir'}        = [ Path::Class::dir('root-') ];

### package files/extensions
$config{'meta_ext'}         = [ '.info' ];
$config{'meta_file'}        = [ META => sub { $_[1] . $_[0]->meta_ext  } ];

$config{'archive_data'}     = [ 'data.tgz'      ];
$config{'archive_control'}  = [ 'control.tgz'   ];
$config{'archive_ext'}      = [ '.jib'          ];

$config{'preinst'}          = [ 'PREINST.pl'    ];
$config{'postinst'}         = [ 'POSTINST.pl'   ];
$config{'prerm'}            = [ 'PRERM.pl'      ];
$config{'postrm'}           = [ 'POSTRM.pl'     ];




### generate a list of methods
### see above for details
for my $func ( keys %config ) {
    no strict 'refs';
    *$func = sub {
        my $obj = shift;
        my $acc = '_'.$func;
        
        ### set the new value if provided
        $obj->$acc( $_[0] ) if @_;
        
        my $sub = $config{$func}->[1];
        return $sub 
            ? $sub->( $obj, $obj->$acc )
            : $obj->$acc;
    }
}

=head2 $config = JIB::Config->new

XXX Singleton

=cut

{   my $Obj;

    sub new {
        return $Obj if $Obj;
        
        my $self = shift;
        $Obj     = $self->SUPER::new();
    
    
        for my $key ( keys %config ) {
            my $acc = '_'.$key;
    
            ### XXX allow handlers?
            $Obj->mk_accessors( $acc );
            
            ### set to a sane default value
            $Obj->$acc( $config{$key}->[0] );
        }
        return $Obj;
    }
}

### add our own ls_accessors, so we don't give back the private accessors
sub ls_all_accessors { sort keys %config }
sub ls_accessors { grep { $_ !~ /^_/ } __PACKAGE__->ls_all_accessors }


1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
