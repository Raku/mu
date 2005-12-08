package JIB::Repository;

use strict;
use warnings;
use base 'Object::Accessor';
use JIB::Config;
use JIB::Package;
use JIB::Constants;

use YAML;
use Path::Class;
use Params::Check qw(check);
use Log::Message::Simple qw(:STD);
use Math::Combinatorics;

=head1 METHODS

=head2 $repo = JIB::Repository->new(...);

Required arguments:

=over 2

=item root

The root directory of the repository

=item pool

Path to the pool directory of the repository. $root/jibs if not specified.

=item pool_rel

The path to the pool directory that will be put into the index files. jibs/ if
not specified.

=item index

Path to the index directory. default: $root/dists

=item index_file

Path to the main index file. default: $root/dists/index

=back

=cut

{
    my $config = JIB::Config->new;

    sub new {
        my $class = shift;
        my %args = @_;

        my ($root, $pool, $pool_rel, $index, $index_file);
        my $tmpl = {
            root => {
                required    => 1,
                store       => \$root
            },
            config => {
                no_override => 1,
                default => $config
            },
            pool => {
                store => \$pool,
                #XXX allow?
            },
            pool_rel => {
                store => \$pool_rel,
            },
            index => {
                store => \$index
            },
            index_file => {
                store => \$index_file
            }
        };

        check($tmpl, \%args) or error(Params::Check->last_error), return;
        
        my $obj = $class->SUPER::new;
        $obj->mk_accessors(qw( root config pool pool_rel index index_file ));

        $obj->root(dir($root));
        $obj->pool( (defined $pool ? dir($pool) : $obj->root->subdir($config->repo_pool)) );
        $obj->index( (defined $index ? dir($index) : $obj->root->subdir($config->repo_index)) );
        $obj->index_file( (defined $index_file ? file($index_file) : $obj->index->file($config->repo_index_file)) );
        $obj->pool_rel( (defined $pool_rel ? dir($pool_rel) : $config->repo_pool) );
        $obj->config($config);

        return $obj;
    }
}

=head2 $repo->create()

Creates a new repository on the filesystem. Wipes out an existing one with the
same root path if it exists.

Returns a true value on success, otherwise something false.

=cut

sub create {
    my $self = shift;
    my $conf = $self->config;

    $self->root->rmtree();

    my @dirs = (
            $self->root,
            $self->pool,
            $self->index,
    );

    for my $dir (@dirs) {
        $dir->mkpath() or error($!), return;
    }

    return 1;
}

=head2 @ret = $repo->add_packages($pkg1, $pkg2, ...)

Adds a list of JIB::Package instances to the repository.

=cut

sub add_packages {
    my $self = shift;
    my @ret;

    push @ret, $self->add_package(package => $_) for @_;

    return @ret;
}

=head2 $repo->add_package(package => $pkg

=cut

sub add_package {
    my $self = shift;
    my %args = @_;
    my $pkg;

    my $tmpl = {
        package => {
            required    => 1,
            store       => \$pkg,
            allow       => sub { UNIVERSAL::isa(shift, 'JIB::Package') }
        }
    };
    
    check($tmpl, \%args) or error(Params::Check->last_error), return;

    if (-e $self->pool->file(file($pkg->file)->basename)) {
        error($pkg->package. " already exists in this repository");
        return;
    }

    File::Copy::copy($pkg->file, $self->pool) or error($!), return;
    $self->add_package_to_index(package => $pkg);

    return 1;
}

=head2 $repo->add_package_to_index(package => $pkg)

Adds a package to all necessary index files.

=cut

sub add_package_to_index { #TODO: compression of index files.
    my $self = shift;
    my %args = @_;
    my $pkg;

    my $tmpl = {
        package => {
            required    => 1,
            store       => \$pkg,
            allow       => sub { UNIVERSAL::isa(shift, 'JIB::Package') }
        }
    };

    check($tmpl, \%args) or error(Params::Check->last_error), return;

    for my $index ($self->index_file, $self->index_files(package => $pkg)) {
        unless (-e $index) {
            # create the directory an index file lives in (ignore errors (file exists))
            # and touch the index file.
            $index->dir->mkpath, $index->openw->close or error($!), return;
        }
        my @index_content = YAML::LoadFile($index);
        my $meta = $pkg->meta->to_struct;
        $meta->{archive} = $self->pool_rel->file(file($pkg->file)->basename)->stringify();
        push @index_content, $meta;
        YAML::DumpFile($index, @index_content);
    }
}

=head2 @index_files = $repo->index_files(package => $pkg)

Returns a list of all index_files that a given package belongs to.

=cut

sub index_files { #TODO: When using two properties (A and B) to group the dists
                  #      tree A/A_val/B/B_val.gz and B/B_val/A/A_val.gz are
                  #      equal. Use hard- or symlinks to minimize storage
                  #      requirements
    my $self = shift;
    my %args = @_;
    my $pkg;

    my $tmpl = {
        package => {
            required    => 1,
            store       => \$pkg,
            allow       => sub { UNIVERSAL::isa(shift, 'JIB::Package') }
        }
    };

    check($tmpl, \%args) or error(Params::Check->last_error), return;

    return map { $self->index->file($_) } $self->_index_files($pkg, $self->config->repo_index_groups, '');
}

sub _index_files {
    my ($self, $pkg, $groups, $cur) = @_;
    return unless $groups && @$groups;

    my @index_files;
    my %copy = map { $_ => $_ ne $cur } @$groups;
    my @copy = grep { $copy{$_} } keys %copy;
    for my $key (@copy) {
        my $path = dir($key)->subdir($pkg->meta->$key); #maybe use another mechanism to call arbitrary stuff on the package obj?
        push @index_files, file($path.'.index');
        push @index_files, map { $path->file($_) } $self->_index_files($pkg, \@copy, $key);
    }

    return @index_files;
}

=head2 $repo->add_files($file1, $file2, ...)

Adds a list of jib files to the repository.

=cut

sub add_files {
    my $self = shift;
    my @ret;
    
    push @ret, $self->add_file(file => $_) for @_;

    return @ret;
}

=head2 $repo->add_file(file => $file)

Adds a given file to the repository.

=cut

sub add_file {
    my $self = shift;
    my %args = @_;
    my $file;

    my $tmpl = {
        file => {
            required    => 1,
            store       => \$file,
            allow       => FILE_EXISTS
        }
    };

    check($tmpl, \%args) or error(Params::Check->last_error), return;

    my $pkg = JIB::Package->new(file => $file);

    return $self->add_package(package => $pkg);
}

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:              
