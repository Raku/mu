package Module::Install;
$VERSION = '0.36';
use FindBin;

die << "." unless $INC{join('/', inc => split(/::/, __PACKAGE__)).'.pm'};
Please invoke ${\__PACKAGE__} with:

    use inc::${\__PACKAGE__};

not:

    use ${\__PACKAGE__};

.

use strict 'vars';
use Cwd ();
use File::Find ();
use File::Path ();

@inc::Module::Install::ISA = 'Module::Install';
*inc::Module::Install::VERSION = *VERSION;

sub import {
    my $class = shift;
    my $self = $class->new(@_);

    if (not -f $self->{file}) {
        require Cwd;
        die << ".";
*** Error: $self->{file} does not exist!
    Current directory is @{[Cwd::abs_path(Cwd::cwd())]}
    Program path is @{[Cwd::abs_path($FindBin::Bin)]}
*** Please report this issue to <perl6-compiler\@perl.org>.
.
        require "$self->{path}/$self->{dispatch}.pm";
        File::Path::mkpath("$self->{prefix}/$self->{author}");
        $self->{admin} = 
          "$self->{name}::$self->{dispatch}"->new(_top => $self);
        $self->{admin}->init;
        @_ = ($class, _self => $self);
        goto &{"$self->{name}::import"};
    }

    *{caller(0) . "::AUTOLOAD"} = $self->autoload;

    # Unregister loader and worker packages so subdirs can use them again
    delete $INC{"$self->{file}"};
    delete $INC{"$self->{path}.pm"};
}

sub autoload {
    my $self = shift;
    my $caller = caller;

    my $cwd = Cwd::cwd();
    my $sym = "$caller\::AUTOLOAD";

    $sym->{$cwd} = sub {
        my $pwd = Cwd::cwd();
        if (my $code = $sym->{$pwd}) {
            goto &$code unless $cwd eq $pwd; # delegate back to parent dirs
        }
        $$sym =~ /([^:]+)$/ or die "Cannot autoload $caller";
        unshift @_, ($self, $1);
        goto &{$self->can('call')} unless uc($1) eq $1;
    };
}

use Cwd qw(cwd abs_path);
sub new {
    my ($class, %args) = @_;

    # ignore the prefix on extension modules built from top level.
    delete $args{prefix}
      unless Cwd::abs_path(Cwd::cwd()) eq Cwd::abs_path($FindBin::Bin);

    return $args{_self} if $args{_self};

    $args{dispatch} ||= 'Admin';
    $args{prefix}   ||= 'inc';
    $args{author}   ||= '.author';
    $args{bundle}   ||= 'inc/BUNDLES';
    $args{base}     ||= Cwd::abs_path($FindBin::Bin);

    $class =~ s/^inc:://;
    $args{name}     ||= $class;
    $args{version}  ||= $class->VERSION;

    unless ($args{path}) {
        $args{path}  = $args{name};
        $args{path}  =~ s!::!/!g;
    }
    $args{file}     ||= "$args{base}/$args{prefix}/$args{path}.pm";

    bless(\%args, $class);
}

sub call {
    my $self   = shift;
    my $method = shift;
    my $obj = $self->load($method) or return;

    unshift @_, $obj;
    goto &{$obj->can($method)};
}

sub load {
    my ($self, $method) = @_;

    my $extensions = $self->{extensions} || [];
    unless (@$extensions) {
        $self->load_extensions("$self->{prefix}/$self->{path}", $self);
    }

    foreach my $obj (@{$self->{extensions}}) {
        return $obj if $obj->can($method);
    }

    my $admin = $self->{admin} or die << "END";
The '$method' method does not exist in the '$self->{prefix}' path!
Please remove the '$self->{prefix}' directory and run $0 again to load it.
END

    my $obj = $admin->load($method, 1);
    push @{$self->{extensions}}, $obj;

    $obj;
}

sub load_extensions {
    my ($self, $path, $top_obj) = @_;
    $path = "$self->{base}/$path";

    unshift @INC, $self->{prefix}
        unless grep { $_ eq $self->{prefix} } @INC;

    foreach my $rv ($self->find_extensions($path)) {
        my ($file, $pkg) = @{$rv};
        next if $self->{pathnames}{$pkg};

        eval { require $file; 1 } or (warn($@), next);
        $self->{pathnames}{$pkg} = delete $INC{$file};
        push @{$self->{extensions}}, $pkg->new( _top => $top_obj );
    }
}

sub find_extensions {
    my ($self, $path) = @_;
    my @found;

    File::Find::find(sub {
        my $file = $File::Find::name;
        return unless $file =~ m!^\Q$path\E/(.+)\.pm\Z!is;
        return if $1 eq $self->{dispatch};

        $file = "$self->{path}/$1.pm";
        my $pkg = "$self->{name}::$1"; $pkg =~ s!/!::!g;
        push @found, [$file, $pkg];
    }, $path) if -d $path;

    @found;
}

1;

__END__
