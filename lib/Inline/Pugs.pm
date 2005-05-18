package Inline::Pugs;

use strict;
use vars qw<$VERSION @ISA>;

@ISA     = 'Inline';
$VERSION = Perl6::Pugs->VERSION;

use Perl6::Pugs;
use Data::Dumper;
use IPC::Open2;
use constant MAGIC =>
    'my$Z= =$*IN;while 1{$_=perl eval eval=$*IN;say$!//$_;print$Z;flush$*OUT}';
use constant COOKIE => rand();

sub register {
    return {
        language => 'Pugs',
        aliases  => [ qw(pugs) ],
        type     => 'interpreted',
        suffix   => 'p6',
    };
}

sub validate { }

sub build {
    my $self = shift;
    my $path = "$self->{API}{install_lib}/auto/$self->{API}{modpname}";
    my $obj  = $self->{API}{location};
    $self->mkpath($path)                   unless -d $path;
    $self->mkpath($self->{API}{build_dir}) unless -d $self->{API}{build_dir};
    local *OBJECT;
    open(OBJECT, ">$obj") or die "Unable to open object file: $obj : $!";
    close(OBJECT) or die "Unable to close object file: $obj : $!";
}

sub load {
    my $self = shift;
    my $code = $self->{API}{code};
    my $pkg  = $self->{API}{pkg} || 'main';
    my $pid  = $self->{pid} ||= $self->init_pugs;
    $self->eval_pugs($code);

    # now try to figure out what functions are toplevel...
    # XXX - bloody hack for now

    no strict 'refs';
    foreach my $sym ($code =~ /^\s*sub\s+(\w+)\s+/mg) {
        *{"$pkg\::$sym"} = sub {
            local $Data::Dumper::Terse = 1;
            my @args = map { $self->quote_pugs(Dumper($_)).'.eval' } @_;
            $self->eval_pugs(
                "$sym(".join(',', @args).")"
            );
        }
    }
}

sub init_pugs {
    my $self = shift;
    my $pid = open2(\*OUT, \*IN, 'pugs', '-e', MAGIC);
    print IN COOKIE, "\n";
    return $pid;
}

sub eval_pugs {
    my $self = shift;
    print IN $self->quote_pugs($_[0]), "\n";
    local $/ = COOKIE;
    my $out = substr(<OUT>, 0, -length($/));
    $out =~ s{\n+$}{};
    $out =~ s{^\n+}{};
    die $out if $out =~ /\n/;
    return eval $out;
}

sub quote_pugs {
    my $self = shift;
    my $q = join '', map { sprintf("\\x%02X", ord) } split(//, $_[0]);
    return qq["$q"];
}

1;
