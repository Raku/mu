#!/usr/bin/perl

package Blondie::Nodes;
use base qw/Exporter/;

use strict;
use warnings;

use Carp ();
use Class::Inspector ();

use Blondie::ADTs;

our @EXPORT = qw/stub/;

sub stub { map { $_, Thunk(Stub($_)) } @_ }

sub mk_constructors {
    foreach my $class (@{ Class::Inspector->subclasses("Blondie::Node") }) {
        next unless $class =~ /^Blondie::(\w+)$/;
        next if __PACKAGE__->can($1);

        no strict 'refs';
        push @EXPORT, $1;
        *{$1} = sub { $class->new(@_) };
    }
}

mk_constructors();

{
    package Blondie::Node;

    use Digest ();
    use Storable ();
    use overload ();
    use Carp ();

    sub atomic { '' }

    sub fmap {
        Carp::croak "virtual method fmap called on @_";
    }

    sub str {
        Carp::croak "virtual method str called on @_";
    }

    my %DIGEST_CACHE;

    sub digest {
        my $self = shift;
        $DIGEST_CACHE{overload::StrVal($self)} ||= $self->compute_digest;
    }

    sub DESTROY {
        my $self = shift;
        delete $DIGEST_CACHE{overload::StrVal($self)};
    }

    sub compute_digest {
    my $self = shift;
        my $sha1 = Digest->new("SHA-1");

        {
            local *STDERR;
            local $Storable::forgive_me = 1;
            local $Storable::canonical = 1;
            local $Storable::Deparse = 1;

            $sha1->add( Storable::nfreeze($self) );
        }

        $sha1->hexdigest;
    }
}

{
    package Blondie::Node::Unit;
    use base qw/Blondie::Unit Blondie::Node/;

    package Blondie::Node::List;
    use base qw/Blondie::List Blondie::Node/;

    package Blondie::Node::Map;
    use base qw/Blondie::Map Blondie::Node/;
}

{
    package Blondie::Thunk;
    use base 'Blondie::Node::Unit';

    package Blondie::Sym;
    use base 'Blondie::Node::Unit';

    package Blondie::Param;
    use base 'Blondie::Node::Unit';

    package Blondie::App;
    use base 'Blondie::Node::List';

    package Blondie::Val;
    use base 'Blondie::Node::Unit';

    package Blondie::Seq;
    use base 'Blondie::Node::List';

    package Blondie::Stub;
    use base qw/Blondie::Node::Unit/;
}

=begin comment
    package Blondie::Env;
    use base 'Blondie::Node::Map';

    package Prim;
    use base qw/Map/;

    package Placeholder;
    use base 'Val';
    sub set { ${ $_[0] } = $_[1]; bless $_[0], "Val" }

=cut

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Nodes - 

=head1 SYNOPSIS

    use Blondie::Nodes;

=head1 DESCRIPTION

see L<Blondie> for a documentation of the node types

=head1 EXPORTS

For eaxch 

=cut


