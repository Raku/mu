#!/usr/bin/perl

package Blondie::Emitter;
use base qw/Blondie::Reducer/;

use strict;
use warnings;



__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Emitter - emitter base class - not more than just a namespace.

=head1 SYNOPSIS

    # moot

=head1 DESCRIPTION

Emitters take a compiled program and serialize them into some format.

For example L<Blondie::Emitter::Pretty> emits ASCII text that is supposed to be
readable.

=cut


