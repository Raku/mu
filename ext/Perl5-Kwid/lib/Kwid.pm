package Kwid;
use strict;
use warnings;
use Kwid::Base;
use base 'Kwid::Base';

our $VERSION = '0.00_01';

sub kwid_to_html {
    require Kwid::HTML;
    my $self = shift;
    my $buffer = '';
    my $loader = Kwid::HTML->new(
        output => \$buffer,
        complete => 0,
    );
    return $self->parse_to(@_ => $loader);
}

sub kwid_to_xml {
    require Kwid::XML;
    my $self = shift;
    return $self->parse_to(@_ => die 'Kwid::XML');
}

sub kwid_to_man {
    require Kwid::Man;
    my $self = shift;
    return $self->parse_to(@_ => die 'Kwid::Man');
}

sub kwid_to_bytecode {
    require Kwid::Bytecode;
    my $self = shift;
    return $self->parse_to(@_ => die 'Kwid::Bytecode');
}

sub kwid_to_ast {
    require Kwid::AST;
    my $self = shift;
    return $self->parse_to(@_ => die 'Kwid::AST');
}

sub parse_to {
    require Kwid::Parser;
    my $self = shift;
    my $kwid = shift;
    my $loader = shift;
    my $parser = Kwid::Parser->new(
        input => \$kwid,
        loader => $loader,
    );
    return $parser->parse;
}

1;

__DATA__

=kwid = NAME

Kwid - Kwiki Documentation Format

= SYNOPSIS

= DESCRIPTION

= AUTHOR

Brian Ingerson <ingy@cpan.org>

= COPYRIGHT

Copyright (c) 2005. Brian Ingerson. All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
