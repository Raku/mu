package Perldoc;
use Perldoc::Base -Base;

our $VERSION = '0.00_02';

sub doc_to_class {
    my %args = @_;
    my $class = $args{class} or die "No class!";
    eval "require $class";
    die $@ if $@;
    $args{receiver} ||= $class->new(%args);
    return $self->parse_to(%args);
}

sub parse_to {
    require Perldoc::Reader;
    require Perldoc::Parser;
    my %args = @_;
    $args{reader} ||= Perldoc::Reader->new(%args);
    $args{parser} ||= Perldoc::Parser->new(%args);
    return $args{parser}->parse;
}

sub doc_to_dom {
    $self->doc_to_class(@_, class => 'Perldoc::DOM');
}

sub doc_to_html {
    $self->doc_to_class(@_, class => 'Perldoc::HTML');
}

sub doc_to_xml {
    $self->doc_to_class(@_, class => 'Perldoc::XML');
}

sub doc_to_man {
    $self->doc_to_class(@_, class => 'Perldoc::Man');
}

sub doc_to_bytecode {
    $self->doc_to_class(@_, class => 'Perldoc::Bytecode');
}

=doc.kwid

= NAME

Perldoc - Perl Documentation Tools

= SYNOPSIS

    print Perldoc->new->doc_to_html(
        input => 'somthing.kwid',
    );

= DESCRIPTION

`Perldoc` is a set of tools that define and work with the /Perldoc
Information Model/. The tools provide parsers for various /Perldoc
Dialects/ (including Pod and Kwid), and formatters for various
output formats.

= AUTHORS

* Brian Ingerson <ingy@cpan.org>
* Sam Vilain <samv@cpan.org>

= COPYRIGHT

Copyright (c) 2005. Brian Ingerson. All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
