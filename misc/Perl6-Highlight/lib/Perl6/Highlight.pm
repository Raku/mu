package Perl6::Highlight;

# core modules & directives
use 5.010000;
use strict;
use warnings;
use English;
use Carp;
require Exporter;

# cpan modules
use File::Slurp qw(read_file);
use YAML::Dumper;
use Term::ANSIColor;

# Larry's STD.pm
use STD;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw() ] );
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
our @EXPORT = qw();

our $VERSION = '0.01';

sub new {
    my ($class, %options) = @ARG;
    my $self = bless(\%options, $class);
    return $self;
}

sub snippet_html {
    my ($self) = @ARG;
    croak "Not implemented " . $self->{p6code};
}

sub simple_html {
    my ($self) = @ARG;
    croak "Not implemented";
}

sub full_html {
    my ($self) = @ARG;
    croak "Not implemented";
}

sub ansi {
    my ($self) = @ARG;
    croak "Not implemented";
}

sub yaml {
    my ($self) = @ARG;
    croak "Not implemented";
}


1;
__END__

=head1 NAME

Perl6::Highlight - Perl 6 source code highlighter

=head1 SYNOPSIS

  use Perl6::Highlight;
  
  my $crayon = Perl6::Highlight->new(
    $file => \*STDIN
  );
  print $crayon->snippet_html;
  print $crayon->simple_html;
  print $crayon->full_html;
  print $crayon->ansi;
  print $crayon->yaml;

=head1 DESCRIPTION

Highlights Perl 6 source code using STD.pm into html, ansi-escaped text and YAML.

=head2 EXPORT

None by default.

=head1 SEE ALSO

Discussion about this module is usually in #perl6 @ irc.freenode.net

=head1 AUTHOR

Ahmad Zawawi, E<lt>ahmad.zawawi@gmail.comE<gt>

Written by Ahmad M. Zawawi (azawawi), Moritz Lenz and Paweł Murias (pmurias)

The project idea was inspired by Moritz Lenz (moritz)
See http://www.nntp.perl.org/group/perl.perl6.users/2008/07/msg788.html

The initial STD tree traversal code was written by Paweł Murias (pmurias).

The redspans traversal code was written by Larry Wall (TimToady).
redspans stands for "...'red' for "reductions", and 'spans' from the 
from/to span calculations"

The browser code was written by Ahmad M. Zawawi (azawawi)

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2008 by Ahmad Zawawi

This library is free software; you can redistribute it and/or modify
it under the same terms asssssss
at your option, any later version of Perl 5 you may have available.

=cut
