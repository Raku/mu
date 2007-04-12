package Perl6::Perldoc;

use version; $VERSION = qv('0.0.2');
use warnings;
use strict;
use re 'eval';

use Filter::Simple;

my $IDENT            = qr{ (?> [^\W\d] \w* )            }xms;
my $QUAL_IDENT       = qr{ $IDENT (?: :: $IDENT)*       }xms;
my $TO_EOL           = qr{ (?> [^\n]* ) (?:\Z|\n)       }xms;
my $HWS              = qr{ (?> [^\S\n]+ )               }xms;
my $OHWS             = qr{ (?> [^\S\n]* )               }xms;
my $BLANK_LINE       = qr{ ^ $OHWS $ | (?= ^ =)         }xms;
my $DIRECTIVE        = qr{ config | encoding | use      }xms;
my $OPT_EXTRA_CONFIG = qr{ (?> (?: ^ = $HWS $TO_EOL)* ) }xms;


# Recursive matcher for =DATA sections...

my $DATA_PAT = qr{
    ^ = 
    (?:
        begin $HWS DATA $TO_EOL
        $OPT_EXTRA_CONFIG
            (.*?)
        ^ =end $HWS DATA
    |
        for $HWS DATA $TO_EOL
        $OPT_EXTRA_CONFIG
            (.*?)
        $BLANK_LINE
    |
        DATA \s
            (.*?)
        $BLANK_LINE
    )
}xms;


# Recursive matcher for all other Perldoc sections...

my $POD_PAT; $POD_PAT = qr{
    ^ =
    (?:
        (?:(?:begin|for) $HWS)? END
        (?> .*) \z
    |
        begin $HWS ($IDENT) (?{ local $type = $^N}) $TO_EOL
        $OPT_EXTRA_CONFIG
            (?: ^ (??{$POD_PAT}) | . )*?
        ^ =end $HWS (??{$type}) $TO_EOL
    |
        for $HWS $TO_EOL
        $OPT_EXTRA_CONFIG
            .*?
        $BLANK_LINE
    |
        ^ $DIRECTIVE $HWS $TO_EOL
        $OPT_EXTRA_CONFIG
    |
        ^ (?! =end) =$IDENT $HWS $TO_EOL
            .*?
        $BLANK_LINE
    )
}xms;


FILTER {
    my @DATA;

    # Extract DATA sections, deleting them but preserving newlines...
    s{ ($DATA_PAT) }{
        my ($data_block, $contents) = ($1,$+);

        push @DATA, $contents;
        $data_block =~ tr[\n\0-\377][\n]d;
        $data_block;
    }gxmse;

    # Collect all declared package names...
    my %packages = (main=>1);
    s{ (\s* package \s+ ($QUAL_IDENT)) }{
        my ($package_decl, $package_name) = ($1,$2);
        $packages{$package_name} = 1;
        $package_decl;
    }gxmse;

    # Delete all other pod sections, preserving newlines...
    s{ ($POD_PAT) }{ my $text = $1; $text =~ tr[\n\0-\377][\n]d; $text }gxmse;

    # Consolidate data and open a filehandle to it...
    local *DATA_glob;
    my $DATA_as_str = join q{}, @DATA;
    *DATA_glob = \$DATA_as_str;
    *DATA_glob = \@DATA;
    open *DATA_glob, '<', \$DATA_as_str
        or require Carp and Carp::croak "Can't set up *DATA handle ($!)";

    # Alias each package's *DATA, @DATA, and $DATA...
    for my $package (keys %packages) {
        no strict 'refs'; 
        *{$package.'::DATA'} = *DATA_glob;
    }
}

__END__

=head1 NAME

Perl6::Perldoc - Use Perl 6 documentation in a Perl 5 program


=head1 VERSION

This document describes Perl6::Perldoc version 0.0.2


=head1 SYNOPSIS

    use Perl6::Perldoc;

    =comment
        Now you can use Perl 6 style documentation

    =for DATA
        Including Perl 6 style DATA sections

    print <DATA>;

    =for DATA
        Of which there can be more than one

  
=head1 DESCRIPTION

This module preprocesses your code from the point at which the module is
first used, stripping out any Perl 6 documentation (as specified in
Synopsis 26).

This means that, so long as your program starts with:

    use Perl6::Perldoc;

you can document it using the new Pod mark-up notation and it will still
run correctly under the Perl 5 interpreter.

In addition, the module detects any C<=DATA> sections in the stripped
documentation and makes them available to your program in three ways:

=over 

=item *

As a single concatentated string, in the C<$DATA> package variable

=item *

As a sequence of strings (one per C<=DATA> block) in the C<@DATA> package
variable

=item *

As a single concatenated input stream in the C<*DATA> filehandle.

=back


=head1 INTERFACE 

None. You C<use> the module and it takes care of everything.


=head1 DIAGNOSTICS

=over

=item C<< Can't set up *DATA handle (%s) >>

The filter found at least one C<=DATA> block, but was unable to create a
C<*DATA> filehandle in the caller's namespace (for the reason specified in the
parens).

=back


=head1 CONFIGURATION AND ENVIRONMENT

Perl6::Perldoc requires no configuration files or environment variables.


=head1 DEPENDENCIES

Requires the C<version> module and the standard module C<Filter::Simple>.


=head1 INCOMPATIBILITIES

None reported.


=head1 LIMITATIONS

Unlike Perl 6 itself:

=over

=item * 

This module does not make every Pod block available to the surrounding
program, only the C<=DATA> blocks. This is to avoid the unacceptably
slow compilation speed that would result from attempting to fully parse
the entire embedded Pod markup and then construct an internal
representation of it.

=item *

The contents of C<=DATA> blocks appear in the global variables
C<$DATA> and C<@DATA>, and the global C<*DATA> filehandle, rather than
in a special C<$?DATA> object. These variables and filehandle are
accessible from C<main> and in every other package that is explicitly
declared in the file.

=item *

This module does not honour C<=encoding> directives.

=back


=head1 BUGS

No bugs have been reported.

Please report any bugs or feature requests to
C<bug-perl6-pod@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org>.


=head1 CRUEL IRONY

The module itself had to be documented in Perl 5 POD


=head1 AUTHOR

Damian Conway  C<< <DCONWAY@CPAN.org> >>


=head1 LICENCE AND COPYRIGHT

Copyright (c) 2007, Damian Conway C<< <DCONWAY@CPAN.org> >>. All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.


=head1 DISCLAIMER OF STABILITY

This module will attempt to track any future changes to the Perl 6
specification. Hence its features and the Pod syntax it recognizes may
change in future releases.


=head1 DISCLAIMER OF WARRANTY

BECAUSE THIS SOFTWARE IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE SOFTWARE, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
PROVIDE THE SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE
ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE IS WITH
YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL
NECESSARY SERVICING, REPAIR, OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE SOFTWARE AS PERMITTED BY THE ABOVE LICENCE, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL,
OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE
THE SOFTWARE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.
