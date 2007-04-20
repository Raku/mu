package Perl6::Perldoc::To::Text;

use warnings;
use strict;

package Perl6::Perldoc::Parser::ReturnVal;

sub to_text {
    my ($self, $internal_state) = @_;

    $internal_state ||= {};

    my $text_rep = $self->{tree}->to_text($internal_state);

    if (($internal_state->{note_count}||0) > 0) {
        $text_rep .= "\nNotes\n\n$internal_state->{notes}";
    }

    return $text_rep;
}

package Perl6::Perldoc::Root;  

my $INDENT = 4;

sub add_text_nesting {
    my ($self, $text, $depth) = @_;

    # Nest according to the specified nestedness of the block...
    if (my $nesting = $self->option('nested')) {
        $depth = $nesting * $INDENT;
    }

    # Or else default to one indent...
    elsif (!defined $depth) {
        $depth = $INDENT;
    }

    my $indent = q{ } x $depth;
    $text =~ s{^}{$indent}gxms;
    return $text;
}

sub _list_to_text {
    my ($list_ref, $state_ref) = @_;
    my $text = q{};
    for my $content ( @{$list_ref} ) {
        next if ! defined $content;
        if (ref $content) {
            $text .= $content->to_text($state_ref);
        }
        else {
            $text .= $content;
        }
    }
    $text =~ s{\A \n+}{}xms;
    $text =~ s{\n+ \z}{\n}xms;
    return $text;
}

sub to_text {
    my $self = shift;
    return $self->add_text_nesting(_list_to_text([$self->content], @_),0);
}

# Representation of file itself...
package Perl6::Perldoc::Document;  
    use base 'Perl6::Perldoc::Root';

# Ambient text around the Pod...
package Perl6::Perldoc::Ambient;  

sub to_text {
    return q{};
}

# Pod blocks...
package Perl6::Perldoc::Block;    

# Standard =pod block...
package Perl6::Perldoc::Block::pod;    

# Standard =para block (may be implicit)...
package Perl6::Perldoc::Block::para;   

sub to_text {
    my $self = shift;
    return "\n" . $self->SUPER::to_text(@_);
}

# Standard =code block (may be implicit)...
package Perl6::Perldoc::Block::code;   

sub min {
    my $min = shift;
    for my $next (@_) {
        $min = $next if $next < $min;
    }
    return $min;
}

sub to_text {
    my $self = shift;
    my $text = Perl6::Perldoc::Root::_list_to_text([$self->content],@_);
    my $left_space = min map { length } $text =~ m{^ [^\S\n]* (?= \S) }gxms;
    $text =~ s{^ [^\S\n]{$left_space} }{}gxms;
    return "\n" . $self->add_text_nesting($text, $INDENT);
}


# Standard =input block
package Perl6::Perldoc::Block::input;   

sub to_text {
    my $self = shift;
    my $text = Perl6::Perldoc::Root::_list_to_text([$self->content],@_);
    return "\n" . $self->add_text_nesting($text, $INDENT);
}


# Standard =output block
package Perl6::Perldoc::Block::output;   

sub to_text {
    my $self = shift;
    my $text = Perl6::Perldoc::Root::_list_to_text([$self->content],@_);
    return "\n" . $self->add_text_nesting($text, $INDENT);
}

# Standard =config block...
package Perl6::Perldoc::Config; 

sub to_text {
    return q{};
}

# Standard =table block...
package Perl6::Perldoc::Block::table; 

sub to_text {
    my $self = shift;
    my ($text) = $self->content;
    return "\n" . $self->add_text_nesting($text, $INDENT);
}


# Standard =head1 block...
package Perl6::Perldoc::Block::head1;  

sub to_text {
    my $self = shift;
    my $title = $self->SUPER::to_text(@_);
    $title =~ s{\A\s+|\s+\Z}{}gxms;
    $title =~ s{\s+}{ }gxms;
    my $number = $self->number;
    if (defined $number) {
        $title = "$number. $title";
    }
    return "\n\n$title\n";
}

# Standard =head2 block...
package Perl6::Perldoc::Block::head2;  

sub to_text {
    my $self = shift;
    my $title = $self->SUPER::to_text(@_);
    $title =~ s{\A\s+|\s+\Z}{}gxms;
    $title =~ s{\s+}{ }gxms;
    my $number = $self->number;
    if (defined $number) {
        $title = "$number. $title";
    }
    return "\n\n$title\n";
}

# Standard =head3 block...
package Perl6::Perldoc::Block::head3;  

sub to_text {
    my $self = shift;
    my $title = $self->SUPER::to_text(@_);
    $title =~ s{\A\s+|\s+\Z}{}gxms;
    $title =~ s{\s+}{ }gxms;
    my $number = $self->number;
    if (defined $number) {
        $title = "$number. $title";
    }
    return "\n\n$title\n";
}

# Standard =head4 block...
package Perl6::Perldoc::Block::head4;  

sub to_text {
    my $self = shift;
    my $title = $self->SUPER::to_text(@_);
    $title =~ s{\A\s+|\s+\Z}{}gxms;
    $title =~ s{\s+}{ }gxms;
    my $number = $self->number;
    if (defined $number) {
        $title = "$number. $title";
    }
    return "\n\n$title\n";
}

# Implicit list block...
package Perl6::Perldoc::Block::list;   
    use base 'Perl6::Perldoc::Root';

sub to_text {
    my $self = shift;
    return $self->add_text_nesting($self->SUPER::to_text(@_));
}


# Standard =item block...
package Perl6::Perldoc::Block::item;   

sub to_text {
    my $self = shift;

    my $counter = $self->number;
    $counter = $counter ? qq{$counter.} : q{*};

    my $body = $self->SUPER::to_text(@_);

    if (my $term = $self->term()) {
        $term = $self->term( {as_objects=>1} )->to_text(@_);
        if (length $counter) {
            $term =~ s{\A (\s* <[^>]+>)}{$1$counter. }xms;
        }
        my $body = $self->add_text_nesting($body);
        $body =~ s{\A \n+}{}xms;
        return "\n$term\n$body";
    }

    $body = $self->add_text_nesting($body, 1 + length $counter);
    $body =~ s{\A \n+}{}xms;
    $body =~ s{\A \s*}{$counter }xms;

    return "\n$body";
}

# Implicit toclist block...
package Perl6::Perldoc::Block::toclist;   
    use base 'Perl6::Perldoc::Root';

sub to_text {
    my $self = shift;
    
    # Convert list items to text, and return in an text list...
    my $text = join q{}, map {$_->to_text(@_)}  $self->content;

    return $self->add_text_nesting($text);
}


# Standard =tocitem block...
package Perl6::Perldoc::Block::tocitem;   

sub to_text {
    my $self = shift;

    my @title = $self->title;
    return "" if ! @title;
    
    my $title = Perl6::Perldoc::Root::_list_to_text(\@title, @_);

    return "* $title\n";
}

# Handle headN's and itemN's and tocitemN's...
for my $depth (1..100) {
    no strict qw< refs >;

    @{'Perl6::Perldoc::Block::item'.$depth.'::ISA'}
        = 'Perl6::Perldoc::Block::item';

    @{'Perl6::Perldoc::Block::tocitem'.$depth.'::ISA'}
        = 'Perl6::Perldoc::Block::tocitem';

    next if $depth < 5;
    @{'Perl6::Perldoc::Block::head'.$depth.'::ISA'}
        = 'Perl6::Perldoc::Block::head4';
}
# Handle headN's and itemN's
for my $depth (1..100) {
    no strict qw< refs >;
    @{'Perl6::Perldoc::Block::item'.$depth.'::ISA'}
        = 'Perl6::Perldoc::Block::item';
}

# Standard =nested block...
package Perl6::Perldoc::Block::nested;   


# Standard =comment block...
package Perl6::Perldoc::Block::comment;   

sub to_text {
    return q{};
}

# Standard SEMANTIC blocks...
package Perl6::Perldoc::Block::Semantic;
BEGIN {
    my @semantic_blocks = qw(
        NAME              NAMES
        VERSION           VERSIONS
        SYNOPSIS          SYNOPSES
        DESCRIPTION       DESCRIPTIONS
        USAGE             USAGES
        INTERFACE         INTERFACES
        METHOD            METHODS
        SUBROUTINE        SUBROUTINES
        OPTION            OPTIONS
        DIAGNOSTIC        DIAGNOSTICS
        ERROR             ERRORS
        WARNING           WARNINGS
        DEPENDENCY        DEPENDENCIES
        BUG               BUGS
        SEEALSO           SEEALSOS
        ACKNOWLEDGEMENT   ACKNOWLEDGEMENTS
        AUTHOR            AUTHORS
        COPYRIGHT         COPYRIGHTS
        DISCLAIMER        DISCLAIMERS
        LICENCE           LICENCES
        LICENSE           LICENSES
        TITLE             TITLES
        SECTION           SECTIONS
        CHAPTER           CHAPTERS
        APPENDIX          APPENDIXES       APPENDICES
        TOC               TOCS
        INDEX             INDEXES          INDICES
        FOREWORD          FOREWORDS
        SUMMARY           SUMMARIES
    );

    # Reuse content-to-text converter
    *_list_to_text = *Perl6::Perldoc::Root::_list_to_text;

    for my $blockname (@semantic_blocks) {
        no strict qw< refs >;

        *{ "Perl6::Perldoc::Block::${blockname}::to_text" }
            = sub {
                my $self = shift;

                my @title = $self->title();

                return "" if !@title;
                my $title = _list_to_text(\@title, @_);

                return "\n$title\n\n"
                     . _list_to_text([$self->content], @_);
            };
    }
}


# Base class for formatting codes...

package Perl6::Perldoc::FormattingCode; 

package Perl6::Perldoc::FormattingCode::Named; 

# Basis formatter...
package Perl6::Perldoc::FormattingCode::B;

sub to_text {
    my $self = shift;
    return '*' . $self->SUPER::to_text(@_) . '*';
}

# Code formatter...
package Perl6::Perldoc::FormattingCode::C;

sub to_text {
    my $self = shift;
    return '`' . $self->SUPER::to_text(@_) . '`';
}

# Definition formatter...
package Perl6::Perldoc::FormattingCode::D;

sub to_text {
    my $self = shift;
    return '/' . $self->SUPER::to_text(@_) . '/';
}


# Entity formatter...
package Perl6::Perldoc::FormattingCode::E;

my %is_break_entity = (
    'LINE FEED (LF)'       => 1,     LF  => 1,
    'CARRIAGE RETURN (CR)' => 1,     CR  => 1,
    'NEXT LINE (NEL)'      => 1,     NEL => 1,

    'FORM FEED (FF)'       => 10,    FF  => 10, 
);

my %is_translatable = (
    nbsp  => q{ },
    bull  => q{*},
    mdash => q{--},
    ndash => q{--},
);

# Convert E<> contents to text named or numeric entity...
sub _to_text_entity {
    my ($spec) = @_;
    # Is it a line break?
    if (my $BR_count = $is_break_entity{$spec}) {
        return "\n" x $BR_count;
    }
    # Is it a numeric codepoint in some base...
    if ($spec =~ m{\A \d}xms) {
        # Convert Perl 6 octals and decimals to Perl 5 notation...
        if ($spec !~ s{\A 0o}{0}xms) {       # Convert octal
            $spec =~ s{\A 0d}{}xms;          # Convert explicit decimal
            $spec =~ s{\A 0+ (?=\d)}{}xms;   # Convert implicit decimal
        }

        # Then return the Xtext numeric code...
        use charnames ':full';
        $spec = charnames::viacode(eval $spec);
    }
    if (my $replacement = $is_translatable{$spec}) {
        return $replacement;
    }
    else {
        return "[$spec]";
    }
}

sub to_text {
    my $self = shift;
    my $entities = $self->content;
    return join q{}, map {_to_text_entity($_)} split /\s*;\s*/, $entities;
}

# Important formatter...
package Perl6::Perldoc::FormattingCode::I;

sub to_text {
    my $self = shift;
    return '_' . $self->SUPER::to_text(@_) . '_';
}

# Keyboard input formatter...
package Perl6::Perldoc::FormattingCode::K;

sub to_text {
    my $self = shift;
    return '`' . $self->SUPER::to_text(@_) . '`';
}

# Link formatter...
package Perl6::Perldoc::FormattingCode::L;

my $PERLDOC_ORG = 'http://perldoc.perl.org/';
my $SEARCH      = 'http://www.google.com/search?q=';

sub to_text {
    my $self = shift;
    my $target   = $self->target();
    my $text = $self->has_distinct_text ? $self->SUPER::to_text(@_) : undef;

    # Link within this document...
    if ($target =~ s{\A (?:doc:\s*)? [#] }{}xms ) {
        return defined $text ? qq{$text (see the "$target" section)}
                             : qq{the "$target" section}
    }

    # Link to other documentation...
    if ($target =~ s{\A doc: }{}xms) {
        return defined $text ? qq{$text (see the documentation for $target)} 
                             : qq{the documentation for $target}
    }

    # Link to manpage...
    if ($target =~ s{\A man: }{}xms) {
        return defined $text ? qq{$text (see the $target manpage)}
                             : qq{the $target manpage}
    }

    # Link back to definition in this document...
    if ($target =~ s{\A (?:defn) : }{}xms) {
        return defined $text ? qq{$text (see the definition of "$target")}
                             : $target
    }

    # Anything else...
    return defined $text ? qq{$text <$target>}
                         : $target;
}

# Meta-formatter...
package Perl6::Perldoc::FormattingCode::M;


# Note formatter...
package Perl6::Perldoc::FormattingCode::N;

sub to_text {
    my $self = shift;
    my $count = ++$_[0]{note_count};
    my $marker = "[$count]";
    $_[0]{notes} .= qq{$marker } . $self->SUPER::to_text(@_) . "\n";
    return qq{$marker};
}

# Placement link formatter...
package Perl6::Perldoc::FormattingCode::P;

sub to_text {
    my $self = shift;
    my $target = $self->target();

    # Link within this document...
    if ($target =~ s{\A (?:doc:\s*)? [#] }{}xms ) {
        return qq{(See the "$target" section)};
    }

    # Link to other documentation...
    if ($target =~ s{\A doc: }{}xms) {
        return qq{(See the documentation for $target)};
    }

    # Link to manpage...
    if ($target =~ s{\A man: }{}xms) {
        return qq{(See the $target manpage)};
    }

    # TOC insertion...
    if ($target =~ s{\A toc: }{}xms) {
        return Perl6::Perldoc::Root::_list_to_text([$self->content],@_);
    }

    # Anything else...
    $target =~ s{\A (?:defn) : }{}xms;
    return qq{(See $target)};
}

# Replacable item formatter...
package Perl6::Perldoc::FormattingCode::R;

sub to_text {
    my $self = shift;
    return '[' .  $self->SUPER::to_text(@_) . ']';
}

# Space-preserving formatter...
package Perl6::Perldoc::FormattingCode::S;

sub to_text {
    my $self = shift;
    return $self->SUPER::to_text(@_);
}


# Terminal output formatter...
package Perl6::Perldoc::FormattingCode::T;

sub to_text {
    my $self = shift;
    return '`' .  $self->SUPER::to_text(@_) . '`';
}

# Unusual formatter...
package Perl6::Perldoc::FormattingCode::U;

sub to_text {
    my $self = shift;
    return '_' .  $self->SUPER::to_text(@_) . '_';
}

# Verbatim formatter...
package Perl6::Perldoc::FormattingCode::V;

# indeX formatter...
package Perl6::Perldoc::FormattingCode::X;

# Zero-width formatter...
package Perl6::Perldoc::FormattingCode::Z;

sub to_text {
    return q{};
}


# Standard =table block...
package Perl6::Perldoc::Block::table;   


1; # Magic true value required at end of module
__END__

=head1 NAME

Perl6::Perldoc::To::Text - Add a to_text() method to Perl6::Perldoc::Parser


=head1 VERSION

This document describes Perl6::Perldoc::To::Text version 0.0.1


=head1 SYNOPSIS

    use Perl6::Perldoc::Parser;
    use Perl6::Perldoc::To::Text;

    # All Perl6::Perldoc::Parser DOM classes now have a to_text() method

  
=head1 DESCRIPTION

This module adds a method named C<to_text()> to each of the classes in
the C<Perl6::Perldoc::Root> hierarchy, enabling them all to produce a
plaintext representation of themselves and their nested components.

The module also adds a C<to_text()> method to the
C<Perl6::Perldoc::ReturnVal> object returned by
C<Perl6::Perldoc::Parser::parse()>, so that perldoc-to-text conversion
can be performed in a single statement:

    use Perl6::Perldoc::Parser;
    use Perl6::Perldoc::To::Text;

    print Perl6::Perldoc::Parser->parse($file)
                                ->report_errors()
                                ->to_text();


=head1 INTERFACE 

Loading the module automatically installs the necessary C<to_text()>
methods in every C<Perl6::Perldoc> subclass.

Each C<to_text()> method takes no arguments and returns a string
containing a plaintext representation of the object to which the method
was applied.


=head1 DIAGNOSTICS

Adds no new diagnostics to those of Perl6::Perldoc::Parser.


=head1 CONFIGURATION AND ENVIRONMENT

Perl6::Perldoc::To::Text requires no configuration files or environment
variables.


=head1 DEPENDENCIES

Perl6::Perldoc::Parser


=head1 INCOMPATIBILITIES

None reported.


=head1 BUGS AND LIMITATIONS

The plaintext formatting that is produced is relatively primitive. It
could certainly be more readable in places. Patches in that direction
will be most especially welcome.

The translator does not expand C<< PZ<><> >> formatting codes (it
represents them as external links, rather than pulling the contents
of the link into the document). This approach is permitted under the
Perldoc definition, but not the desired behaviour.


No bugs have been reported.

Please report any bugs or feature requests to
C<bug-perldoctotext@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org>.


=head1 AUTHOR

Damian Conway  C<< <DCONWAY@cpan.org> >>


=head1 LICENCE AND COPYRIGHT

Copyright (c) 2006, Damian Conway C<< <DCONWAY@cpan.org> >>. All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.


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
