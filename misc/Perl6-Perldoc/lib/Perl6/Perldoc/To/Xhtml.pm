package Perl6::Perldoc::To::Xhtml;

use warnings;
use strict;

# Everything else is silence...
package UNIVERSAL;

sub to_xhtml {
    return q{};
}

package Perl6::Perldoc::Parser::ReturnVal;

my %DTD_TYPE = (
    strict       => '-//W3C//DTD XHTML 1.0 Strict//EN',
    transitional => '-//W3C//DTD XHTML 1.0 Transitional//EN',
);

sub _full_doc {
    my ($tree, $opt_ref, $body) = @_;

    # Find document title (either explicitly specified or filename)...
    my $title = $opt_ref->{title};
    if (!defined $title) {
        $title = $tree->range->{file} || q{};
        $title =~ s{.*/}{}xms;
    }

    # Determine DTD type (either explicitly specified or default)...
    my $requested = $opt_ref->{full_doc};
    my $type = !defined $requested          ? 'transitional'
             : exists $DTD_TYPE{$requested} ? $DTD_TYPE{$requested}
             :                                $requested
             ;

    # Encapsulate body in appropriate frou-frou...
    return <<END_STRICT_DOC;
<!DOCTYPE html PUBLIC "$type">
<html>
<head><title>$title</title></head>
<body>
$body
</body>
</html>
END_STRICT_DOC
}

sub to_xhtml {
    my ($self, $opt_ref) = @_;

    $opt_ref ||= {};

    my $xhtml_rep = $self->{tree}->to_xhtml($opt_ref);

    if ($opt_ref->{notes}) {
        $xhtml_rep .= "<h1>Notes</h1>\n$opt_ref->{notes}";
    }

    if ($opt_ref->{full_doc}) {
        $xhtml_rep = _full_doc($self->{tree}, $opt_ref, $xhtml_rep);
    }

    return $xhtml_rep;
}

package Perl6::Perldoc::Root;  

sub add_xhtml_nesting {
    my ($self, $xhtml, $default) = @_;
    my $nested = $self->option('nested');
    if (!defined $nested) {
        $nested = $default || 0;
    }
    for (1..$nested) {
        $xhtml = "<blockquote>$xhtml</blockquote>";
    }
    return $xhtml;
}

sub _list_to_xhtml {
    my $list_ref = shift;
    my $xhtml = q{};
    for my $content ( @{$list_ref} ) {
        next if ! defined $content;
        if (ref $content) {
            $xhtml .= $content->to_xhtml(@_);
        }
        else {
            $content =~ s{&}{&amp;}gxms;
            $content =~ s{<}{&lt;}gxms;
            $content =~ s{>}{&gt;}gxms;
            $xhtml .= $content;
        }
    }
    return $xhtml;
}

sub to_xhtml {
    my $self = shift;
    my $content = _list_to_xhtml([$self->content],@_);
    my $name = Perl6::Perldoc::Root::_list_to_xhtml([
                 Perl6::Perldoc::FormattingCode::L::_flatten([$self->content])
               ]);
    
    # Assign jump targets to headings, semantic blocks, and user-defined...
    my $is_target 
        =  $self->isa('Perl6::Perldoc::Block')
        && $self->typename =~ m{\A head\d+ \z | [[:upper:]] }xms;

    if ($is_target) {
        $content = qq{<a name="$name"><a name="}
                 . ($self+0)
                 . qq{">$content</a></a>};
    }

    return $self->add_xhtml_nesting($content);
}

# Representation of file itself...
package Perl6::Perldoc::Document;  
    use base 'Perl6::Perldoc::Root';

# Ambient text around the Pod...
package Perl6::Perldoc::Ambient;  

sub to_xhtml {
    my $self = shift;
    return q{};
}

# Pod blocks...
package Perl6::Perldoc::Block;    

# Standard =pod block...
package Perl6::Perldoc::Block::pod;    

# Standard =para block (may be implicit)...
package Perl6::Perldoc::Block::para;   

sub to_xhtml {
    my $self = shift;
    return '<p>' . $self->SUPER::to_xhtml(@_) . "</p>\n";
}

# Standard =code block (may be implicit)...
package Perl6::Perldoc::Block::code;   

sub _min {
    my $min = shift;
    for my $next (@_) {
        $min = $next if $next < $min;
    }
    return $min;
}

sub to_xhtml {
    my $self = shift;
    my $xhtml = Perl6::Perldoc::Root::_list_to_xhtml([$self->content], @_);
    my $left_space = _min(map { length } $xhtml =~ m{^ [^\S\n]* (?= \S) }gxms);
    $xhtml =~ s{^ [^\S\n]{$left_space} }{}gxms;
    $xhtml = '<pre>' . $xhtml . '</pre>';
    return $self->add_xhtml_nesting($xhtml, 1);
}


# Standard =input block
package Perl6::Perldoc::Block::input;   

sub to_xhtml {
    my $self = shift;
    return '<blockquote><pre><kbd>'
         . Perl6::Perldoc::Root::_list_to_xhtml([$self->content], @_)
         . "</kbd></pre></blockquote>\n";
}


# Standard =output block
package Perl6::Perldoc::Block::output;   

sub to_xhtml {
    my $self = shift;
    return '<blockquote><pre><samp>'
         . Perl6::Perldoc::Root::_list_to_xhtml([$self->content], @_)
         . "</samp></pre></blockquote>\n";
}

# Standard =config block...
package Perl6::Perldoc::Config; 

sub to_xhtml {
    my $self = shift;
    return q{};
}

# Standard =table block...
package Perl6::Perldoc::Block::table; 

sub to_xhtml {
    my $self = shift;

    my $caption = $self->option('caption') || q{};
    if ($caption) {
        if (ref($caption) eq 'ARRAY') {
            $caption = "@{$caption}";
        }
    }
    $caption = Perl6::Perldoc::Root::_list_to_xhtml([$caption]);

    my $xhtml = qq{<a name="$caption"><a name="} . ($self+0) . qq{"><table>\n};

    if ($caption) {
        $xhtml .= qq{<caption>$caption</caption>\n};
    }

    for my $row ( $self->rows() ) {
        $xhtml .= "<tr>\n";
        for my $cell ( $row->cells() ) {
            $xhtml .= $cell->is_header ? "<th>\n" : "<td>\n";
            for my $block ( $cell->content ) {
                $xhtml .= $block->to_xhtml(@_);
            }
            $xhtml .= $cell->is_header ? "</th>\n" : "</td>\n";
        }
        $xhtml .= "</tr>\n";
    }
    $xhtml .= "</table></a></a>\n";

    return $self->add_xhtml_nesting($xhtml);
}

package Perl6::Perldoc::Block::table::Row;

package Perl6::Perldoc::Block::table::Cell;

# Standard =head1 block...
package Perl6::Perldoc::Block::head1;  

sub to_xhtml {
    my $self = shift;
    my $title = $self->SUPER::to_xhtml(@_);
    $title =~ s{\A\s+|\s+\Z}{}gxms;
    $title =~ s{\s+}{ }gxms;
    my $number = $self->number;
    if (defined $number) {
        $title = "$number. $title";
    }
    return qq{<h1>$title</h1>\n};
}

# Standard =head2 block...
package Perl6::Perldoc::Block::head2;  

sub to_xhtml {
    my $self = shift;
    my $title = $self->SUPER::to_xhtml(@_);
    $title =~ s{\A\s+|\s+\Z}{}gxms;
    $title =~ s{\s+}{ }gxms;
    my $number = $self->number;
    if (defined $number) {
        $title = "$number. $title";
    }
    return qq{<h2>$title</h2>\n};
}

# Standard =head3 block...
package Perl6::Perldoc::Block::head3;  

sub to_xhtml {
    my $self = shift;
    my $title = $self->SUPER::to_xhtml(@_);
    $title =~ s{\A\s+|\s+\Z}{}gxms;
    $title =~ s{\s+}{ }gxms;
    my $number = $self->number;
    if (defined $number) {
        $title = "$number. $title";
    }
    return qq{<h3>$title</h3>\n};
}

# Standard =head4 block...
package Perl6::Perldoc::Block::head4;  

sub to_xhtml {
    my $self = shift;
    my $title = $self->SUPER::to_xhtml(@_);
    $title =~ s{\A\s+|\s+\Z}{}gxms;
    $title =~ s{\s+}{ }gxms;
    my $number = $self->number;
    if (defined $number) {
        $title = "$number. $title";
    }
    return qq{<h4>$title</h4>\n};
}

# Implicit list block...
package Perl6::Perldoc::Block::list;   
    use base 'Perl6::Perldoc::Root';

sub to_xhtml {
    my $self = shift;
    my $xhtml = $self->SUPER::to_xhtml(@_);
    my ($first_item) = $self->content;
    if ($first_item->option('term')) {
        return '<dl>' . $xhtml . '</dl>';
    }
    elsif ($first_item->number) {
        return '<ol>' . $xhtml . '</ol>';
    }
    else {
        return '<ul>' . $xhtml . '</ul>';
    }
}


# Standard =item block...
package Perl6::Perldoc::Block::item;   

sub to_xhtml {
    my $self = shift;

    my $counter = $self->number || q{};

    if (my $term = $self->term()) {
        open my $term_fh, '<', \$term
            or die "Internal error: Can't parse :term contents";
        $term = $self->term( {as_objects=>1} )->to_xhtml(@_);
        if (length $counter) {
            $term =~ s{\A (\s* <[^>]+>)}{$1$counter. }xms;
        }
        return "<dt>$term</dt><dd>"
             . $self->SUPER::to_xhtml(@_)
             . "</dd>\n";
    }
    else {
        $counter =~ s{.*[.]?(\d+)$}{ value="$1"}xms;
        return "<li$counter>"
             . $self->SUPER::to_xhtml(@_)
             . "</li>\n";
    }
}

# Implicit toclist block...
package Perl6::Perldoc::Block::toclist;   
    use base 'Perl6::Perldoc::Root';

sub to_xhtml {
    my $self = shift;
    
    # Convert list items to XHTML, and return in an XHTML list...
    my $xhtml = join q{}, map {$_->to_xhtml(@_)}  $self->content;

    return '<ul class="toclist">' . $xhtml . '</ul>';
}


# Standard =tocitem block...
package Perl6::Perldoc::Block::tocitem;   

sub to_xhtml {
    my $self = shift;

    my $target = $self->target();
    my $typename = $self->typename();

    my @title = $self->title;
    return "" if ! @title;
    
    my $title = Perl6::Perldoc::Root::_list_to_xhtml(\@title, @_);

    return qq{<a href="$target">\n<li class="$typename">$title</li></a>\n};
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

# Standard =nested block...
package Perl6::Perldoc::Block::nested;   

sub to_xhtml {
    my $self = shift;
    return '<blockquote>' . $self->SUPER::to_xhtml(@_) . "</blockquote>\n";
}

# Standard =comment block...
package Perl6::Perldoc::Block::comment;   

sub to_xhtml {
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
        FOREWORD          FOREWORDS
        SUMMARY           SUMMARIES
        SECTION           SECTIONS
        CHAPTER           CHAPTERS
        APPENDIX          APPENDIXES       APPENDICES
        TOC               TOCS
        INDEX             INDEXES          INDICES
    );

    # Reuse content-to-xhtml converter
    *_list_to_xhtml = *Perl6::Perldoc::Root::_list_to_xhtml;

    # All semantic blocks are self-titling, followed by contents...
    for my $blockname (@semantic_blocks) {
        no strict qw< refs >;

        *{ "Perl6::Perldoc::Block::${blockname}::to_xhtml" }
            = sub {
                my $self = shift;

                my @title = $self->title();
                return "" if !@title;

                my $title = _list_to_xhtml(\@title, @_);

                return qq{<a name="$title"><a name="}
                     . ($self+0)
                     . qq{"><h1 class="$blockname">$title</h1></a></a>\n}
                     . _list_to_xhtml([$self->content], @_);
            };
    }
}


# Base class for formatting codes...

package Perl6::Perldoc::FormattingCode; 

package Perl6::Perldoc::FormattingCode::Named; 

# Basis formatter...
package Perl6::Perldoc::FormattingCode::B;

sub to_xhtml {
    my $self = shift;
    return '<strong>' . $self->SUPER::to_xhtml(@_) . "</strong>";
}

# Code formatter...
package Perl6::Perldoc::FormattingCode::C;

sub to_xhtml {
    my $self = shift;
    return '<code>' . $self->SUPER::to_xhtml(@_) . "</code>";
}

# Definition formatter...
package Perl6::Perldoc::FormattingCode::D;

sub to_xhtml {
    my $self = shift;
    my $tag = join q{}, $self->content;
    $tag = Perl6::Perldoc::Root::_list_to_xhtml([$tag]);
    return qq{<a name="$tag"><dfn>} . $self->SUPER::to_xhtml(@_) . '</dfn></a>';
}


# Entity formatter...
package Perl6::Perldoc::FormattingCode::E;

my %is_break_entity = (
    'LINE FEED (LF)'       => 1,     LF  => 1,
    'CARRIAGE RETURN (CR)' => 1,     CR  => 1,
    'NEXT LINE (NEL)'      => 1,     NEL => 1,

    'FORM FEED (FF)'       => 10,    FF  => 10, 
);

# Convert E<> contents to XHTML named or numeric entity...
sub _to_xhtml_entity {
    my ($spec) = @_;
    # Is it a line break?
    if (my $BR_count = $is_break_entity{$spec}) {
        return '<br/>' x $BR_count;
    }
    # Is it a named specification?
    if ($spec !~ m{\A \d}xms) {
        # Try Unicode first...
        use charnames ':full';
        my $ord = charnames::vianame($spec);
        return sprintf('&#%d;',$ord) if defined $ord;

        # Otherwise, it must be an XHMTL named entity...
        return qq{&$spec;};
    }
    # Otherwise, it's the numeric codepoint in some base...
    else {
        # Convert Perl 6 octals and decimals to Perl 5 notation...
        if ($spec !~ s{\A 0o}{0}xms) {       # Convert octal
            $spec =~ s{\A 0d}{}xms;          # Convert explicit decimal
            $spec =~ s{\A 0+ (?=\d)}{}xms;   # Convert implicit decimal
        }

        # Then return the XHTML numeric code...
        return sprintf '&#%d;', eval $spec;
    }
}

sub to_xhtml {
    my $self = shift;
    return join q{},
                map {_to_xhtml_entity($_)}
                    split /\s*;\s*/, scalar $self->content;
}

# Important formatter...
package Perl6::Perldoc::FormattingCode::I;

sub to_xhtml {
    my $self = shift;
    return '<em>' . $self->SUPER::to_xhtml(@_) . "</em>";
}

# Keyboard input formatter...
package Perl6::Perldoc::FormattingCode::K;

sub to_xhtml {
    my $self = shift;
    return '<kbd>' . $self->SUPER::to_xhtml(@_) . "</kbd>";
}

# Link formatter...
package Perl6::Perldoc::FormattingCode::L;

my $PERLDOC_ORG = 'http://perldoc.perl.org/';
my $SEARCH      = 'http://www.google.com/search?q=';

sub to_xhtml {
    my $self = shift;
    my $target = $self->target() || '????';
    my $local_target = substr($target,0,1) eq '#';

    my $url = $target =~ m{\A    doc:   ([^#] .*)}xms ? "$PERLDOC_ORG$1.html"
            : $target =~ m{\A (?:doc:)? ( [#] .*)}xms ? $1
            : $target =~ m{\A   defn:   (     .*)}xms ? "#$1"
            : $target =~ m{\A    man:   (     .*)}xms ? "help:$1"
            : $target =~ m{\A (is.n):   (     .*)}xms ? "$SEARCH$1+$2"
            :                                           $target
            ;

    $url =~ s{\s+}{ }gxms;

    my $xhtml = $self->SUPER::to_xhtml(@_);
    if ($local_target) {
        $xhtml =~ s{\A [#]}{}xms;
    }
    else {
        $xhtml =~ s{\A (?: doc | defn | file | man ) : }{}xms;
    }
    return qq{<a href="$url">$xhtml</a>};
}

# Meta-formatter...
package Perl6::Perldoc::FormattingCode::M;


# Note formatter...
package Perl6::Perldoc::FormattingCode::N;

sub to_xhtml {
    my $self = shift;
    my $count = ++$_[0]{note_count};
    my $marker = "<sup>$count</sup>";
    $_[0]{notes}
        .= qq{<p><a name="_nb_out_$count" href="#_nb_in_$count">$marker</a>}
         . $self->SUPER::to_xhtml(@_)
         . "</p>\n";
    return qq{<a name="_nb_in_$count" href="#_nb_out_$count">$marker</a>};
}

# Placement link formatter...
package Perl6::Perldoc::FormattingCode::P;

sub to_xhtml {
    my $self = shift;

    my $link = $self->SUPER::to_xhtml(@_);

    # Table-of-contents placement...
    if ($self->target =~ m{\A \s* toc :}xms) {
        return $link;
    }

    # Everything else is just an outwards link (at the moment; should be fixed)
    else {
        return qq{See: <a href="$link">$link</a>};
    }
}

# Replacable item formatter...
package Perl6::Perldoc::FormattingCode::R;

sub to_xhtml {
    my $self = shift;
    return '<var>' .  $self->SUPER::to_xhtml(@_) . "</var>";
}

# Space-preserving formatter...
package Perl6::Perldoc::FormattingCode::S;

sub to_xhtml {
    my $self = shift;
    my $text = $self->SUPER::to_xhtml(@_);
    $text =~ s{ }{&nbsp;}gxms;
    $text =~ s{\n}{<br/>}gxms;
    return $text;
}


# Terminal output formatter...
package Perl6::Perldoc::FormattingCode::T;

sub to_xhtml {
    my $self = shift;
    return '<samp>' .  $self->SUPER::to_xhtml(@_) . "</samp>";
}

# Unusual formatter...
package Perl6::Perldoc::FormattingCode::U;

sub to_xhtml {
    my $self = shift;
    return '<em>' .  $self->SUPER::to_xhtml(@_) . "</em>";
}

# Verbatim formatter...
package Perl6::Perldoc::FormattingCode::V;

# indeX formatter...
package Perl6::Perldoc::FormattingCode::X;

# Zero-width formatter...
package Perl6::Perldoc::FormattingCode::Z;

sub to_xhtml {
    return q{};
}


# Standard =table block...
package Perl6::Perldoc::Block::table;   


1; # Magic true value required at end of module
__END__

=head1 NAME

Perl6::Perldoc::To::Xhtml - Add a to_xhtml() method to Perl6::Perldoc::Parser


=head1 VERSION

This document describes Perl6::Perldoc::To::Xhtml version 0.0.1


=head1 SYNOPSIS

    use Perl6::Perldoc::Parser;
    use Perl6::Perldoc::To::Xhtml;

    # All Perl6::Perldoc::Parser DOM classes now have a to_xhtml() method

  
=head1 DESCRIPTION

This module adds a method named C<to_xhtml()> to each of the classes in
the C<Perl6::Perldoc::Root> hierarchy, enabling them all to produce an
XHTML representation of themselves and their nested components.

The module also adds a C<to_xhtml()> method to the
C<Perl6::Perldoc::ReturnVal> object returned by
C<Perl6::Perldoc::Parser::parse()>, so that perldoc-to-xhtml translation
can be performed in a single statement:

    use Perl6::Perldoc::Parser;
    use Perl6::Perldoc::To::Xhtml;

    print Perl6::Perldoc::Parser->parse($file)
                                ->report_errors()
                                ->to_xhtml();


=head1 INTERFACE 

Loading the module automatically installs the necessary C<to_xhtml()>
methods in every C<Perl6::Perldoc> subclass.

Each C<to_xhtml()> method takes a reference to a hash containing options,
and returns a string containing an XHTML representation of the object to
which the method was applied.

The options currently supported are:

=over

=item C<< full_doc => 1 >>

If this option is true, the C<to_xhtml()> method generates a complete
XHTML document (including a DTD, C<< <head> >>, C<< <title> >>, and C<<
<body> >> tags), rather than just the body contents.

By default the DTD is C<"XHTML 1.0 Transitional//EN">, but you can make
it C<"XHTML 1.0 Strict//EN"> instead with:

    $perldoc->to_xhtml({ full_doc => 'strict' });

Alternatively, you can specify the DTD explicitly:

    $perldoc->to_xhtml({ full_doc => '-//W3C//DTD XHTML 1.0 Frameset//EN' });

Note that generating a full XHTML document is not the default behaviour
because defaulting to generating unencapsuated body contents makes it
easy to generate XHTML markup from separate Perldoc documents and
then concatentate them into a single body:

    my @body_parts = map { $_->to_xhtml() } @perldoc_reps;

    print <<"END_XHTML"
    <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN">
    <html>
        <head>
            <title>My compound document</title>
        </head>
        <body>
            @body_parts
        </body>
    </html>
    END_XHTML


=item C<< title => $plaintext_title >>

If this option is present (and the C<'full_doc'> option is also
specifed), the value of this option is used as the C<< <title> >> value
in the resulting XHTML document.

If this option is not specified, the name of the file from which the Perldoc
object was generated is used instead.

=back


=head1 DIAGNOSTICS

Adds no new diagnostics to those of Perl6::Perldoc::Parser.


=head1 CONFIGURATION AND ENVIRONMENT

Perl6::Perldoc::To::Xhtml requires no configuration files or environment
variables.


=head1 DEPENDENCIES

Perl6::Perldoc::Parser


=head1 INCOMPATIBILITIES

None reported.


=head1 BUGS AND LIMITATIONS

The translator does not expand C<< PZ<><> >> formatting codes (it
represents them as ordinary links, rather than pulling the contents of
the link into the document). This approach is permitted under the Perldoc
definition, but not the desired behaviour.


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
