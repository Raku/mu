package Perldoc::Parser::Kwid;
use Perldoc::Parser -Base;

const top_class => 'Perldoc::Parser::Kwid::Top';
const class_prefix => 'Perldoc::Parser::Kwid::';

sub classes {
    qw(
        AsisPhrase
        BoldPhrase
        CodePhrase
        CommentBlock
        DefinitionItem
        DefinitionList
        DocumentLink
        HeadingBlock
        HyperLink
        ItalicPhrase
        ListItem
        NamedBlock
        NamedPhrase
        OrderedList
        TextParagraph
        UnorderedList
        UrlLink
        VerbatimBlock
    );
}

################################################################################
package Perldoc::Parser::Kwid::Top;
use base 'Perldoc::Parser::Kwid';
const id => 'top';
const contains => [qw( comment named pre dlist ulist olist para )];

sub parse {
    $self->receiver->begin('stream');
    my $buffer = $self->reader->buffer;
    my $table = $self->table;
    my $contains = $self->contains;
    while (not $self->the_end) {
        my $matched = 0;
        for my $id (@$contains) {
            warn $id,"\n";
            my $class = $table->{$id} or next;
            next unless $class->can('start_patterns');
            if ($self->match_start($buffer, $class)) {
                $self->create_parser($class)->parse;
                $matched++;
                last;
            }
        }
        die "No Rule to match:\n" . $$buffer;
    }
    $self->receiver->end('stream');
}

sub match_start {
    my $buffer = shift;
    my $class = shift;
    warn $class, "\n";
    my $patterns = $class->start_patterns;
    for my $pattern (@$patterns) {
        return 1 if $$buffer =~ $pattern;
    }
    return 0;
}

sub the_end {
    $self->reader->eos;
}

################################################################################
package Perldoc::Parser::Kwid::TextParagraph;
use base 'Perldoc::Parser::Kwid';
const id => 'para';
const start_patterns => [qr{^.}];
const contains => [qw(bold italic text)];

sub parse {
    XXX $self;
}



################################################################################
package Perldoc::Parser::Kwid::NamedBlock;
use base 'Perldoc::Parser::Kwid';
const id => 'named';
const start_patterns => [qr{^\.\w+}];

sub parse {
    # Load the sub parsing module
    # Invoke a subparse
}

################################################################################
package Perldoc::Parser::Kwid::VerbatimBlock;
use base 'Perldoc::Parser::Kwid';
const id => 'pre';

################################################################################
package Perldoc::Parser::Kwid::DefinitionList;
use base 'Perldoc::Parser::Kwid';
const id => 'dlist';

################################################################################
package Perldoc::Parser::Kwid::UnorderedList;
use base 'Perldoc::Parser::Kwid';
const id => 'ulist';

################################################################################
package Perldoc::Parser::Kwid::OrderedList;
use base 'Perldoc::Parser::Kwid';
const id => 'olist';

################################################################################
package Perldoc::Parser::Kwid::BoldPhrase;
use base 'Perldoc::Parser::Kwid';
const id => 'bold';

################################################################################
package Perldoc::Parser::Kwid::ItalicPhrase;
use base 'Perldoc::Parser::Kwid';

################################################################################
package Perldoc::Parser::Kwid::CodePhrase;
use base 'Perldoc::Parser::Kwid';

__END__

################################################################################
sub parse {
    my $result = $self->do_parse;
}

sub do_parse {
    $self->receiver->begin({type => 'stream'});
    while (my $block = $self->next_block) {
        $self->receiver->begin($block);
        $self->reparse($block);
        $self->receiver->end($block);
    }
    $self->receiver->end({type => 'stream'});
    return $self->finish;
}

sub reparse {
    my $chunk = shift;
    my $type = $chunk->{type};
    my $class = "Perldoc::Parser::$type";
    my $parser = $class->new(
        input => \$chunk->{content},
        receiver => $self->receiver,
    );
    $parser->parse;
}

sub contains_blocks {
    qw( heading verbatim paragraph )
}

sub next_block {
    $self->throwaway
      or return;
    for my $type ($self->contains_blocks) {
        my $method = "get_$type";
        my $block = $self->$method;
        next unless defined $block;
        $block = { content => $block }
          unless ref $block;
        $block->{type} ||= $type;
        return $block;
    }
    return;
}

sub throwaway {
    while (my $line = $self->read) {
        next if
          $self->comment_line($line) or
          $self->blank_line($line);
        $self->unread($line);
        return 1;
    }
    return;
}

sub read_paragraph {
    my $paragraph = '';
    while (my $line = $self->read) {
        last if $self->blank_line($line);
        $paragraph .= $line;
    }
    return $paragraph;
}

sub comment_line { (pop) =~ /^#\s/ }
sub blank_line { (pop) =~ /^\s*$/ }
sub line_matches {
    my $regexp = shift;
    my $line = $self->read;
    $self->unread($line);
    $line =~ $regexp;
}

# Methods to parse out top level blocks
sub get_heading {
    return unless $self->line_matches(qr/^={1,4} \S/);
    my $heading = $self->read_paragraph;
    $heading =~ s/\s*\n\s*(?=.)/ /g;
    chomp $heading;
    $heading =~ s/^(=+)\s+// or die;
    my $level = length($1);
    return +{
        content => $heading,
        level => $level,
    };
}

sub get_verbatim { 
    my $verbatim = '';
    my $prev_blank = 0;
    while (my $line = $self->read) {
        if ($line =~ /^\S/) {
            if ($prev_blank) {
                $self->unread($line);
                last;
            }
            $self->unread($verbatim, $line);
            return;
        }
        next if $self->comment_line($line);
        $verbatim .= $line;
        $prev_blank = $self->blank_line($line);
    }
    return unless $verbatim;
    until ($verbatim =~ /^\S/) {
        $verbatim =~ s/^ //gm;
    }
    return $verbatim;
}

sub get_paragraph {
    my $paragraph = $self->read 
      or return;
    while (my $line = $self->read) {
        next if $self->comment_line($line);
        last if $self->blank_line($line);
        $paragraph .= $line;
    }
    $paragraph =~ s/\s*\n(?=.)/ /g;
    return $paragraph;
}

# Methods to handle reading and buffering input
package Perldoc::Parser::Unit;
our @ISA = qw(Perldoc::Parser);

sub do_parse {
    $self->receiver->content(${$self->input});
}

sub reparse {
    die;
}

package Perldoc::Parser::heading;
use base 'Perldoc::Parser::Unit';

package Perldoc::Parser::verbatim;
use base 'Perldoc::Parser::Unit';

package Perldoc::Parser::paragraph;
use base 'Perldoc::Parser::Unit';
