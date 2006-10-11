
package Perl6::MetaModel::Parser::Tokenizer;

use strict;
use warnings;

our $VERSION = '0.01';

use String::Tokenizer;

sub new {
    my $class = shift;
    my $self = bless {
        tokenizer => String::Tokenizer->new()
    } => $class;
    $self->{tokenizer}->setDelimiter("{}();,\n\t");
    $self->{tokenizer}->handleWhitespace(String::Tokenizer->RETAIN_WHITESPACE);
    return $self;
}

sub tokenize { 
    my ($self, $source) = @_;
    $self->{tokenizer}->tokenize($source);
    grep { $_ } $self->{tokenizer}->getTokens();
}

sub iterator { (shift)->{tokenizer}->iterator }

1;