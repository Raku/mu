package Kwid::HTML;
use strict;
use warnings;
use Kwid::Base;
use base 'Kwid::Loader';

sub start_stream {
    my $self = shift;
    $self->init;
    $self->print(<<_) if $self->complete;
<html>
<head>
</head>
<body>
_
}

sub end_stream {
    my $self = shift;
    $self->print(<<_) if $self->complete;
</body>
</html>
_
    $self->finish;
}

sub start_para {
    my $self = shift;
    $self->print("<p>\n");
}

sub end_para {
    my $self = shift;
    $self->print("</p>\n");
}

sub content {
    my $self = shift;
    $self->print($self->html_escape(shift));
}

sub html_escape {
    my $self = shift;
    my $val = shift;
    $val =~ s/&/&#38;/g;
    $val =~ s/</&lt;/g;
    $val =~ s/>/&gt;/g;
    $val =~ s/\(/&#40;/g;
    $val =~ s/\)/&#41;/g;
    $val =~ s/"/&#34;/g;
    $val =~ s/'/&#39;/g;
    return $val;
}

1;
