package Kwid::HTML;
use strict;
use warnings;
use Kwid::Base;
use base 'Kwid::Loader';

sub begin {
    my $self = shift;
    my $chunk = shift;
    my $type = $chunk->{type} or die;
    my $method = "begin_$type";
    $self->$method($chunk);
}

sub end {
    my $self = shift;
    my $chunk = shift;
    my $type = $chunk->{type} or die;
    my $method = "end_$type";
    $self->$method($chunk);
}

sub begin_stream {
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

sub begin_heading {
    my $self = shift;
    my $chunk = shift;
    my $level = $chunk->{level};
    $self->print("<h$level>");
}

sub end_heading {
    my $self = shift;
    my $chunk = shift;
    my $level = $chunk->{level};
    $self->print("</h$level>\n");
}

sub begin_verbatim {
    my $self = shift;
    $self->print("<pre>\n");
}

sub end_verbatim {
    my $self = shift;
    $self->print("</pre>\n");
}

sub begin_paragraph {
    my $self = shift;
    $self->print("<p>\n");
}

sub end_paragraph {
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
    $val =~ s/&/&amp;/g;
    $val =~ s/</&lt;/g;
    $val =~ s/>/&gt;/g;
    $val =~ s/\(/&#40;/g;
    $val =~ s/\)/&#41;/g;
    $val =~ s/"/&#34;/g;
    $val =~ s/'/&#39;/g;
    return $val;
}

1;
