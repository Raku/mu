package Perldoc::HTML;
use Perldoc::Writer -Base;
use Perldoc::Receiver -mixin;

field 'is_fragment' => 0;

sub begin {
    my $chunk = shift;
    my $type = $chunk->{type} or die;
    my $method = "begin_$type";
    $self->$method($chunk);
}

sub end {
    my $chunk = shift;
    my $type = $chunk->{type} or die;
    my $method = "end_$type";
    $self->$method($chunk);
}

sub begin_stream {
    $self->write(<<_) unless $self->is_fragment;
<html>
<head>
</head>
<body>
_
}

sub end_stream {
    $self->write(<<_) unless $self->is_fragment;
</body>
</html>
_
}

sub begin_heading {
    my $chunk = shift;
    my $level = $chunk->{level};
    $self->write("<h$level>");
}

sub end_heading {
    my $chunk = shift;
    my $level = $chunk->{level};
    $self->write("</h$level>\n");
}

sub begin_verbatim {
    $self->write("<pre>\n");
}

sub end_verbatim {
    $self->write("</pre>\n");
}

sub begin_paragraph {
    $self->write("<p>\n");
}

sub end_paragraph {
    $self->write("</p>\n");
}

sub content {
    $self->write($self->html_escape(shift));
}

sub html_escape {
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
