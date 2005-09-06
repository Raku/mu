#!/usr/bin/pugs

=pod

Expects to be passed a comma-separated list of colors and descriptions,
one color per line.

Outputs an SVG file.

Example:

  cat colors.txt | pugs make_swatch.p6 | display

=cut

use v6;

my $columns = 3; # how many color squares across
my $rows = 5;    # how many color squares down
my $width = 20;  # how many pixels per square
my $total_width  = ($columns+2)*$width*1.5;
my $total_height = ($rows+2)*$width*1.5;

my $svg_begin = q:t:s/END/
<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" 
"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">

<svg width="$total_width" height="$total_height" version="1.1"
xmlns="http://www.w3.org/2000/svg">

END;

my $svg_end = "\n</svg>\n";

say $svg_begin;

my $row = 1;
my $column = 0;

for =$*IN -> $line {
    $column++;
    if ($column > $columns) {
        $column = 1;
        $row++;
    }
    #say $line;
    my @data = split(/,/, $line.chomp);
    #say @data;
    say square($column*$width*1.5, $row*$width*1.5, @data[0], @data[1]);
}

say $svg_end;

# TODO had to put $hint in quotes, otherwise got a pugs error: Can't modify constant item: VUndef
sub square (Int $x, Int $y, Str $hex, Str $hint) {
    return q:t:s/END/
<rect x="$x" y="$y" width="$width" height="$width"
style="fill:#$hex;stroke-width:0; stroke:#$hex">
  <hint>"$hint"</hint>
</rect>
END
}

