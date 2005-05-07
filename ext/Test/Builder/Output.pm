class Test::Builder::Output;

has $.output;
has $.error_output;

submethod BUILD ( ?$.output = $*OUT, ?$.error_output = $*ERR ) {}

method write ( Str $line )
{
    $line ~~ s:perl5:g{\n(?!#)}{\n#};
    $.output.say( $line );
}

method diag ( Str $line )
{
    $line ~~ s:perl5{^(?!#)}{#};
    $line ~~ s:perl5:g{\n(?!#)}{\n#};
    $.error_output.say( $line );
}
