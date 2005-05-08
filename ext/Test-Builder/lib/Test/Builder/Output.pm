class Test::Builder::Output-0.0.1;

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

=pod

=head1 NAME

Test::Builder::Output

=head1 SYNOPSIS

  use Test::Builder::Output;

=head1 DESCRIPTION

This class handles all the output methods needed for Test::Builder.

=head1 PUBLIC ATTRIBUTES

=over 4

=item B<$.output>

=item B<$.error_output>

=back

=head1 METHODS

=over 4

=item B<write ( Str $line )>

=item B<diag ( Str $line )>

=back

=head1 SEE ALSO

Perl5 Test::Builder

=head1 AUTHORS

code by chromatic E<lt>chromatic@wgz.orgE<gt>

documentation by Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
