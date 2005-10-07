#!/usr/bin/perl

use strict;
use warnings;

use Perl6::MetaModel;
use Carp 'confess';

require "lib/TestBuilder.pm";

$::TestBuilder->STORE('::Output' => class 'TestBuilder::Output' => {
    is => [ $::Object ],
    attributes => [ '$.output', '$.error_output' ],
    submethods => {
        'BUILD' => sub {
            my ($self, %params) = @_;
            _('$.output'       => $params{'$.output'}       || \*STDOUT);
            _('$.error_output' => $params{'$.error_output'} || \*STDERR);           
        }
    },
    methods => {
        'write' => sub {
            my ($self, $message) = @_;
            $message =~ s/\n(?!#)/\n# /g;
            print {_('$.output')} $message, "\n";
        },
        'diag' => sub {
            my ($self, $message) = @_;            
            $message =~ s/^(?!#)/# /;
            $message =~ s/\n(?!#)/\n# /g;
            print {_('$.error_output')} $message, "\n";            
        }
    }
});

1;

__END__

=pod

=head1 NAME

Test::Builder::Output

=head1 SYNOPSIS

  use Test::Builder::Output;

=head1 DESCRIPTION

This class handles all the output for Test::Builder.

=head1 PUBLIC ATTRIBUTES

=over 4

=item B<$.output>

The filehandle to which to write all normal output (test results and
descriptions).

=item B<$.error_output>

The filehandle to which to write all diagnostic output.

=back

=head1 METHODS

=over 4

=item B<write( Str $message )>

Writes a message to the normal output filehandle, adding leading C<#>
characters after all newlines if they are not present.

=item B<diag( Str $message )>

Writes a message to the diagnostic output filehandle, adding a leading C<#> to
every line if they are not present.

=back

=head1 SEE ALSO

Perl 5 Test::Builder.

=head1 AUTHORS

Perl6::MetaModel 2.0 code by Stevan Little E<lt>stevan@iinteractive.comE<gt>

Perl 6 code by chromatic E<lt>chromatic@wgz.orgE<gt>

documentation by Stevan Little E<lt>stevan@iinteractive.comE<gt> and chromatic.

=head1 Perl 6 Code
  
  class Test::Builder::Output-0.2.1;
  
  has $.output;
  has $.error_output;
  
  submethod BUILD ( ?$.output = $*OUT, ?$.error_output = $*ERR ) {}
  
  method write ( Str $message is copy )
  {
      $message ~~ s:perl5:g{\n(?!#)}{\n# };
      $.output.say( $message );
  }
  
  method diag ( Str $message is copy )
  {
      $message ~~ s:perl5{^(?!#)}{# };
      $message ~~ s:perl5:g{\n(?!#)}{\n# };
      $.error_output.say( $message );
  }

=cut
