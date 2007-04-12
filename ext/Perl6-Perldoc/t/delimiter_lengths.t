my $perldoc_data = <<'END_PERLDOC';
C<  $foo<bar>   >
C<< $foo<<bar>> >>
C«$foo < $bar»
C<<$foo < $bar>>
The Perl 5 heredoc syntax was: C« <<END_MARKER »
The Perl 5 heredoc syntax was: C<<< <<END_MARKER >>>

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      content: 
        - !!perl/hash:Perl6::Perldoc::Block::para 
          content: 
            - !!perl/hash:Perl6::Perldoc::FormattingCode::C 
              content: 
                - "  $foo<bar>   "
              left_delim: <
              right_delim: ">"
            - "\n"
            - !!perl/hash:Perl6::Perldoc::FormattingCode::C 
              content: 
                - " $foo<<bar>> "
              left_delim: <<
              right_delim: ">>"
            - "\n"
            - !!perl/hash:Perl6::Perldoc::FormattingCode::C 
              content: 
                - $foo < $bar
              left_delim: "\xAB"
              right_delim: "\xBB"
            - "\n"
            - !!perl/hash:Perl6::Perldoc::FormattingCode::C 
              content: 
                - $foo < $bar
              left_delim: <<
              right_delim: ">>"
            - "\n\
              The Perl 5 heredoc syntax was: "
            - !!perl/hash:Perl6::Perldoc::FormattingCode::C 
              content: 
                - " <<END_MARKER "
              left_delim: "\xAB"
              right_delim: "\xBB"
            - "\n\
              The Perl 5 heredoc syntax was: "
            - !!perl/hash:Perl6::Perldoc::FormattingCode::C 
              content: 
                - " <<END_MARKER "
              left_delim: <<<
              right_delim: ">>>"
            - "\n"
      typename: pod
  typename: (document)
warnings: []


END_EXPECTED

use Perl6::Perldoc::Parser;
use Test::More 'no_plan';

sub is_subset {
    my ($found, $expected) = @_;
    my @found    = split /\n/, $found;
    my @expected = split /\n/, $expected;

    while (@found && @expected) {
        if ($found[0] eq $expected[0]) {
            is $found[0], $expected[0], $expected[0];
            shift @found;
            shift @expected;
        }
        else {
            shift @found;
        }
    }
    
    for my $expected (@expected) {
        ok 0, "Missing '$expected'";
    }
}

open my $fh, '<', \$perldoc_data
    or die "Could not open file on test data";

my $representation = Perl6::Perldoc::Parser->parse($fh ,{all_pod=>1});

use YAML::Syck 'Dump';
is_subset Dump($representation), $expected_structure;
