my $perldoc_data = <<'END_PERLDOC';
=begin code :allow<V>

V<=> in the first column is always a Perldoc directive

=end code

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      config: {}

      content: 
        - !!perl/hash:Perl6::Perldoc::Block::code 
          config: {}

          content: 
            - "\n"
            - !!perl/hash:Perl6::Perldoc::FormattingCode::V 
              config: {}

              content: 
                - =
              is_verbatim: 1
              left_delim: <
              right_delim: ">"
              style: formatting
              typename: V
            - " in the first column is always a Perldoc directive\n\n"
          is_verbatim: 1
          options: 
            allow: 
              - V
          style: delimited
          typename: code
      is_implicit: 1
      style: implicit
      typename: pod
  terminator: (?!)
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
