my $perldoc_data = <<'END_PERLDOC';
=begin code :allow< B R >
sub demo {
    B<say> 'Hello R<name>', $I<code>;
}
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
            - "sub demo {\n    "
            - !!perl/hash:Perl6::Perldoc::FormattingCode::B 
              config: {}

              content: 
                - say
              is_verbatim: 1
              left_delim: <
              right_delim: ">"
              style: formatting
              typename: B
            - " 'Hello "
            - !!perl/hash:Perl6::Perldoc::FormattingCode::R 
              config: {}

              content: 
                - name
              is_verbatim: 1
              left_delim: <
              right_delim: ">"
              style: formatting
              typename: R
            - "', $I<code>;\n\
              }\n"
          is_verbatim: 1
          options: 
            allow: 
              - B
              - R
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
