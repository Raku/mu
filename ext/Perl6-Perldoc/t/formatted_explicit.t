my $perldoc_data = <<'END_PERLDOC';
=begin para
B<I<
Warning: Do not immerse in water. Do not expose to bright light.
Do not feed after midnight.
>>
=end para

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      content: 
        - !!perl/hash:Perl6::Perldoc::Block::para 
          content: 
            - !!perl/hash:Perl6::Perldoc::FormattingCode::B 
              content: 
                - !!perl/hash:Perl6::Perldoc::FormattingCode::I 
                  content: 
                    - "\n\
                      Warning: Do not immerse in water. Do not expose to bright light.\n\
                      Do not feed after midnight.\n"
                  left_delim: <
                  right_delim: ">"
                  style: formatting
                  typename: I
              left_delim: <
              right_delim: ">"
              style: formatting
              typename: B
            - "\n"
          style: delimited
          typename: para
      style: implicit
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
