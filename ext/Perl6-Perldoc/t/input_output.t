my $perldoc_data = <<'END_PERLDOC';
=input
find baracus
=begin output
    Name:    Baracus, B.A.
    Rank:    Sgt
    Serial:  1PTDF007

    Do you want additional personnel details? K<y>

    Height:  180cm/5'11"
    Weight:  104kg/230lb
    Age:     49

    Print? K<n>
=end output

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      content: 
        - !!perl/hash:Perl6::Perldoc::Block::input 
          content: 
            - "find baracus\n"
          style: abbreviated
          typename: input
        - !!perl/hash:Perl6::Perldoc::Block::output 
          content: 
            - "    Name:    Baracus, B.A.\n    Rank:    Sgt\n    Serial:  1PTDF007\n\n    Do you want additional personnel details? "
            - !!perl/hash:Perl6::Perldoc::FormattingCode::K 
              content: 
                - y
              style: formatting
              typename: K
            - "\n\n    Height:  180cm/5'11\"\n    Weight:  104kg/230lb\n    Age:     49\n\n    Print? "
            - !!perl/hash:Perl6::Perldoc::FormattingCode::K 
              content: 
                - n
              style: formatting
              typename: K
            - "\n"
          style: delimited
          typename: output
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
