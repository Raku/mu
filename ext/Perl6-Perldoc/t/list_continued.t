my $perldoc_data = <<'END_PERLDOC';
=for item1
# Retreat to remote Himalayan monastery

=for item1
# Learn the hidden mysteries of space and time

I<????>

=for item1 :continued
# Prophet!

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      content: 
        - !!perl/hash:Perl6::Perldoc::Block::list 
          content: 
            - !!perl/hash:Perl6::Perldoc::Block::item1 
              content: 
                - "Retreat to remote Himalayan monastery\n"
              number: 1
            - !!perl/hash:Perl6::Perldoc::Block::item1 
              content: 
                - "Learn the hidden mysteries of space and time\n"
              number: 2
          level: 1
          typename: list
        - !!perl/hash:Perl6::Perldoc::Block::para 
          content: 
            - !!perl/hash:Perl6::Perldoc::FormattingCode::I 
              content: 
                - ????
            - "\n"
        - !!perl/hash:Perl6::Perldoc::Block::list 
          content: 
            - !!perl/hash:Perl6::Perldoc::Block::item1 
              content: 
                - "Prophet!\n"
              number: 3
              typename: item1
          level: 1
          typename: list
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
