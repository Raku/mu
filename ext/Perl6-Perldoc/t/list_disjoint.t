my $perldoc_data = <<'END_PERLDOC';
The options are:

=item1 # Liberty
=item1 # Death
=item1 # Beer

The tools are:

=item1 # Revolution
=item1 # Deep-fried peanut butter sandwich
=item1 # Keg

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      content: 
        - !!perl/hash:Perl6::Perldoc::Block::para 
          content: 
            - "The options are:\n"
        - !!perl/hash:Perl6::Perldoc::Block::list 
          content: 
            - !!perl/hash:Perl6::Perldoc::Block::item1 
              content: 
                - "Liberty\n"
              number: 1
            - !!perl/hash:Perl6::Perldoc::Block::item1 
              content: 
                - "Death\n"
              number: 2
            - !!perl/hash:Perl6::Perldoc::Block::item1 
              content: 
                - "Beer\n"
              number: 3
        - !!perl/hash:Perl6::Perldoc::Block::para 
          content: 
            - "The tools are:\n"
        - !!perl/hash:Perl6::Perldoc::Block::list 
          content: 
            - !!perl/hash:Perl6::Perldoc::Block::item1 
              content: 
                - "Revolution\n"
              number: 1
            - !!perl/hash:Perl6::Perldoc::Block::item1 
              content: 
                - "Deep-fried peanut butter sandwich\n"
              number: 2
            - !!perl/hash:Perl6::Perldoc::Block::item1 
              content: 
                - "Keg\n"
              number: 3
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
