my $perldoc_data = <<'END_PERLDOC';
=item1  Animal
=item2     Vertebrate
=item2     Invertebrate

=item1  Phase
=item2     Solid
=item2     Liquid
=item2     Gas
=item2     Chocolate

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
                - "Animal\n"
            - !!perl/hash:Perl6::Perldoc::Block::list 
              content: 
                - !!perl/hash:Perl6::Perldoc::Block::item2 
                  content: 
                    - "Vertebrate\n"
                - !!perl/hash:Perl6::Perldoc::Block::item2 
                  content: 
                    - "Invertebrate\n"
              level: 2
            - !!perl/hash:Perl6::Perldoc::Block::item1 
              content: 
                - "Phase\n"
            - !!perl/hash:Perl6::Perldoc::Block::list 
              content: 
                - !!perl/hash:Perl6::Perldoc::Block::item2 
                  content: 
                    - "Solid\n"
                - !!perl/hash:Perl6::Perldoc::Block::item2 
                  content: 
                    - "Liquid\n"
                - !!perl/hash:Perl6::Perldoc::Block::item2 
                  content: 
                    - "Gas\n"
                - !!perl/hash:Perl6::Perldoc::Block::item2 
                  content: 
                    - "Chocolate\n"
              level: 2
              style: implicit
              typename: list
          level: 1
          style: implicit
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
