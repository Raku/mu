my $perldoc_data = <<'END_PERLDOC';
=for item  :term<MAD>
Affected with a high degree of intellectual independence.

=for item  :term<MEEKNESS>
Uncommon patience in planning a revenge that is worth while.

=for item  :term<MORAL>
Conforming to a local and mutable standard of right.
Having the quality of general expediency.

An item that's specified as a term can still be numbered:

=for item :numbered :term<SELFISH>
Devoid of consideration for the selfishness of others. 

=for item :numbered :term<SUCCESS> 
The one unpardonable sin against one's fellows.

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      content: 
        - !!perl/hash:Perl6::Perldoc::Block::list 
          content: 
            - !!perl/hash:Perl6::Perldoc::Block::item 
              content: 
                - "Affected with a high degree of intellectual independence.\n"
              options: 
                term: 
                  - MAD
            - !!perl/hash:Perl6::Perldoc::Block::item 
              content: 
                - "Uncommon patience in planning a revenge that is worth while.\n"
              options: 
                term: 
                  - MEEKNESS
            - !!perl/hash:Perl6::Perldoc::Block::item 
              content: 
                - "Conforming to a local and mutable standard of right.\n\
                  Having the quality of general expediency.\n"
              options: 
                term: 
                  - MORAL
          level: 1
          typename: list
        - !!perl/hash:Perl6::Perldoc::Block::para 
          content: 
            - "An item that's specified as a term can still be numbered:\n"
        - !!perl/hash:Perl6::Perldoc::Block::list 
          content: 
            - !!perl/hash:Perl6::Perldoc::Block::item 
              content: 
                - "Devoid of consideration for the selfishness of others. \n"
              number: 1
                numbered: 1
                term: 
                  - SELFISH
            - !!perl/hash:Perl6::Perldoc::Block::item 
              content: 
                - "The one unpardonable sin against one's fellows.\n"
              number: 2
                numbered: 1
                term: 
                  - SUCCESS
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
