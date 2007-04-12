my $perldoc_data = <<'END_PERLDOC';
D<definition
B<basis
I<important
K<keyboard input
N<note
R<replaceable
S<space preserving
T<terminal output
U<unusual
Z<zero width comment
>
>
>
>
>
>
>
>
>
|synonym>

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      content: 
        - !!perl/hash:Perl6::Perldoc::Block::para 
          content: 
            - !!perl/hash:Perl6::Perldoc::FormattingCode::D 
              content: 
                - "definition\n"
                - !!perl/hash:Perl6::Perldoc::FormattingCode::B 
                  content: 
                    - "basis\n"
                    - !!perl/hash:Perl6::Perldoc::FormattingCode::I 
                      content: 
                        - "important\n"
                        - !!perl/hash:Perl6::Perldoc::FormattingCode::K 
                          content: 
                            - "keyboard input\n"
                            - !!perl/hash:Perl6::Perldoc::FormattingCode::N 
                              content: 
                                - "note\n"
                                - !!perl/hash:Perl6::Perldoc::FormattingCode::R 
                                  content: 
                                    - "replaceable\n"
                                    - !!perl/hash:Perl6::Perldoc::FormattingCode::S 
                                      content: 
                                        - "space preserving\n"
                                        - !!perl/hash:Perl6::Perldoc::FormattingCode::T 
                                          content: 
                                            - "terminal output\n"
                                            - !!perl/hash:Perl6::Perldoc::FormattingCode::U 
                                              content: 
                                                - "unusual\n"
                                                - !!perl/hash:Perl6::Perldoc::FormattingCode::Z 
                                                  content: 
                                                    - "zero width comment\n"
                                                - "\n"
                                            - "\n"
                                        - "\n"
                                    - "\n"
                                - "\n"
                            - "\n"
                        - "\n"
                    - "\n"
                - "\n"
              synonyms: 
                - synonym
            - "\n"
          typename: para
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
