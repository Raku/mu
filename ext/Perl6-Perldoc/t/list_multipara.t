my $perldoc_data = <<'END_PERLDOC';
=begin item :numbered
I<The rain in Spain falls mainly on the plain.>

This is a common myth and an unconscionable slur on the Spanish
people, the majority of whom are extremely attractive.
=end item

=begin item :numbered
I<The early bird gets the worm.>

In deciding whether to become an early riser, it is worth
considering whether you would actually enjoy annelids
for breakfast.
=end item

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
                - !!perl/hash:Perl6::Perldoc::Block::para 
                  content: 
                    - !!perl/hash:Perl6::Perldoc::FormattingCode::I 
                      content: 
                        - The rain in Spain falls mainly on the plain.
                    - "\n"
                - !!perl/hash:Perl6::Perldoc::Block::para 
                  content: 
                    - "This is a common myth and an unconscionable slur on the Spanish\n\
                      people, the majority of whom are extremely attractive.\n"
              number: 1
            - "\n"
            - !!perl/hash:Perl6::Perldoc::Block::item 
              content: 
                - !!perl/hash:Perl6::Perldoc::Block::para 
                  content: 
                    - !!perl/hash:Perl6::Perldoc::FormattingCode::I 
                      content: 
                        - The early bird gets the worm.
                    - "\n"
                - !!perl/hash:Perl6::Perldoc::Block::para 
                  content: 
                    - "In deciding whether to become an early riser, it is worth\n\
                      considering whether you would actually enjoy annelids\n\
                      for breakfast.\n"
              number: 2
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
