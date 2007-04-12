my $perldoc_data = <<'END_PERLDOC';
=begin END

This is a para

=end END

This is a para too

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      content: 
        - !!perl/hash:Perl6::Perldoc::Block::END 
          content: 
            - !!perl/hash:Perl6::Perldoc::Block::para 
              content: 
                - "This is a para\n"
            - !!perl/hash:Perl6::Perldoc::Block::para 
              content: 
                - "This is a para too\n"
          typename: END
      typename: pod
  typename: (document)
warnings: 
  - Ignored explicit '=end END' (END blocks run to end-of-file) at line 5

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
