my $perldoc_data = <<'END_PERLDOC';
=config head1 :numbered
=config head2 :numbered
=config head3 :!numbered

=head1 The Problem
=head1 The Solution
=head2   Analysis
=head3     Overview
=head3     Details
=head2   Design
=head1 The Implementation

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      content: 
        - !!perl/hash:Perl6::Perldoc::Block::head1 
          content: 
            - "The Problem\n"
          level: 1
          number: 1
        - !!perl/hash:Perl6::Perldoc::Block::head1 
          content: 
            - "The Solution\n"
          level: 1
          number: 2
        - !!perl/hash:Perl6::Perldoc::Block::head2 
          content: 
            - "Analysis\n"
          level: 2
          number: 2.1
        - !!perl/hash:Perl6::Perldoc::Block::head3 
          content: 
            - "Overview\n"
          level: 3
        - !!perl/hash:Perl6::Perldoc::Block::head3 
          content: 
            - "Details\n"
          level: 3
        - !!perl/hash:Perl6::Perldoc::Block::head2 
          content: 
            - "Design\n"
          level: 2
          number: 2.2
        - !!perl/hash:Perl6::Perldoc::Block::head1 
          content: 
            - "The Implementation\n"
          level: 1
          number: 3
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
