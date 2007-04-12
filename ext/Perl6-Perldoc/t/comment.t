my $perldoc_data = <<'END_PERLDOC';
=item # Retreat to remote Himalayan monastery

=item # Learn the hidden mysteries of space and time

=item # Achieve enlightenment

=begin comment
=item # Prophet!
=end comment

=item # Profit!

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
                - "Retreat to remote Himalayan monastery\n"
            - !!perl/hash:Perl6::Perldoc::Block::item 
              content: 
                - "Learn the hidden mysteries of space and time\n"
            - !!perl/hash:Perl6::Perldoc::Block::item 
              content: 
                - "Achieve enlightenment\n"
            - !!perl/hash:Perl6::Perldoc::Block::comment 
              content: 
                - "=item # Prophet!\n"
            - !!perl/hash:Perl6::Perldoc::Block::item 
              content: 
                - "Profit!\n"
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
