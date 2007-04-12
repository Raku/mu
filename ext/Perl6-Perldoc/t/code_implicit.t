my $perldoc_data = <<'END_PERLDOC';
This ordinary paragraph introduces a code block:

    $this = 1 * code('block');
    $which.is_specified(:by<indenting>);
    # Code is B<verbatim>

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      content: 
        - !!perl/hash:Perl6::Perldoc::Block::para 
          content: 
            - "This ordinary paragraph introduces a code block:\n"
        - !!perl/hash:Perl6::Perldoc::Block::code 
          content: 
            - "    $this = 1 * code('block');\n    $which.is_specified(:by<indenting>);\n    # Code is B<verbatim>\n"
          is_verbatim: 1
          style: implicit
          typename: code
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
