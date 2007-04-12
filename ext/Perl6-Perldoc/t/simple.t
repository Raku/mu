my $perldoc_data = <<'END_PERLDOC';
=begin pod

=head1 A heading

This is Pod too. Specifically, this is a simple C<para> block

    $this = pod('also');  # Specifically, a code block

=end pod

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      content: 
        - !!perl/hash:Perl6::Perldoc::Block::pod 
          content: 
            - !!perl/hash:Perl6::Perldoc::Block::head1 
              content: 
                - "A heading\n"
              level: 1
              style: abbreviated
              typename: head1
            - !!perl/hash:Perl6::Perldoc::Block::para 
              content: 
                - "This is Pod too. Specifically, this is a simple "
                - !!perl/hash:Perl6::Perldoc::FormattingCode::C 
                  config: {}
                  content: 
                    - para
                  left_delim: <
                  right_delim: ">"
                  style: formatting
                  typename: C
                - " block\n"
              style: implicit
              typename: para
            - !!perl/hash:Perl6::Perldoc::Block::code 
              content: 
                - "    $this = pod('also');  # Specifically, a code block\n"
              style: implicit
              typename: code
          style: delimited
          typename: pod
      style: implicit
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
