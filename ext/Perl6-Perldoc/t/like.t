my $perldoc_data = <<'END_PERLDOC';
=config head1 :formatted<B>
=config head2  :like<head1> :formatted<I>
=head1 TOP HEADING
=head2 Second Heading

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      content: 
        - !!perl/hash:Perl6::Perldoc::Directive::config 
          options: 
            formatted: &1 
              - B
          style: directive
          target: head1
          typename: config
        - !!perl/hash:Perl6::Perldoc::Directive::config 
          options: 
            formatted: 
              - I
            like: 
              - head1
          style: directive
          target: head2
          typename: config
        - !!perl/hash:Perl6::Perldoc::Block::head1 
          config: 
            formatted: *1
          content: 
            - !!perl/hash:Perl6::Perldoc::FormattingCode::B 
              content: 
                - "TOP HEADING\n"
              is_implicit: 1
              style: formatting
              typename: B
          level: 1
          options: {}
          style: abbreviated
          typename: head1
        - !!perl/hash:Perl6::Perldoc::Block::head2 
          config: 
            formatted: 
              - B
              - I
          content: 
            - !!perl/hash:Perl6::Perldoc::FormattingCode::B 
              content: 
                - !!perl/hash:Perl6::Perldoc::FormattingCode::I 
                  content: 
                    - "Second Heading\n"
                  style: formatting
                  typename: I
              style: formatting
              typename: B
          level: 2
          style: abbreviated
          typename: head2
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
