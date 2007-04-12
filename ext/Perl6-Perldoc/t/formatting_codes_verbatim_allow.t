my $perldoc_data = <<'END_PERLDOC';
=config C<> :allow<B>
=config V<> :allow<N>
C<code is N<of course> B<verbatim>>
V<verbatim is N<of course> B<verbatim>>

END_PERLDOC

my $expected_structure = <<'END_EXPECTED';
errors: []

tree: !!perl/hash:Perl6::Perldoc::Document 
  content: 
    - !!perl/hash:Perl6::Perldoc::Block::pod 
      config: {}

      content: 
        - !!perl/hash:Perl6::Perldoc::Directive::config 
          is_blank_terminated: 1
          options: 
            allow: &1 
              - B
          style: directive
          target: C<>
          typename: config
        - !!perl/hash:Perl6::Perldoc::Directive::config 
          is_blank_terminated: 1
          options: 
            allow: &2 
              - N
          style: directive
          target: V<>
          typename: config
        - !!perl/hash:Perl6::Perldoc::Block::para 
          config: {}

          content: 
            - !!perl/hash:Perl6::Perldoc::FormattingCode::C 
              config: 
                allow: *1
              content: 
                - "code is N<of course> "
                - !!perl/hash:Perl6::Perldoc::FormattingCode::B 
                  config: {}

                  content: 
                    - verbatim
                  is_verbatim: 1
                  left_delim: <
                  right_delim: ">"
                  style: formatting
                  typename: B
              is_verbatim: 1
              left_delim: <
              right_delim: ">"
              style: formatting
              typename: C
            - "\n"
            - !!perl/hash:Perl6::Perldoc::FormattingCode::V 
              config: 
                allow: *2
              content: 
                - "verbatim is "
                - !!perl/hash:Perl6::Perldoc::FormattingCode::N 
                  config: {}

                  content: 
                    - of course
                  is_verbatim: 1
                  left_delim: <
                  right_delim: ">"
                  style: formatting
                  typename: N
                - " B<verbatim>"
              delim_nesting: 0
              is_verbatim: 1
              left_delim: <
              right_delim: ">"
              style: formatting
              typename: V
            - "\n"
          is_blank_terminated: 1
          is_verbatim: ~
          style: implicit
          typename: para
      is_implicit: 1
      style: implicit
      typename: pod
  terminator: (?!)
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
