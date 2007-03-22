package Regexp::Parser::ConvertToSix;

use warnings;
use strict;
use Carp;

use version; our $VERSION = qv('0.0.0');

# Other recommended modules (uncomment to use):
#  use IO::Prompt;
#  use Perl6::Export;
#  use Perl6::Slurp;
#  use Perl6::Say;

use Regexp::Parser;

#
# Kludge around a Regex::Parser bug.
#
if($Regexp::Parser::VERSION <= 0.20) {
  eval <<'END';
#line 1 "Regexp::Parser bug fix/workaround"
    # Regexp::Parser bug fix/workaround
    package Regexp::Parser::anyof_class;
    sub visual {
      my $self = shift;
      if (ref $self->{data}) {
        $self->{data}->visual;
      }
      else {
        # The actual bug is in Handlers.pm init() - the \$how 's.
        #join "", "[", $self->{how}, ($self->{neg} ? '^' : ''),
        #     $self->{type}, $self->{how}, "]";
        join "", "[", ${$self->{how}}, ($self->{neg} ? '^' : ''),
             $self->{type}, ${$self->{how}}, "]";
      }
    }
END
}
# End of kludge.


local $Regexp::Parser::ConvertToSix::env;

{
  package Regexp::Parser::__object__;

  sub _join_visual6 {
    my($o)=@_;
    join("",map { $_->visual6() } @{$o->data});
  }
  sub _backref {
    my($o)=@_;
    my $idx = $o->{nparen};
    if($Regexp::Parser::ConvertToSix::env->{preserve_capture_numbers}) {
      '$'.$idx;
    } else {
      $Regexp::Parser::ConvertToSix::env->{capture_map}{$idx};
    }
  }
}


{
  # \A ^ \B \b \G \Z \z $
  package Regexp::Parser::anchor;
  sub visual6 {
    my($o)=@_;
    my $pat = $o->visual();
    my $flag_m = $o->{flags} & $o->{rx}->FLAG_m;
    return '^' if $pat eq '\A';
    return '$' if $pat eq '\z';
    return '\n?$' if $pat eq '\Z';
    return ($flag_m ? '^^' : '^') if $pat eq '^';
    return ($flag_m ? '$$' : '\n?$') if $pat eq '$';
    return '\B' if $pat eq '\B';
    return '\b' if $pat eq '\b';
    if($pat eq '\G') {
     $Regexp::Parser::ConvertToSix::env->{required_modifiers} .= ':pos';
      #XXX - /(?<=\G..)./g
      return '#(\G)';
    }
    die "didn't implement $pat"
  }
}
{
  # . \C
  package Regexp::Parser::reg_any;
  sub visual6 {
    my($o)=@_;
    my $pat = $o->visual();
    if($pat eq '.') {
      my $flag_s = $o->{flags} & $o->{rx}->FLAG_s;
      $flag_s ? '.' : '\N';
    } elsif($pat eq '\C') {
      '[:bytes .]'
    } else {
      die "didn't implement $pat";
    }
  }
}
{
  # \w \W
  package Regexp::Parser::alnum;
  sub visual6 { shift->visual() }
  sub _can_be_inlined_in_character_list {1}
}
{
  # \s \S
  package Regexp::Parser::space;
  sub visual6 { shift->visual() }
  sub _can_be_inlined_in_character_list {1}
}
{
  # \d \D
  package Regexp::Parser::digit;
  sub visual6 { shift->visual() }
  sub _can_be_inlined_in_character_list {1}
}
{
  package Regexp::Parser::anyof;
  local $Regexp::Parser::ConvertToSix::CHARSET_SENSE = 0;
  sub visual6 {
    my($o)=@_;
    my $neg = $o->neg ? 1 : 0;
    my $op = $o->neg() ? '-' : '+';
    my $pat = "";
    my $cls = "";
    local $Regexp::Parser::ConvertToSix::CHARSET_SENSE = $neg;
    for my $n (@{$o->data}) {
      if($n->_can_be_inlined_in_character_list) {
        $cls .= $n->visual6();
      } else {
        $pat .= $op."[$cls]" if $cls ne ''; $cls = '';
        $pat .= $n->visual6();
      }
    }
    $pat .= $op."[$cls]" if $cls ne '';
    $pat =~ s/^\+//;
    '<'.$pat.">";
  }
}
{
  package Regexp::Parser::anyof_char;
  sub visual6 { shift->visual() }
  sub _can_be_inlined_in_character_list {1}
}
{
  package Regexp::Parser::anyof_range;
  sub visual6 {
    my($o)=@_;
    my($lhs,$rhs) = map {$_->visual6()} @{$o->data};
    "$lhs..$rhs";
  }
  sub _can_be_inlined_in_character_list {1}
}
{
  package Regexp::Parser::anyof_class;
  sub visual6 {
    my($o)=@_;
    if($o->data eq 'POSIX') {
      my $sense = $Regexp::Parser::ConvertToSix::CHARSET_SENSE;
      die "assert: posix class used outside charclass" if !defined $sense;
      my $neg = ($o->neg ? 1 : 0) ^ ($sense || 0);
      my $sgn = $neg ? '-' : '+';
      $sgn.$o->{type}; # Not $o->type();
    } else {
      $o->data->visual6();
    }  
  }
  sub _can_be_inlined_in_character_list {
    my($o)=@_;
    return 0 if $o->data eq 'POSIX';
    if(ref($o->data) eq 'ARRAY') {
      for my $n (@{$o->data}) {
        return 0 if !$n->_can_be_inlined_in_character_list;
      }
      return 1;
    }
    return $o->data->_can_be_inlined_in_character_list;
  }
}
{
  # not needed
  package Regexp::Parser::anyof_close;
}
{
  package Regexp::Parser::prop;
  sub visual6 {
    my($o)=@_;
    my $sense = $Regexp::Parser::ConvertToSix::CHARSET_SENSE;
    my $inside_class = defined $sense;
    my $neg = ($o->neg ? 1 : 0) ^ ($sense || 0);
    my $sgn = $neg ? '-' : '+';
    my $name = $o->type;
    if($name =~ s/^is//i){
      $name = ucfirst $name;
      $name = "is".$name;
    }
    $inside_class ? $sgn.$name : '<'.$sgn.$name.'>';
  }
  sub _can_be_inlined_in_character_list {0}
}
{
  # \X
  package Regexp::Parser::clump;
  sub visual6 { "[<-isM><isM>*]" }
}
{
  # |
  package Regexp::Parser::branch;
  sub visual6 {
    my($o)=@_;
    my $env = $Regexp::Parser::ConvertToSix::env;
    my $first = $env->{capture_var}[-1];
    my $next_cap = $first;
    my $pat = join("|",map {
      my $a = $_;
      local $env->{capture_var} = [@{$env->{capture_var}}];
      $env->{capture_var}[-1] = $first;
      my $re = join("", map {$_->visual6()} @$a);
      my $cap = $env->{capture_var}[-1];
      $next_cap = $cap if $cap > $next_cap;
      $re;
    } @{$o->data});
    $env->{capture_var}[-1] = $next_cap;
    $pat;
  }
}
{
  package Regexp::Parser::exact;
  sub visual6{
    my($o)=@_;
    my $flag_x = $o->{flags} & $o->{rx}->FLAG_x;
    my $pat = "";
    for my $c (@{$o->{vis}}) {
      $c =~ s/^\\N{(.+)}$/\\c[$1]/;
      $c =~ s/^([^\w\s])$/\\$1/;
      $c =~ s/^([\s])$/\\$1/ if !$flag_x;
      $pat .= $c;
    }
    $pat;
  }
  sub _can_be_inlined_in_character_list {1}
}
{
  package Regexp::Parser::quant;
  sub visual6 {
    my($o)=@_;
    my($min,$max)= ($o->min,$o->max);
    $min = 0 if $min eq "";
    my $pat = $o->data->visual6();
    my $type = $o->type;
    return $pat."*" if $type eq 'star';
    return $pat."+" if $type eq 'plus';
    return $pat."?" if $min == 0 && $max == 1;
    return $pat."**{$min..$max}";
  }
}
{
  # *? +? {,}?
  package Regexp::Parser::minmod;
  sub visual6 {
    my($o)=@_;
    my $pat = $o->data->visual6();
    $pat."?";
  }
}
{
  # ( non-capturing
  package Regexp::Parser::group;
  sub visual6 {
    my($o)=@_;
    my $pat = $o->_join_visual6();
    my $mod = "";
    $mod .= ":i " if $o->on =~ /i/;
    $mod .= ":i<0> " if $o->off =~ /i/;
    '['.$mod.$pat.']';
  }
}
{
  # ( capturing
  package Regexp::Parser::open;
  sub visual6 {
    my($o)=@_;
    my $pat;
    my $capture_var = $Regexp::Parser::ConvertToSix::env->{capture_var};
    {
      my $down = [@$capture_var,0];
      local $Regexp::Parser::ConvertToSix::env->{capture_var} = $down;
      $pat = $o->_join_visual6();
    }
    my $own_capture_var = [@$capture_var];
    $capture_var->[-1]++;

    my $idx = $o->{nparen};
    if($Regexp::Parser::ConvertToSix::env->{preserve_capture_numbers}) {
      $Regexp::Parser::ConvertToSix::env->{capture_map}{$idx} = $o->_capture($idx);
      " \$$idx := [$pat] ";
    } else {
      $Regexp::Parser::ConvertToSix::env->{capture_map}{$idx} = $o->_capture(@$own_capture_var);
      "($pat)";
    }
  }
  sub _capture {
    my($o,@cap)=@_;
    my $head = shift @cap;
    '$'.$head.join("",map {"[$_]"} @cap);
  }
}
{
  # ) closing
  # not needed
  package Regexp::Parser::close;
}
{
  # ) for non-captures
  # not needed
  package Regexp::Parser::tail;
}
{
  # \1 (backrefs)
  package Regexp::Parser::ref;
  sub visual6 {
    my($o)=@_;
    $o->_backref;
  }
}
{
  # not needed
  package Regexp::Parser::assertion;
}
{
  # (?=) (?<=)
  package Regexp::Parser::ifmatch;
  sub visual6 {
    my($o)=@_;
    my $pat = $o->_join_visual6();
    my $dir = $o->{'dir'};
    $dir>0 ? "<?before $pat>" : "<?after $pat>";
  }
}
{
  # (?!) (?<!)
  package Regexp::Parser::unlessm;
  sub visual6 {
    my($o)=@_;
    my $pat = $o->_join_visual6();
    my $dir = $o->{'dir'};
    $dir>0 ? "<?!before $pat>" : "<?!after $pat>";
  }
}
{
  # (?>)
  package Regexp::Parser::suspend;
  sub visual6 {
    my($o)=@_;
    my $pat = $o->_join_visual6();
    "[$pat]:";
  }
}
{
  # (?(n)t|f)
  package Regexp::Parser::ifthen;
  sub visual6 {
    my($o)=@_;
    my $test = $o->data->[0]->visual6();
    my $crufty = $o->data->[1]->data;
    my $then = $crufty->[0][0]->visual6();
    my $else = "<null>";
    $else = $crufty->[1][0]->visual6() if @{$crufty} > 1;
    "<{$test ?? /$then/ :: /$else/}>";
  }
}
{
  # the N in (?(N)t|f) when N is a number
  package Regexp::Parser::groupp;
  sub visual6 {
    my($o)=@_;
    $o->_backref;
  }
}
{
  # (?{ ... })
  package Regexp::Parser::eval;
  sub visual6 {
    my($o)=@_;
    my $code = join("",$o->data);
    '{ eval(q{'.$code.'},:lang<perl5>) }';
  }
}
{
  # (??{ ... })
  package Regexp::Parser::logical;
  sub visual6 {
    my($o)=@_;
    my $code = join("",$o->data);
    '<{ eval(q{'.$code.'},:lang<perl5>) }>';
  }
}
{
  package Regexp::Parser::flags;
  sub visual6 {
    my($o)=@_;
    return ' :i ' if $o->on =~ /i/;
    return ' :i<0> ' if $o->off =~ /i/;
    return '';
  }
}


package Regexp::Parser;

sub visual6 {
  my($self)=@_;
  $self->convert_to_six();
}

sub convert_to_six {
  my($self,$preserve_capture_numbers)=@_;
  my $env = { preserve_capture_numbers => $preserve_capture_numbers,
              capture_map => {0=>'$/'},
              capture_var => [0],
              required_modifiers => "",
              avoid_delim => ""
          };
  $self->{ConvertToSix_env} = $env;
  local $Regexp::Parser::ConvertToSix::env = $env;
  join("",map{$_->visual6()} @{$self->root});
}

sub convert_string_with_match_vars_to_six {
  my($self,$str)=@_;
  my $slash = 0; # need_slash
  my $map = $self->{ConvertToSix_env}{capture_map};
  my $noesc = qr/(?<!\\)|(?<=\\\\)/;
  $str =~ s/$noesc\$(\d+)/$map->{$1} || ''/eg;
  $str =~ s/$noesc\$\-\[(\d+)\]/$slash++ if $1 == 0;'{'.$map->{$1}.'.from}'/eg;
  $str =~ s/$noesc\$\+\[(\d+)\]/$slash++ if $1 == 0;'{'.$map->{$1}.'.to}'/eg;
  $str =~ s/$noesc\$&/\$\//g and $slash++;
  $self->{ConvertToSix_env}{avoid_delim} .= '/' if $slash;
  $str;
}

sub convert_literal_to_six {
  use re 'eval';
  my($self,$lit5)=@_;
  $lit5 =~ s/^\s+//; $lit5 =~ s/\s+$//;

  my $modre = qr/[imsxogce]/;
  my %close = ('('=>qr/\)/,'{'=>qr/}/,'['=>qr/]/,'<'=>qr/>/);
  my $cl = sub{my $s = $_[0]; $close{$s}||qr/$s/ };
  my($op,$delim,$pat5,$delimC,$subst,$mod5);
  if($lit5 =~ /^()(\/)(.+?)(\/)()($modre*)$/) {
    ($op,$delim,$pat5,$delimC,$subst,$mod5)=($1,$2,$3,$4,$5,$6);
  }
  elsif($lit5 =~ /^(qr|m)(.)(.+?)((??{$cl->($2)}))()($modre*)$/) {
    ($op,$delim,$pat5,$delimC,$subst,$mod5)=($1,$2,$3,$4,$5,$6);
  }
  elsif($lit5 =~ /^(s)(.)(.+?)((??{$cl->($2)}))\2?(.+?)\4($modre*)$/){
    ($op,$delim,$pat5,$delimC,$subst,$mod5)=($1,$2,$3,$4,$5,$6);
  }
  else { die "invalid literal: $lit5" }

  my $premod = $mod5;
  $premod =~ s/[^msx]//g;
  my $use_pat = "(?:$pat5)";
  $use_pat = "(?$premod)$use_pat" if $premod ne "";
  $self->regex($use_pat);
  my $pat = $self->convert_to_six();
  $pat =~ s/^\s*\[//; $pat =~ s/\]\s*$//;

  my $rest = "";
  if($op eq 's') {
    my $subst6 = $self->convert_string_with_match_vars_to_six($subst);

    my $avoid = qr/[ $self->{ConvertToSix_env}{avoid_delim}]/;
    if($delim =~ $avoid) {
      die 'assert' if '{' =~ $avoid;
      $delim = '{'; $delimC = '}';
    }

    $subst6 = "eval(q{$subst6},:lang<perl5>)" if $mod5 =~ /e/;
    $rest = ($delim eq $delimC ? '' : $delim).$subst6.$delimC;
  }

  my $mod = "";
  $mod .= $self->{ConvertToSix_env}{required_modifiers};
  $mod .= ':i' if $mod5 =~ /i/;
  $mod .= ':g' if $mod5 =~ /g/;
  $mod .= ':pos' if $mod5 =~ /c/;

  my $op6 .= ($op eq 's') ? 's' : ($delim eq '/' && $mod eq '') ? '' : 'rx';
  $op6.$mod.$delim.$pat.$delimC.$rest;
}

package Regexp::Parser::ConvertToSix;

sub repl {
  print "Enter a Perl 5 regexp pattern or literal.\n";
  while(<>) {
    chomp;
    my $parser = Regexp::Parser->new($_);  
    print "Regexp::Parser regenerates it as: "; print $parser->visual(),"\n";
    if(/^(\/|(m|s|qr)\W)/) {
      print "As literal: ";
      eval { print $parser->convert_literal_to_six($_),"\n"; } or print $@;
    } else {
      print "Normal Perl 6 pattern: ";
      print $parser->convert_to_six(),"\n";
      print "Backwards compatible:  ";
      print $parser->convert_to_six(1),"\n";
      use Data::Dumper;
      print Dumper($parser);
    }
    my $str = '$&, $1, $2, $3, $4, $5, $6';
    print "'$str' -> '",$parser->convert_string_with_match_vars_to_six($str),"'\n";
  }
}

# IMPLEMENTATION TODO
#
# The order dependency on avoid_delim is troubling.  Perhaps it should be local()?  And it's fragile ('[').
#

1;
__END__

=head1 NAME

Regexp::Parser::ConvertToSix - Convert regular expressions from Perl 5 syntax to Perl 6 syntax.


=head1 VERSION

This document describes Regexp::Parser::ConvertToSix version 0.0.1


=head1 SYNOPSIS

    use Regexp::Parser::ConvertToSix;

    Regexp::Parser->new('(?:(a)b{2,3})')->convert_to_six() #=> '[foo**{2..3}]'
    Regexp::Parser->new('$0 $1 $+[1]')->convert_string_with_match_vars_to_six() #=> '$/ $0 $0.to'
    Regexp::Parser->new('s/(a(b)(?:c))/$2/')->convert_literal_to_six() #=> 's/(a(b)[c])/$0[0]/'

    # interactive shell
    perl -MRegexp::Parser::ConvertToSix -e 'Regexp::Parser::ConvertToSix::repl'

  
=head1 DESCRIPTION

Given a string containing a Perl 5 regular expression, provides the
Perl 6 equivalent.  Intended to aid learing, and converting Perl 5
code to Perl 6.  It works with both patterns (the stuff between
C<//>s), and literals (the entire C</foo/ixg> or C<s/foo/bar/>).


=head1 INTERFACE 

=for author to fill in:
    Write a separate section listing the public components of the modules
    interface. These normally consist of either subroutines that may be
    exported, or methods that may be called on objects belonging to the
    classes provided by the module.


=head1 DIAGNOSTICS

=for author to fill in:
    List every single error and warning message that the module can
    generate (even the ones that will "never happen"), with a full
    explanation of each problem, one or more likely causes, and any
    suggested remedies.

=over

=item C<< Error message here, perhaps with %s placeholders >>

[Description of error here]

=item C<< Another error message here >>

[Description of error here]

[Et cetera, et cetera]

=back


=head1 CONFIGURATION AND ENVIRONMENT

Regexp::Parser::ConvertToSix requires no configuration files or environment variables.


=head1 DEPENDENCIES

C<Regexp::Parser>


=head1 INCOMPATIBILITIES

None reported.


=head1 BUGS AND LIMITATIONS

=over

=item Largely untested.  Needs to be run over re_tests.  //x especially untested.
=item Needs t/ tests.

=item Pod documentation is unfinished.

=item C<\G> support is limited.  Should convert non-leftmost C<\G> to rx/{temp $pos_target = .pos}[...{ $pos_target == .pos or fail }...]/;

=item C<$^N $+ $` $'> are not supported in patterns.  C<$^N $` $'> are not supported in strings.  C<$` $'> should perhaps be done with C<.prematch .postmatch>.  What else is missing?

=item Loses comments in //x.  Avoidable?

=item More selective slashification would be nice.

=item Regexp::Parser::ConvertToSix currently parasitizes Regexp::Parser.  A more conventional object oriented approach might be nice.

=item Explain "Subroutine visual redefined at Regexp::Parser bug fix/workaround line 3.", or better, get the Regexp::Parser bug fixed.

=item Use something like IO::Prompt in the repl.

=back

Please report any bugs or feature requests to
C<bug-regexp-parser-converttosix@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org>.


=head1 AUTHOR

Pugs Team  C<< <perl6-compiler@perl.org> >>


=head1 LICENCE AND COPYRIGHT

Copyright (c) 2006, Pugs Team C<< <perl6-compiler@perl.org> >>. All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.


=head1 DISCLAIMER OF WARRANTY

BECAUSE THIS SOFTWARE IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE SOFTWARE, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
PROVIDE THE SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE
ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE IS WITH
YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL
NECESSARY SERVICING, REPAIR, OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE SOFTWARE AS PERMITTED BY THE ABOVE LICENCE, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL,
OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE
THE SOFTWARE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.
