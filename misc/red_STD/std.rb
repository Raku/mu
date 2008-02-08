# A ruby transliteration of src/perl6/STD.pm
# Sources to crib from:
#  v6/v6-KindaPerl6/compiled/ruby-kp6-mp6/kp6_runtime.rb
#  misc/pX/Common/redsix/redsix
#  misc/pX/Common/yet_another_regex_engine/Regexp_ModuleA.pm (perhaps)
# We'll pay the pain of sticking with ruby 1.8 for now, since folks
# seem to have trouble finding the real 1.9.0.
#
# Notes
# 
#
require 'prelude'

class Perl < Grammar
  def dot_ws
    true
  end

  def expect_infix
    (infix && star{infix_postfix_meta_operator}) || infix_prefix_meta_operator || infix_circumfix_meta_operator
  end

  ## I'm unclear on what infix() is, and thus on how to get from expect_infix() to _EXPR().
  def prefix; false; end
  def prefix_circumfix_meta_operator; false; end
  def expect_postfix; false; end
  def adverbs; false; end

  def _EXPR
    see_that_mountain?
  end

  def expect_term
    b = pos
    # queue up the prefixes to interleave with postfixes
    pre = star lambda{
      m = _match_from(pos)
      if _prefix = prefix
        m[:prec] = _prefix[:prec]
      elsif precircum = prefix_circumfix_meta_operator
        m[:prec] = precircum[:prec]
      else
        return false
      end
      # XXX assuming no precedence change
      star{prefix_postfix_meta_operator}
      dot_ws
      m
    }

    _noun = noun or return fail_at(b)

    # also queue up any postfixes, since adverbs could change things
    postfix = star{expect_postfix}
    dot_ws
    ques{adverbs}

    # now push ops over the noun according to precedence.
    #    { make $¢.nounphrase(:noun($<noun>), :pre(@<pre>), :post(@<post>)) }
    _match_from(b,{:noun=>_noun,:postfix=>postfix})
  end
  
  def noun
    ##(pair || package_declarator || scope_declarator || plurality_declarator ||
    ## routine_declarator || regex_declarator || type_declarator || circumfix ||
    ( variable || value || subcall || capterm || sigterm || term || statement_prefix)
  end
  def variable; false; end
  def value; quote || number || version || fulltypename; end
  def quote; false; end
  def number; dec_number || integer || rad_number; end
  def integer
    _match_pat %r{
      0 ( b [01]+           ( _ [01]+ )*
        | o [0-7]+         ( _ [0-7]+ )*
        | x [0-9a-fA-F]+ ( _ [0-9a-fA-F]+ )*
        | d \d+               ( _ \d+)*
        | \d+(_\d+)*
        )
    | \d+(_\d+)*
    }x
  end
  def dec_number; false; end
end

p Perl.new(('42')).noun()
p Perl.new(('42')).expect_term()
p Perl.new(('42'))._EXPR()

say "Starting...";
my $r = Perl.new(('42')).expect_infix();
say $r;
exit;
say "WHAT\t", $r.WHAT;
say "BOOL\t", $r.bool;
say "FROM\t", $r.from;
say "TO\t", $r.to;

## vim: expandtab sw=4
