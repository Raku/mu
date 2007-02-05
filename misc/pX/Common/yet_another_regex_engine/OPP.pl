=pod
This is yet another operator precedence parser implementation.
It is a first, rough, non-running draft.
It was a transliteration of the redsix OPP.  The original ruby is
rather more readable, and might be a useful reference.
The transliteration is still a work in progress.
Most significantly, all the search and match stuff needs to be adapted
to a new set of search and match apis.

Perhaps this file should be in its own directory, rather than here as
part of yet_another_regex_engine.  I would ideally like to have this
be standalone, but I'm afraid that might be problematic.  So I'll try
to minimize dependencies, but we'll see.

--putter 2007-Feb-04
=cut


###*** Rule

# class Rule;
#   attr_accessor :name,:raw_patA
#   sub new(*args) $o->{name,$o->{raw_patA=*args }
#   sub search(str,pos=0,can_skip=true)
#     (pos..str.length).each{|p|
#       scanner = StringScanner.new(str);
#       scanner.pos = pos;
#       ev = Engine.new(scanner,[:apat,[:skimCo,self]],nil,$grammar).run;
#       return ev if ev && ev.bool;
#       break if !can_skip;
#     }
#     return Match.new_from(str,pos).failed!;
#   }
#   sub skimC
#     [:apat,[:seq,[
#      [:match_open,self,$o->{name],
#      [:seq,$o->{raw_patA],
#      [:match_close]
#     ]]]
#   end    
# }
# class String
#   sub search(rule,pos=0,can_skip=true) rule.search(self,pos,can_skip) }
# }

###*** OperatorPrecedenceParser

{
  package OperatorPrecedenceParser;
  use Digest::MD5 qw(md5_base64);
  local $depth = 0;
  sub log {
    my($o,$msg)=@_;
    if(1) {
      my $sp = "    " x $depth;
      my($id) = md5_base64("$o") =~ /^(.{6})/;
      print STDERR "${sp}OPP $id $msg\n";
    }
  }
  { package Edge; sub new { bless {},shift; }}
  local $BOS = Edge->new;
  local $EOS = Edge->new;
  #attr_accessor :tokens,:ws,:s,:token_filter
  sub new {
    my($cls,$tokens,$ws,$token_filter)=@_;
    my $o = bless {},$cls;
    ($o->{tokens},$o->{ws},$o->{token_filter})=($tokens,$ws,$token_filter);
    $o;
  }
#  sub skimC(token_filter=nil)
#    [:apat,[:full_skimCf,OperatorPrecedenceParser::Parsing.new($o->{tokens,$o->{ws,token_filter)]]
#  }
#  sub skimCf {
#    my($o,@args)=@_;
#    @@depth += 1
#    scanner=args[1]
#    $o->{s=scanner
#    ok = parse
#    if ok
#      $o->log("OPP-PARSE succeeded\n"
#      @@depth -= 1
#      args[4] = ok
#      args
#    else 
#      $o->log("OPP-PARSE failed\n"
#      @@depth -= 1
#      false
#    }
#  }
  #attr_accessor :saw_ws
  sub eat_ws {
    my($o)=@_;
    my $m = $o->{ws}->search($o->{s}{string},$o->{s}{pos},undef);
    if($m->bool) {
      $o->{s}{pos} = $m->to;
      $m;
    }
    else {
      undef;
    }
  }
  sub lexeme_compatible_with_context_p {
    my($o,$l,$prepost,$ws)=@_;
    if($ws ne 'dontcare') {
      my $lws = $l->{whitespace_on_left};
      return 0 if not (do{
        if(!defined $lws) { 1 }
        elsif('prohibit') { !$ws }
        elsif('require') { !!$ws }
        elsif('ws_irrelevant') { 1 }
        else { die("assert: $lws") }
      });
    }
    if(defined $prepost) {
      my $lexp = $l->{expression_on_left};
      return 0 if not (do{
        if($lexp eq 'need_expr') {
          if($prepost eq 'pre')     { 0 }
          elsif($prepost eq 'post') { 1 }
          elsif($prepost eq 'post_commalike') { $l->{is_commalike} }
          else { die('assert') }
        }
        elsif($lexp eq 'no_expr_needed') { $prepost eq 'pre' }
        else { die("assert $lexp"); }
       });
    }
    return 1;
  }
  #attr_accessor :prepost
  sub lex {
    my($o,$current_lexemes)=@_;
    $o->log("LEX"); $o->log("Lex'ing at ".$o->{s}{pos}". >".slice($o->{s}{string},$o->{s}{pos},20));
    $o->{saw_ws} = $o->eat_ws;
    $o->log("Lex saw ws") if $o->{saw_ws};
    $o->log("Lex context: ".$o->{prepost}." ".($o->{saw_ws} ? 'ws' : 'nows'));
    my $matches = [];
#    my $ss = StringScanner.new($o->{s.string); ss.pos = $o->{s.pos; #XXX
  # $o->log("BEGIN =================================");
    for my $l (@$current_lexemes) {
      my $key = $l->{competition_key};
#      $o->log("x# #{key.inspect} \t#{l.name}";
      my $len = undef;
      if($key && ref($key) eq 'Regexp') {
        my $x = (ss.scan(key) || (ss.eos? && "".match(key) && "")); #XXX
#  $o->log(l.name.inspect+' '+(x ? 'OK' : "FAILED #{key.inspect}")+"";
        next if not $x;
        ss.pos = $o->{s}{pos};
        $len = length("$x"); #XXX
      }
      next if not $o->lexeme_compatible_with_context_p($l,undef,$o->{saw_ws});
      my $m = $l->search($o->{s}{string},$o->{s}{pos},undef);
      next if not $m->bool;
      $len = $m.hash[$key].to - $m.from if !$len && key.is_a?(Symbol); #XXX - Architecture problem - is this actually used?
      $len = $m.to - $m.from if !$len;
      push(@$matches,[$m,$l,$len]);
    };
    $o->log("Lex #{matches.size} candidates");
    my $ms = [sort{ $b->[2] <=> $a->[2]} @$matches];
    if(@$ms) {
      my $at = $ms->[0][2];
      $o->log("Lex target #{at}");
#      p "=================== ",at,ms[0][0].inspect,'-----';
      $ms = [map{
        my $a = $_;
        $o->log("...".$a->[2]."   ".$a->[1]{name});
        $at == $a->[2] && $o->lexeme_compatible_with_context_p($a->[1],$o->{prepost},'dontcare');
      }
    }
    $o->log("Lex ".(@$ms+0)." matches");
    if(@$ms+0 > 1) {
      print STDERR "\nWARNING: ambiguous parse: ".$ms->[0][0]."\n";
      for my $a (@$ms) { print STDERR "  ".$a->[1]{name}."\n"; }
    }
    my $m = !@$ms ? undef : $ms->[0][0];
    $o->log("Lex RESULT: ".$m->inspect);
    return undef if !$m;
    $o->{s}{pos} = $m->to;
    $o->log("Lex now at ".$o->{s}{pos});
    $o->{saw_ws} = 0;
    $m;
  }
  #attr_accessor :ops,:opands,:ends,:ends_sz
  #attr_accessor :tok
  #attr_accessor :lexemes
  sub init {
    my($o)=@_;
    $o->{lexemes} = [map{$_->{first_lexeme}} @{$o->{tokens}}];
    $o->{lexemes_at_top_level} = [@{$o->{lexemes}}];
    if($o->{token_filter}) {
      my $top_level_tokens =
        [map{ $o->{token_filter}($_) ? ($_) : ()} @{$o->{tokens}}];
      $o->{lexemes_at_top_level} = [map{$_->{first_lexeme}} @$top_level_tokens];
    }
    $o->{prepost} = 'pre';
    $o->{saw_ws} = 0;

    $o->{ops} = [$BOS];
    $o->{opands} = [];
    $o->{ends} = []; $o->{ends_sz} = [];
    $o->{tok} = undef;
  }
  sub parse {
    my($o)=@_;
    $o->log("OPP-PARSE");
    $o->init;
    catch(:parse_fail) {parser_loop}
  }
  sub log_state {
    my($o)=@_;
    $o->log("Currently:\n\topands: #{$o->{opands.inspect}\n\tops: #{$o->{ops.inspect}\n\ttok: #{$o->{tok.inspect}";
  }
  sub parser_loop {
    my($o)=@_;
    while(1) {
      $o->log("Loop");
      $o->get_token;
      $o->log_state if 0;
      if($o->{tok} == $EOS) {
        break;
      }
      $o->log($o->{tok}{rule}->inspect) if 0;
      if(! $o->{tok}{rule}{token}{is_operator}) {
        $o->operand;
      }
      elsif(@{$o->{ops}}+0 == 1) {
        $o->shift_;
      }
      elsif($o->{tok}{rule} == $o->{ends}[-1]) {
        $o->crunch;
      }
      elsif($o->{ops}[-1]{rule}{expression_on_right} eq 'no_expr_needed' &&
            !$o->{ops}[-1]{rule}{next_lexeme}) {
        $o->reduce;
      }
      elsif($o->{tok}{rule}{expression_on_left} eq 'no_expr_needed') {
        $o->shift_;
      }
      else {
        my $st = $o->{ops}[-1]{rule}{precedence_on_right};
        my $tk = $o->{tok}{rule}{precedence_on_left};
        if     ($st < $tk){ $o->shift_; }
        } elsif($st > $tk){ $o->reduce; }
        } elsif($st == 0 && $tk == 0) {
          $o->shift_ # eg, prelist with circumfix;
        } elsif($o->{tok}{rule}{associativity} eq 'right') {
          $o->shift_;
        } else {
          $o->reduce;
        }
      }
    }
    $o->log("EOS processing");
    while(@{$o->{ops}}+0 > 1) {
      $o->reduce;
    }    
    my $tmp1 = $o->{ops};
    if(!(   ($tmp1 && @$tmp1 == 1 && $tmp1->[0] == $BOS) # aka @ops == [BOS]
         && (@{$o->{opands}}+0 == 1)
         && (@{$o->{ends}}+0 == 0) # aka @ends == []
         && ($o->{tok} == $EOS))) {
      $o->error;
    }
    $o->{s}{pos} = $o->{saw_ws}{from} if $o->{saw_ws};
    $o->{opands}[0];
  }
  sub get_token {
    my($o)=@_;
    return if $o->{tok};
    my $close = $o->{ends}[-1];
    my $current_lexemes = $close ? [@{$o->{lexemes}},$close] : $o->{lexemes_at_top_level};
    $o->{tok} = $o->lex($current_lexemes) || $EOS;
  }
  sub operand {
    my($o)=@_;
    $o->log("operand");
    my $m = $o->{tok}; $o->{tok} = undef;
    push(@{$o->{opands}},$m);
    my $currently_argument_list_top_level = $o->{token_filter} && !$o->{ends}[-1];
    $o->{prepost} = $m->{rule}{token}{operand_post};
    $o->{prepost} = 'post' if ($o->{prepost} eq 'post_commalike' &&
                               !$currently_argument_list_top_level);
  }
  sub shift_ {
    my($o)=@_;
    $o->log("shift_");
    my $m = $o->{tok}; $o->{tok} = undef;
    push(@{$o->{ops}},$m);
    my $eme = $m->{rule}{next_lexeme};
    if($eme) {
      push(@{$o->{ends}},$eme);
      push(@{$o->{ends_sz}},(@{$o->{ops}}+0));
    }
    $o->{prepost} = $o->prepost_from_rule($o->{ops}[-1]{rule});
  }
  sub reduce {
    my($o)=@_;
    $o->log("reduce");
    my $m0 = pop(@{$o->{ops}});
    my $t = $m0->{rule}{token};
    my $count = $t->{part_count} - 1; # all but m0 are on $o->{opands}
    if({$o->{opands}}+0 < $count) {
      $o->log("reduce problem - not enough opands (".(@{$o->{opands}}+0)." vs $count)";
      $o->log($o->{opands}->inspect);
      $o->error;
    }
    my $parts = [slice(@{$o->{opands}},0-$count,$count,())];
    my $m = $t->create_match([$m0,@$parts]);
    push(@{$o->{opands}},$m);
    $o->{prepost} = $o->prepost_from_rule($o->{ops}[-1]{rule}) if $o->{ops}[-1] != $BOS;
  }
  sub crunch {
    my($o)=@_;
    $o->log("crunch");
    die('assert') if $o->{tok}{rule} != $o->{ends}[-1];
    while($o->{ops}{size} > $o->{ends_sz}[-1]) {
      $o->reduce;
    }
    die('assert') if $o->{ops}[-1]{rule}{token} != $o->{ends}[-1]{token};
    pop(@{$o->{ends}}); pop(@{$o->{ends_sz}});
    push(@{$o->{opands}},pop(@{$o->{ops}}));
    $o->shift_;
  }
  sub prepost_from_rule {
    my($o,$r)=@_;
    $r->{expression_on_right} eq 'need_expr' ? 'pre' : 'post';
  }
  sub error {
    my($o)=@_;
    $o->log("ERROR");
    $o->log(" op  ".$o->{ops}[-1]->inspect);
    $o->log(" tok ".$o->{tok}->inspect);
    throw :parse_fail,undef #XXX
  }
}

###**** Tokens

package Lexeme;
@ISA=qw(Rule);
  #attr_accessor :token
  #attr_accessor :competition_key
  #attr_accessor :whitespace_on_left
  #attr_accessor :expression_on_left,:expression_on_right
  #attr_accessor :precedence_on_left,:precedence_on_right
  #attr_accessor :associativity
  #attr_accessor :next_lexeme
  #attr_accessor :is_commalike

  sub new {
    my($cls,$name,$f,$len)=@_;
    my $o = $cls->SUPER::new($name,$f);
    $o->{competition_key} = $len;
  }

{
  package Token_Operand;
  #attr_accessor :first_lexeme
  #attr_accessor :operand_post
  sub is_operator { 0 }
  sub new {
    my($cls,$name,$f,$len)=@_;
    my $o = bless {}, $cls;
    my $l = Lexeme->new($name,$f,$len);
    $o->{first_lexeme} = $l;
    $l->{token} = $o;
    $l->{whitespace_on_left} = 'ws_irrelevant';
    $l->{expression_on_left} = 'no_expr_needed';
    $l->{expression_on_right} = 'no_expr_needed';
    $l->{is_commalike} = 0;
    $o->{operand_post} = 'post';
  }
}
{
  package Token_Operator;
  #attr_accessor :fixity,:strings,:precedence,:assoc,:ws_policy
  #attr_accessor :arity,:part_count
  #attr_accessor :name
  #attr_accessor :first_lexeme
  #attr_accessor :fixity_extra
  local $precgen;
  sub is_operator { 1 }
  sub new {
    my($cls,@args)=@_;
    my $o = bless {}, $cls;
    my($o->{name},$o->{fixity},$o->{fixity_extra},$o->{strings},$o->{precedence},$o->{assoc},$o->{ws_policy},$o->{is_commalike})=@args;
    $o->{precedence} ||= $precgen->default_precedence;
    $o->{name} = $o->{name} || $o->{fixity}.":".join(' ',@{$o->{strings}});


    my $strings = $o->{strings};
    my $name = $o->{name};

    # build lexemes
    $lexemes = [];
    for(my $i=0;$i<@$strings;$i++){
      $str_or_re = $strings->[$i];
      my $re = undef;
      my $native_re = undef;
      if(!defined ref($str_or_re)) {
        my $pat = $str_or_re;
        $pat =~ s/(\W)/\\$1/g;
        $native_re = qr/$pat/;
        $re = apat($native_re);
      } else {
        $re = $str_or_re;
      }
      push(@$lexemes,Lexeme->new("$name:$i",$re,$native_re));
    }
    my $regexps = [@$lexemes];

    # ... and connect them
    $o->{first_lexeme} = $lexemes->[0];
    $lexemes->[0]{next_lexeme} = $lexemes->[1] || undef;
    $lexemes->[1]{next_lexeme} = undef if $lexemes->[1];

    # ... and set defaults
    for my $l @$lexemes {
      $l->{token} = $o;
      $l->{whitespace_on_left} = undef;
      $l->{associativity} = $o->{assoc};
      $l->{is_commalike} = $o->{is_commalike};
    }
    $lexemes->[0]{whitespace_on_left} = $o->{ws_policy};

    # finish up
    my $set = sub {
      my($n,$el,$pl,$er,$pr)=@_;
      $lexemes->[n]{expression_on_left} = $el ? 'need_expr' : 'no_expr_needed';
      $lexemes->[n]{expression_on_right} = $er ? 'need_expr' : 'no_expr_needed';
      $lexemes->[n]{precedence_on_left} = $pl ? $o->{precedence} : 0;
      $lexemes->[n]{precedence_on_right} = $pr ? $o->{precedence} : 0;
    }
    my($arity,$part_count);

    my $fixity = $o->{fixity};
    if($fixity eq 'prefix') {
      $arity = 1; $part_count = 2;
      $set->(0, undef,undef, 'e','p');
    } elsif($fixity eq 'postfix') {
      $arity = 1; $part_count = 2;
      $set->(0, 'e','p', undef,undef);
    } elsif($fixity eq 'circumfix') {
      $arity = 1; $part_count = 3;
      $set->(0, undef,undef, 'e',undef);
      $set->(1,'e',undef, undef,undef);
    } elsif($fixity eq 'infix') {
      $arity = 2; $part_count = 3;
      $set->(0, 'e','p', 'e','p');
    } elsif($fixity eq 'ternary') {
      $arity = 3; $part_count = 5;
      $set->(0, 'e','p', 'e',undef);
      $set->(1, 'e',undef, 'e','p');
    } elsif($fixity eq 'postcircumfix') {
      $arity = 2; $part_count = 4;
      $set->(0, 'e','p', 'e',undef);
      $set->(1, 'e',undef, undef,undef);
    } else {
      die "assert $fixity"
    }
    $o->{arity} = $arity;
    $o->{part_count} = $part_count;
    $lexemes->[-1]{precedence_on_right} = 0 if $o->{fixity_extra} eq 'list';
  }

#XXX
#  def create_match(parts)
#$LogP.print parts.inspect
#    parts.sort!{|a,b| a.from <=> b.from }
#    m = Match.new_from(parts[0].on_str)
#    m.rule = self
#    m.hash[:parts] = parts
#    m.from = parts[0].from
#    m.close!(parts[-1].to)
#    m
#  end

  {
    package PrecedenceGenerator
    #attr :delta
    sub new {
      my($cls)=@_;
      my $o = bless {}, $cls;
      $o-{delta} = 0.5;
    }
    sub consume_a_delta {
      my($o)=@_; 
      my $delta = $o->{delta};
      $o->{delta} = $delta / 2;
      $delta;
    }
    sub default_precedence { 1 }
    sub new_precedence_tighter_than {
      my($o,$prec)=@_;
      $prec + $o->consume_a_delta;
    }
    sub new_precedence_looser_than {
      my($o,$prec)=@_;
      $prec - $o->consume_a_delta;
    }
  }
  $precgen = PrecedenceGenerator->new;
  sub tighter {
    my($o)=@_; 
    $precgen->new_precedence_tighter_than($o->{precedence});
  }
  sub looser {
    my($o)=@_; 
    $precgen->new_precedence_looser_than($o->{precedence});
  }
  sub equiv {
    my($o)=@_;
    $o->{precedence};
  }
}


1;
__END__
#; Local Variables:
#; perl-indent-level: 2
#; perl-continued-statement-offset: 2
#; perl-continued-brace-offset: -2
#; indent-tabs-mode: nil
#; End:
#; vim: shiftwidth=2:
