package PerlMix::_Filter;
use Filter::Simple sub {
  my($cls,$here,@args)=@_;
  $_ = filter_string($cls,$_,$here,@args);
};
sub filter_string {
  my($cls,$s,$here,@args)=@_;
  my $v = $here->compile_api0->compile(code=>$s,@args);
  print STDERR $v; exit;
  defined $v ? $v : "";
}
1;

package PerlMix;
use base Exporter;
@PerlMix::EXPORT_OK = qw(perlmix_compile);
@PerlMix::EXPORT    = @PerlMix::EXPORT_OK;
require Regexp_ModuleA;
BEGIN{ Regexp::ModuleA::Api::PreludeA->import(); }

sub import {
  my($cls,@args)=@_;
  my $nofilter = 0;
  if(@args && $args[0] eq ':nofilter') {
    $nofilter = 1;
    shift(@args);
  }
  $cls->export_to_level(1, @args);
  if(!$nofilter) {
    my $caller = [caller(1)];
    @_ = ('PerlMix::_Filter',$cls,
          pkg=>$cls, caller=>$caller, import_args=>[@args]);
    goto &PerlMix::_Filter::import;
  }
}

sub compile_header_text {
  <<END;
  BEGIN{ Regexp::ModuleA::Api::FilterWithenvA->import; }
  BEGIN{ Regexp::ModuleA::Api::PreludeA->import; }
END
}

sub compile_api0 {shift}
sub compile {
  my($cls,%args)=@_;
  my $m = $cls->parse($args{code},%args);
  my $code = $cls->perlmix_codgen($m,%args);
  $code = $cls->compile_header_text().$code;
  $code;
}
sub parse {
  my($cls,$code,%args)=@_;
  my $m = $cls->regex_api0->match_named_regex('perlmix_compile_input',$code,pkg=>$cls);
  $m;
}
sub codegen {
  my($cls,$m,%args)=@_;
  my $map = $cls->gather_methods(filter=>qr/^compile__(.+)__$/);
  my $map_code = {map{($_,UNIVERSAL::can($cls,$map->{$_}))} keys %$map};
  local $Regexp::ModuleA::Scratch::codegen::map_code = $map_code;
  $cls->codegen_match($m);
}
sub codegen_match {
  my($cls,$m)=@_;
  my $r = $$m->{RULE};#X
  my $map_code = $Regexp::ModuleA::Scratch::make0_from_match::map_code;
  my $meth = $map_code->{$r} || $map_code->{DEFAULT} || sub{$_[2]};
  $meth->($cls,$m);
}

print regex_api0();

regex_api0->define_named_regex('perlmix_compile_input','<perlmix_code>');
regex_api0->define_named_regex('perlmix_code','<perlmix_atom>*');
regex_api0->define_named_regex('perlmix_atom','<perlmix_early> :: || <perlmix_rule_declaration> :: || <perlmix_p5_line> ');
regex_api0->define_named_regex('perlmix_perl5_line','<!before \s*\}EARLY> \N*\n');
sub codegen__perlmix_perl5_line__ {my($cls,$m)=@_; "$m";}
regex_api0->define_named_regex('perlmix_early',' \s* <wb> EARLY\{ <perlmix_code> \s* \}EARLY');
sub codegen__perlmix_early__ {""}
sub perlmix_early__post_action {
  my($cls,$m)=@_;
  my $code = $cls->codegen_match($m->{perlmix_code}[0]);
  eval "package $cls; $code"; die if $@;
  $m;
}
regex_api0->define_named_regex('perlmix_rule_declaration','regex \s+ (\w+) \s+ (<-[\{]>*) \{( .+? )\n\}');
sub codegen__perlmix_rule_declaration__ {
  my($cls,$m)=@_;
  my($name,$mods,$body)=@$m;
  $mods =~ s/\s+$//;
  my($nameq,$modsq,$bodyq)=map{s/([\\\'])/\\$1/g;"'$_'"}($name,$mods,$body);
  my $modsarg = $mods eq "''" ? '' : ",mods=>$modsq";
  "regex_api0->define_named_regex($nameq,WITH"."ENV{$bodyq}WITHENV$modsarg);";
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
