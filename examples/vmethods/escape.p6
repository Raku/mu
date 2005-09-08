#!/usr/bin/pugs
# Please remember to update examples/output/vmethods/escape if you change this
# file so its output will change.

use v6;

multi sub escape (Str $source is copy, Str $lang) {
  given $lang {
    when "html" {
      $source ~~ s:Perl5:g/([&<>"'-])/{ #"#--vim
        $0 eq "&" ?? "&amp;"  !!
        $0 eq "<" ?? "&lt;"   !!
        $0 eq ">" ?? "&gt;"   !!
        $0 eq '"' ?? "&quot;" !!
        $0 eq "'" ?? "&#39;"  !!
        $0 eq "-" ?? "&#45;"  !! die
      }/;
    }

    when "doublequote" {
      $source ~~ s:Perl5:g/([\\"])/{
        $0 eq "\\" ?? "\\\\" !!
        $0 eq "\"" ?? "\\\"" !! die
      }/;
    }

    default {
      die '"lang" parameter must be "html"|"doublequote"' unless
        $lang eq any<html doublequote>;
    }
  }

  return $source;
}

say 'This string should be quoted: "\\<>&-"'.escape(:lang<html>);
say 'This string should be quoted: "\\<>&-"'.escape(:lang<doublequote>);
