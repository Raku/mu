
sub unindent {
  my($s,$leave_indent)=@_;
  $leave_indent ||= "";
  $s =~ /^(\s*)$leave_indent/;
  my $indent = $1;
  $s =~ s/^$indent//mg;
  $s;
}
sub text2file {
  my($text,$file)=@_;
  open(F,">$file") or die "open() failed on $file: $!";
  print F $text;
  close F;
}

sub load_paragraphs {
  my($file)=@_;
  my $text = `cat $file`;
  $text =~ s/\A\s*#.*\n//mg;
  $text =~ s/[ ]*#.*//g;
#  print $text;
  $text =~ s/^([ \t]*\n)*//;
  @x = split(/\n\n+/,$text);
#  print join("\n---\n",@x);
  @x;
}

1;
__END__

