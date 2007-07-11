class Main {
    use KindaPerl6::Runtime::Perl5::Wrap;
    use SDL::App:from<perl5>;
    my $app = SDL::App.new("-width",640,"-height",480,"-depth",16);

    use SDL::Rect:from<perl5>;
    my $rect = SDL::Rect.new(
        "-height",100,
        "-width",100,
        "-x",270,
        "-y",390,
   );
   use SDL::Color:from<perl5>;
   my $color = SDL::Color.new(
           "-r",0,
           "-g",0,
           "-b",255,
   );
   $app.fill($rect,$color);
}

