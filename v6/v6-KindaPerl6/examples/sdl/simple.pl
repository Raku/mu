class Main {
    use Wrap;
    use SDL::App:from<perl5>;
    my $app = SDL::App.new("-width","640","-height","480","-depth","16");
}
