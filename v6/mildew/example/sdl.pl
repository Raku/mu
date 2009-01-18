my $p5 = ::P5Interpreter.new();
my $P5Interpreter = $p5;
use SDL::App:from<perl5>;
use SDL::Rect:from<perl5>;
use SDL::Color:from<perl5>;

my $App = $p5.eval('"SDL::App"');
my $app = $App.new("-width",640,"-height",480,"-depth",16);

my $Rect = $p5.eval('"SDL::Rect"');
my $rect = $Rect.new('-height',100,'-width',100,'-x',270,'-y',190);

my $Color = $p5.eval('"SDL::Color"');
my $color = $Color.new('-r',0,'-g',0,'-b',255);

$app.fill( $rect, $color);
$app.update( $rect.FETCH );
$p5.eval('sleep 2');
