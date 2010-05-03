use SDL::App:from<perl5>;
use SDL::Rect:from<perl5>;
use SDL::Color:from<perl5>;

my $App = EXTERNAL::eval_perl5('"SDL::App"');
my $app = $App.new("-width",640,"-height",480,"-depth",16);

my $Rect = EXTERNAL::eval_perl5('"SDL::Rect"');
my $rect = $Rect.new('-height',100,'-width',100,'-x',270,'-y',190);

my $Color = EXTERNAL::eval_perl5('"SDL::Color"');
my $color = $Color.new('-r',0,'-g',0,'-b',255);

$app.fill( $rect, $color);
$app.update( $rect.FETCH );
EXTERNAL::eval_perl5('sleep 5');
