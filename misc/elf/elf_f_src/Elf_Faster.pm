use Prelude;
use Match;
use IRx1_Nodes;
use IRx1_FromAST;
use IRx1_Analysis;
use EmitSimpleP5;
use EmitFasterP5;
use PrimitivesP5;
use Parser;
use Compiler;
use CommandLine;

$*compiler0 = Compiler.new('emitter',EmitFasterP5.new(),'parser',Parser.new('is_for_active_runtime',1),'is_for_active_runtime',1);
$*compiler1 = Compiler.new('emitter',EmitFasterP5.new(),'parser',Parser.new('is_for_active_runtime',0),'is_for_active_runtime',0);
Program.new().main(@*ARGS);

