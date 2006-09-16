use v6-alpha;

use Rosetta::Shell;

my Str @cmd_line_args = @*ARGS.grep:{ $_ ~~ m/^<[a-zA-Z:_]>+$/ };
my ($engine_name, @user_lang_prefs) = @cmd_line_args;

$engine_name //= 'Rosetta::Engine::Example';
@user_lang_prefs = 'en'
    if @user_lang_prefs == 0;

Rosetta::Shell::main( engine_name => $engine_name,
    user_lang_prefs => @user_lang_prefs );
