#!/usr/bin/pugs
use v6;

use Rosetta::Shell;

#my Str @user_lang_prefs = grep { $_ ~~ m/^<[a-zA-Z:_]>+$/ } @*ARGS;
my Str @cmd_line_args = grep { $_ ~~ m:perl5/^[a-zA-Z:_]+$/ } @*ARGS; #:
my ($engine_name, @user_lang_prefs) = *@cmd_line_args;

$engine_name //= 'Rosetta::Engine::Example';
@user_lang_prefs = 'en'
    if @user_lang_prefs == 0;

Rosetta::Shell::main( 'engine_name' => $engine_name,
    'user_lang_prefs' => @user_lang_prefs );
