#!/usr/bin/pugs
use v6;

use Locale::KeyedText;
use MyLib;

###########################################################################
###########################################################################

sub main () {
    # user indicates language pref as command line argument
    my Str @user_lang_prefs = grep { $_ ~~ m/^<[a-zA-Z]>+$/ } @*ARGS;
    @user_lang_prefs = 'Eng'
        if @user_lang_prefs == 0;

    my Locale::KeyedText::Translator $translator .= new(
        'set_names'    => ['MyApp::L::', 'MyLib::L::'],
        'member_names' => @user_lang_prefs,
    );

    show_message( $translator, Locale::KeyedText::Message.new(
        'msg_key' => 'MYAPP_HELLO' ) );

    INPUT_LINE:
    {
        show_message( $translator, Locale::KeyedText::Message.new(
            'msg_key' => 'MYAPP_PROMPT' ) );

        my Str $user_input = $*IN;
        $user_input .= chomp;

        # user simply hits return on an empty line to quit the program
        last INPUT_LINE
            if $user_input eq q{};

        try {
            my Num $result = MyLib.my_invert( $user_input );
            show_message( $translator, Locale::KeyedText::Message.new(
                'msg_key'  => 'MYAPP_RESULT',
                'msg_vars' => {
                    'ORIGINAL' => $user_input,
                    'INVERTED' => $result,
                },
            ) );
            CATCH {
                # input error, detected by library
                show_message( $translator, $! );
            }
        };

        redo INPUT_LINE;
    }

    show_message( $translator, Locale::KeyedText::Message.new(
        'msg_key' => 'MYAPP_GOODBYE' ) );

    return;
}

sub show_message (Locale::KeyedText::Translator $translator,
        Locale::KeyedText::Message $message) {
    my Str $user_text = $translator.translate_message( $message );
    if (!$user_text) {
        print $*ERR "internal error: can't find user text for a message:\n"
            ~ '   ' ~ $message.as_debug_string() ~ "\n"
            ~ '   ' ~ $translator.as_debug_string() ~ "\n";
        return;
    }
    say $*OUT $user_text;
    return;
}

###########################################################################
###########################################################################

my Str %text_stringsE is readonly = (
    'MYAPP_HELLO' => q[Welcome to MyApp.],
    'MYAPP_GOODBYE' => q[Goodbye!],
    'MYAPP_PROMPT'
        => q[Enter a number to be inverted, or press ENTER to quit.],
    'MYAPP_RESULT' => q[The inverse of "<ORIGINAL>" is "<INVERTED>".],
);

module MyApp::L::Eng {
    sub get_text_by_key (Str $msg_key) returns Str {
        return %text_stringsE{$msg_key};
    }
} # module MyApp::L::Eng

###########################################################################
###########################################################################

my Str %text_stringsF is readonly = (
    'MYAPP_HELLO' => q[Bienvenue allé MyApp.],
    'MYAPP_GOODBYE' => q[Salut!],
    'MYAPP_PROMPT'
        => q[Fournir nombre être inverser, ou appuyer sur]
           ~ q[ ENTER être arrêter.],
    'MYAPP_RESULT' => q[Renversement "<ORIGINAL>" est "<INVERTED>".],
);

module MyApp::L::Fre {
    sub get_text_by_key (Str $msg_key) returns Str {
        return %text_stringsF{$msg_key};
    }
} # module MyApp::L::Fre

###########################################################################
###########################################################################

main();
