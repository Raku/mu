class Log::Entry;

# the log line in unmodified form
has $.line;

# theses entries are in the "combined log format" (apache)
has $.remote_host;
has $.dummy;
has $.user;
has $.time;
has $.request;
has $.status;
has $.size;
has $.referer;
has $.user_agent;

token remotehost  { \V+ };         # anything but horizontal spaces
token dummy       { \V+ };         # I don't know what that should look like
token user        { \V+ };         # never seen that in action either
token rtime       { '[' <-[\]]>+ ']'};  # not elaborate, but it will do for now 
token request     { '"' <(         # we don't need the delimiting "" in our match
        $<requesttype> := <+[A-Z]> # usually GET or POST
        \s 
        $<url> := (\v+)
        \s 
        'HTTP/'
        $<protocolversion> := (\d+ '.' \d+) # mostly 1.0 or 1.1
        )> '"'};
token status      { \d**{3} };
token size        { \d+ };
token referer     { '"' <( <-["]>* )> '"' | - };
token useragent   { '"' <( .* )>  '"' };


method parse {
    my $m =  $.line ~~ m/
        ^<remotehost> \s
         <dummy> \s
         <user> \s
         <rtime> \s
         <request> \s
         <status> \s
         <size> \s
         <referer> \s
         <useragent> $ /;
    if $m {
        $.remote_host = ~$m<remotehost>;
        $.dummy       = ~$m<dummy>;
        $.user        = ~$m<user>;
        $.rtime       = $m<rtime>;
        $.request     = $m<request>;
        $.status      = +$m<status>;
        $.size        = +$m<size>;
        $.referer     = ~$m<referer>;
        $.user_agent  = ~$m<useragent>;
        say "matched successfully";
    } else {
        say "Failed to match line: $.line";
    }
}


