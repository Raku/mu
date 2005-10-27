module HTTP::Server::Simple-6.00;

use v6;

has Int $.port;
has IO  $.socket;
has IO  $.remote;

submethod BUILD ( :$port ) {
    $.port = $port || 8080;
}

method bad_request {
    my $doc = '
<html>
  <head>
    <title>Bad Request</title>
  </head>
  <body>
    <h1>Bad Request</h1>

    <p>Your browser sent a request which this web server could not
      grok.</p>
  </body>
</html>';
    my $length = $doc.chars;
    $.remote.print("HTTP/1.0 400 BAD REQUEST\r\n");
    $.remote.print("Content-Type: text/html; charset=UTF-8\r\n");
    $.remote.print("Content-Length: $length\r\n\r\n");
    $.remote.print($doc);
}

method handle_request {
    my $doc = "
<html>
  <head>
    <title>Hello!</title>
  </head>
  <body>
    <h1>Congratulations!</h1>

    <p>You now have a functional HTTP::Server::Simple running.
      </p>

    <p><i>(If you're seeing this page, it means you haven't subclassed
      HTTP::Server::Simple, which you'll need to do to make it
      useful.)</i>
      </p>
  </body>
</html>";
    my $length = $doc.chars;
    $.remote.print("HTTP/1.0 200 OK\r\n");
    $.remote.print("Content-Type: text/html; charset=UTF-8\r\n");
    $.remote.print("Content-Length: $length\r\n\r\n");
    $.remote.print($doc);
}

method handler { ./handle_request }

method parse_request {
    my Str $chunk = $.remote.readline;
    defined $chunk or return undef;
    $chunk ~~ m:P5/^(\w+)\s+(\S+)(?:\s+(\S+))?\r?$/;
    my Str $method = $0 || '';
    my Str $uri    = $1 || '';
    my Str $proto  = $2 || '';
    return( $method, $uri, $proto );
}

method parse_headers {
    my @headers;
    my Str $chunk = '';
    while $chunk = $.remote.readline {
        $chunk ~~ s:P5/[\r\n\s]+$//;
        if $chunk ~~ m:P5/^([\w\-]+): (.+)/ {
            @headers.push( $0 => $1 );
        }
        last if $chunk ~~ m:P5/^$/;
    }
    return \@headers;
}

method prepare {
    $*IN  := $.remote;
    $*OUT := $.remote;
    my ( $method, $uri, $proto ) = ./parse_request
        or do { ./bad_request; return 0 };
    $proto ||= 'HTTP/0.9';
    $uri ~~ m:P5/([^?]*)(?:\?(.*))?/;
    my Str $file  = $0 || '';
    my Str $query = $1 || '';
    unless $method ~~ m:P5/^(?:GET|POST|HEAD)$/ {
        ./bad_request;
        return 0;
    }
    %*ENV<SERVER_PROTOCOL> = $proto;
    %*ENV<SERVER_PORT>     = $.port;
    %*ENV<SERVER_NAME>     = 'localhost';
    %*ENV<PATH_INFO>       = $file;
    %*ENV<REQUEST_URI>     = $uri;
    %*ENV<REMOTE_ADDR>     = '';
    %*ENV<REMOTE_HOST>     = '';
    %*ENV<QUERY_STRING>    = $query;
    my $headers = ./parse_headers or do { ./bad_request; return 0 };
    for @{ $headers } -> $header {
        my Str $tag = $header.key.uc;
        $tag = "HTTP_$tag"
            unless $tag ~~ m:P5/^(?:CONTENT_(?:LENGTH|TYPE)|COOKIE)$/;
        %*ENV{$tag} = $header.value;
    }
    return 1;
}

method run {
    $.socket = $.port.listen;
    say "You can connect to http://localhost:8080";
    loop {
        my %env = %*ENV;
        $.remote = $.socket.accept;
        ./handler if ./prepare;
        $.remote.close;
        %*ENV = %env;
    }
}

=head1 NAME

HTTP::Server::Simple - A minimal HTTP server

=head1 SYNOPSIS

    class MyServer is HTTP::Server::Simple;

    method handler {
        $.remote.say('Hello World!');
    }


    use MyServer;
    my $s = MyServer.new;
    $s.run;
    
=head1 DESCRIPTION

Perl 6 port of L<HTTP::Server::Simple>.

=head1 AUTHOR

Sebastian Riedel <sri@oook.de>

Based upon L<HTTP::Server::Simple> by Jesse Vincent

=head1 LICENSE

This library is free software . You can redistribute it and/or modify
it under the same terms as perl itself.

=cut
