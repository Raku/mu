#!/usr/bin/pugs
use v6;

module HTTP::Status-0.0.1 {
    my %StatusCode = (
        100 => 'Continue',
        101 => 'Switching Protocols',
        102 => 'Processing',                      # WebDAV
        200 => 'OK',
        201 => 'Created',
        202 => 'Accepted',
        203 => 'Non-Authoritative Information',
        204 => 'No Content',
        205 => 'Reset Content',
        206 => 'Partial Content',
        207 => 'Multi-Status',                    # WebDAV
        300 => 'Multiple Choices',
        301 => 'Moved Permanently',
        302 => 'Found',
        303 => 'See Other',
        304 => 'Not Modified',
        305 => 'Use Proxy',
        307 => 'Temporary Redirect',
        400 => 'Bad Request',
        401 => 'Unauthorized',
        402 => 'Payment Required',
        403 => 'Forbidden',
        404 => 'Not Found',
        405 => 'Method Not Allowed',
        406 => 'Not Acceptable',
        407 => 'Proxy Authentication Required',
        408 => 'Request Timeout',
        409 => 'Conflict',
        410 => 'Gone',
        411 => 'Length Required',
        412 => 'Precondition Failed',
        413 => 'Request Entity Too Large',
        414 => 'Request-URI Too Large',
        415 => 'Unsupported Media Type',
        416 => 'Request Range Not Satisfiable',
        417 => 'Expectation Failed',
        422 => 'Unprocessable Entity',            # WebDAV
        423 => 'Locked',                          # WebDAV
        424 => 'Failed Dependency',               # WebDAV
        500 => 'Internal Server Error',
        501 => 'Not Implemented',
        502 => 'Bad Gateway',
        503 => 'Service Unavailable',
        504 => 'Gateway Timeout',
        505 => 'HTTP Version Not Supported',
        507 => 'Insufficient Storage'            # WebDAV
    );
    
    for %StatusCode.kv -> $code, $message is copy {
        $message .= uc;
        $message ~~ s:g/<[ \-]>/_/;
        
        &HTTP::Status::("RC_" ~ $message) := sub () { $code };
        &HTTP::Status::("RC_" ~ $message) is export(:MANDATORY);
    }
    
    our &RC_MOVED_TEMPORARILY is export(:MANDATORY) ::= &RC_FOUND;
    
    sub status_message ($code) { %StatusCode{$code} }
    
    sub is_info     ($code) is export(:MANDATORY) { 100 <= $code < 200 }
    sub is_success  ($code) is export(:MANDATORY) { 200 <= $code < 300 }
    sub is_redirect ($code) is export(:MANDATORY) { 300 <= $code < 400 }
    sub is_error    ($code) is export(:MANDATORY) { 400 <= $code < 500 }
    
    sub is_server_error ($code) is export(:DEFAULT)   { 500 <= $code < 600 }
    sub is_client_error ($code) is export(:DEFAULT)   { 600 <= $code < 700 }
}
