#!pugs
# $Id: Memcached.pm,v 1.32 2004/07/27 17:07:04 bradfitz Exp $
#
# Copyright (c) 2003, 2004  Brad Fitzpatrick <brad@danga.com>
#
# See COPYRIGHT section in pod text below for usage and distribution rights.
#
# Perl6 port by Gaal Yahas

use v6;

class Cache::Memcached-0.0.1;

use Storable ();
use Socket qw( MSG_NOSIGNAL PF_INET IPPROTO_TCP SOCK_STREAM );
use IO::Handle ();
use Time::HiRes ();
use String::CRC32;
use Errno qw( EINPROGRESS EWOULDBLOCK EISCONN );

has Int   $.debug;
has Bool  $.no_rehash;
has Hash  $.stats;              # XXX: use a subtype instead!
has Int   $.compress_threshold; # "in bytes"; so subtype to be >= 0
has Bool  $.compress_enable;
has Code  $.stat_callback;
has Bool  $.readonly;
has Rat   $.select_timeout;
has Hash  $.pref_ip;
has Str   $.namespace
has Int   $.namespace_len;

has Array $:servers;
has Int   $:active;
has Array $:buckets;
has Int   $:bucketcount;
has Str   $:_single_sock;       # string-form ip, it seems
has Rat   $:_stime;


# flag definitions
our $F_STORABLE is constant = 1;
our $F_COMPRESS is constant = 2;

# size savings required before saving compressed value
our $COMPRESS_SAVINGS is constant = 0.20; # percent

our $HAVE_ZLIB;

INIT {
    $HAVE_ZLIB = eval "use Compress::Zlib (); 1;";
}

our $FLAG_NOSIGNAL = 0;
try { $FLAG_NOSIGNAL = MSG_NOSIGNAL; } # XXX: how does an imported constant look like?

my %host_dead;   # host -> unixtime marked dead until
my %cache_sock;  # host -> socket

my $PROTO_TCP;

our $SOCK_TIMEOUT = 2.6; # default timeout in seconds

submethod BUILD (
    ?$servers = [],
    ?$.debug = 0,
    ?$.no_rehash,
    ?$.pref_ip = {},
    ?$.compress_threshold,
    ?$.stat_callback,
    ?$.readonly,
    ?$.select_timout = 1.0,
    ?$.namespace = '',
    ?$.namespace_len = $.namespace.chars, # XXX: or bytes?
)
{
    $.stats = {};
    $.compress_enable = 1;
    .set_servers($servers);
}

method set_servers (?$list = []) {
    $:servers = $list;
    $:active  = $list.elems;
    $:buckets = undef;
    $:bucketcount = 0;

    $:_single_sock = undef;
    if $:active == 1 {
        $:_single_sock = $:servers.[0];
    }
    return $_; # XXX
}

method forget_dead_hosts {
    %host_dead = ();
}

sub _dead_sock ($sock, $ret, $dead_for) {
    if $sock ~~ /^Sock_(.+?):(\d+)$/ { # XXX: check regexp
        my $now = time();
        my ($ip, $port) = ($1, $2);
        my $host = "$ip:$port";
        $host_dead{$host} = $now + $dead_for
            if $dead_for;
        delete $cache_sock{$host};
    }
    return $ret;  # 0 or undef, probably, depending on what caller wants
}

sub _close_sock ($sock) {
    if $sock ~~ /^Sock_(.+?):(\d+)$/ {
        my ($ip, $port) = ($1, $2);
        my $host = "$ip:$port";
        close $sock;
        delete $cache_sock{$host};
    }
}

sub _connect_sock ($sock, $sin, ?$timeout = 0.25) { # sock, sin, timeout
    # make the socket non-blocking from now on,
    # except if someone wants 0 timeout, meaning
    # a blocking connect, but even then turn it
    # non-blocking at the end of this function

    if $timeout {
        IO::Handle::blocking($sock, 0);
    } else {
        IO::Handle::blocking($sock, 1);
    }

    my $ret = connect($sock, $sin);

    if !$ret && $timeout && $!==EINPROGRESS {

        my $win='';
        vec($win, fileno($sock), 1) = 1; # XXX: do we have vec?

        if select(undef, $win, undef, $timeout) > 0 {
            $ret = connect($sock, $sin);
            # EISCONN means connected & won't re-connect, so success
            $ret = 1 if !$ret && $!==EISCONN;
        }
    }

    unless $timeout { # socket was temporarily blocking, now revert
        IO::Handle::blocking($sock, 0);
    }

    # from here on, we use non-blocking (async) IO for the duration
    # of the socket's life

    return $ret;
}

# XXX: I killed the explicit class method semantics here, since nobody
# seemed to be using them?
method sock_to_host ($self: $host) { # (host)
    return $cache_sock{$host} if $cache_sock{$host};

    my $now = time();
    my ($ip, $port) = $host ~~ /(.*):(\d+)/;
    return undef if
        $host_dead{$host} && $host_dead{$host} > $now;
    my $sock = "Sock_$host";

    my $connected = 0;
    my $sin;
    my $proto = $PROTO_TCP ||= getprotobyname('tcp');

    # if a preferred IP is known, try that first.
    if $.pref_ip{$ip} {
        socket($sock, PF_INET, SOCK_STREAM, $proto);
        my $prefip = $.pref_ip{$ip};
        $sin = Socket::sockaddr_in($port,Socket::inet_aton($prefip));
        if _connect_sock($sock,$sin,0.1) {
            $connected = 1;
        } else {
            close $sock;
        }
    }

    # normal path, or fallback path if preferred IP failed
    unless $connected {
        socket($sock, PF_INET, SOCK_STREAM, $proto);
        $sin = Socket::sockaddr_in($port,Socket::inet_aton($ip));
        unless _connect_sock($sock,$sin) {
            return _dead_sock($sock, undef, 20 + int(rand(10)));
        }
    }

    # make the new socket not buffer writes.
    my $old = select($sock);
    $| = 1;
    select($old);

    return $cache_sock{$host} = $sock;
}

sub get_sock ($self: $key) { # (key)
    return $self.sock_to_host($:_single_sock) if $:_single_sock;
    return undef unless $:active;
    my $hv = ref $key ? int($key->[0]) : _hashfunc($key);

    $self.init_buckets() unless $:buckets;

    my $real_key = ref $key ? $key->[1] : $key;
    my $tries = 0;
    while $tries++ < 20 {
        my $host = $:buckets.[$hv % $:bucketcount];
        my $sock = $self.sock_to_host($host);
        return $sock if $sock;
        return undef if $.no_rehash;
        $hv += _hashfunc($tries . $real_key);  # stupid, but works
    }
    return undef;
}

method init_buckets {
    return if $:buckets;
    my $bu = $:buckets = [];
    for $:servers.elems -> $v {
        if (ref $v eq "ARRAY") {
            for (1..$v.[1]) { $bu.push($v.[0]); }
        } else {
            $bu.push($v);
        }
    }
    $:bucketcount = scalar $:buckets.elems;
}

method disconnect_all {
    my $sock;
    for values %cache_socka -> $sock {
        close $sock;
    }
    %cache_sock = ();
}

submethod _oneline ($sock, str $line) {
    my $res;
    my str $ret;
    my $offset = 0;

    # state: 0 - writing, 1 - reading, 2 - done
    my $state = defined $line ? 0 : 1;

    # the bitsets for select
    my ($rin, $rout, $win, $wout);
    my $nfound;

    my $copy_state = -1;
    temp %SIG<PIPE> = "IGNORE" unless $FLAG_NOSIGNAL; # XXX "%SIG"

    # the select loop
    while 1 {
        if ($copy_state!=$state) {
            last if $state==2;
            ($rin, $win) = ('', '');
            vec($rin, $sock.fileno(), 1) = 1 if $state==1; # XXX vec
            vec($win, $sock.fileno(), 1) = 1 if $state==0;
            $copy_state = $state;
        }
        $nfound = select($rout=$rin, $wout=$win, undef,
                         $.select_timeout);
        last unless $nfound;

        if vec($wout, $sock.fileno(), 1) {
            $res = $sock.send($line, $FLAG_NOSIGNAL);
            next
                if not defined $res and $!==EWOULDBLOCK;
            unless ($res > 0) {
                _close_sock($sock);
                return undef;
            }
            if $res == $line.bytes { # all sent # XXX "chars"? bytes?
                $state = 1;
            } else { # we only succeeded in sending some of it
                $line.substr(0, $res, ''); # delete the part we sent
            }
        }

        if vec($rout, fileno($sock), 1) {
            $res = $sock.sysread($ret, 255, $offset);
            next
                if !defined($res) and $!==EWOULDBLOCK; # XXX: $!
            if $res == 0 { # catches 0=conn closed or undef=error
                _close_sock($sock);
                return undef;
            }
            $offset += $res;
            if $ret.rindex("\r\n") + 2 == $ret.bytes {
                $state = 2;
            }
        }
    }

    unless $state == 2 {
        _dead_sock($sock); # improperly finished
        return undef;
    }

    return $ret;
}


method delete ($self: $key, $time) {
    return 0 if ! $:active || $.readonly;
    my $stime = Time::HiRes::time() if $.stat_callback;
    my $sock = $self.get_sock($key);
    return 0 unless $sock;

    $.stats.<delete>++;
    $key = ref $key ? $key->[1] : $key;
    $time = $time ? " $time" : "";
    my str $cmd = "delete {$self->{namespace} ~ $key ~ $time}\r\n";
    my $res = $self._oneline($sock, $cmd);

    if $.stat_callback {
        my $etime = Time::HiRes::time();
        $.stat_callback($stime, $etime, $sock, 'delete');
    }

    return $res eq "DELETED\r\n";
}

# Gaal: install three curried public methds that delegate work
# to one private method. Iffy, but so was the original code
# here which put the invocant second in _set's @_.

for <add replace set> -> $meth {
    %::«'$' ~ $meth» := _set.assuming(:cmdname($meth));
}

method _set ($self: $cmdname, $key, $val, int ?$exptime = 0) {
    return 0 if ! $:active || $.readonly;
    my $stime = Time::HiRes::time() if $.stat_callback;
    my $sock = $self.get_sock($key);
    return 0 unless $sock;

    $.stats.{$cmdname}++;
    my $flags = 0;
    $key = ref $key ? $key->[1] : $key;

    if ref $val {
        $val = Storable::nfreeze($val);
        $flags +|= F_STORABLE;
    }

    my $len = $val.bytes;

    if $.compress_threshold && $HAVE_ZLIB && $.compress_enable &&
        $len >= $.compress_threshold {

        my str $c_val = Compress::Zlib::memGzip($val);
        my $c_len = $c_val.bytes;

        # do we want to keep it?
        if $c_len < $len*(1 - COMPRESS_SAVINGS) {
            $val = $c_val;
            $len = $c_len;
            $flags +|= F_COMPRESS;
        }
    }

    temp %SIG<PIPE> = "IGNORE" unless $FLAG_NOSIGNAL; # XXX %SIG
    my str $line = "$cmdname $.namespace$key $flags $exptime $len\r\n$val\r\n";

    my $res = $self._oneline($sock, $line);

    if $:debug && $line {
        chop $line; chop $line;
        $line.substr(0, -2, ''); # XXX substr
        $*ERR.say "Cache::Memcache: $cmdname $self->{namespace}$key = $val ($line)";
    }

    if $.stat_callback {
        my $etime = Time::HiRes::time();
        $.stat_callback($stime, $etime, $sock, $cmdname);
    }

    return $res eq "STORED\r\n";
}

for <incr decr> -> $meth {
    %::«'$' ~ $meth» := _incrdecr.assuming(:cmdname($meth));
}

method _incrdecr ($self: $cmdname, $key, $value) {
    return undef if ! $:active || $.readonly;
    my $stime = Time::HiRes::time() if $.stat_callback;
    my $sock = $self.get_sock($key);
    return undef unless $sock;
    $key = $key.[1] if ref $key;
    $.stats.{$cmdname}++;
    $value //= 1;

    my $line = "$cmdname $self->{namespace}$key $value\r\n";
    my $res = $self._oneline($sock, $line);

    if $.stat_callback {
        my $etime = Time::HiRes::time();
        $.stat_callback($stime, $etime, $sock, $cmdname);
    }

    return undef unless $res ~~ /^(\d+)/;
    return $1;
}

method get ($self: $key) {
    # TODO: make a fast path for this?  or just keep using get_multi?
    my $r = $self.get_multi($key);
    my $kval = ref $key ? $key.[1] : $key;
    return $r.{$kval};
}

method get_multi ($self: *@keys) {
    return undef unless $:active;
    $:_stime = Time::HiRes::time() if $.stat_callback;
    $.stats.{"get_multi"}++;
    my %val;        # what we'll be returning a reference to (realkey -> value)
    my %sock_keys;  # sockref_as_scalar -> [ realkeys ]
    my $sock;

    for @keys -> $key {
        $sock = $self->get_sock($key);
        next unless $sock;
        my $kval = ref $key ? $key->[1] : $key;
        $sock_keys{$sock}.push($kval);
    }
    $.stats.get_keys  += @keys;
    $.stats.get_socks += keys %sock_keys;

    temp %SIG<PIPE> = "IGNORE" unless $FLAG_NOSIGNAL; # XXX: %SIG

    $self._load_multi(\%sock_keys, \%val);

    if $.debug {
        for %val.kv -> $k, $v {
            $*ERR.say "MemCache: got $k = $v";
        }
    }
    return \%val;
}

method _load_multi ($self: $sock_keys, $ret) {
    # all keyed by a $sock:
    my %reading; # bool, whether we're reading from this socket
    my %writing; # bool, whether we're writing into this socket
    my %state;   # reading state:
                 # 0 = waiting for a line, N = reading N bytes
    my %buf;     # buffers
    my %offset;  # offsets to read into buffers
    my %key;     # current key per socket
    my %flags;   # flags per socket

    for $sock_keys.keys {
        $*ERR.say "processing socket $_" if $.debug >= 2;
        $writing{$_} = 1;
        $buf{$_} = "get ". join(
                " ", map { "$.namespace$_" } $sock_keys.{$_}.elems) . "\r\n";
    }

    my $active_changed = 1; # force rebuilding of select sets

    my $dead = sub ($sock) {
        $*ERR.say "killing socket $sock" if $.debug >= 2;
        delete $reading{$sock};
        delete $writing{$sock};
        delete $ret.{$key{$sock}}
            if $key{$sock};

        if $.stat_callback {
            my $etime = Time::HiRes::time();
            $.stat_callback.($._stime, $etime, $sock, 'get_multi');
        }

        close $sock;
        _dead_sock($sock);
        $active_changed = 1;
    };

    my $finalize = sub ($sock) {
        my $k = $key{$sock};

        # remove trailing \r\n
        $ret.{$k}.substr(0, -2, ''); # XXX substr

        unless $ret.{$k}.bytes == $state{$sock}-2 {
            $dead($sock);
            return undef;
        }

        $ret.{$k} = Compress::Zlib::memGunzip($ret.{$k})
            if $HAVE_ZLIB && $flags{$sock} +& F_COMPRESS;
        if ($flags{$sock} +& F_STORABLE) {
            # wrapped in eval in case a perl 5.6 Storable tries to
            # unthaw data from a perl 5.8 Storable.  (5.6 is stupid
            # and dies if the version number changes at all.  in 5.8
            # they made it only die if it unencounters a new feature)

            # XXX Gaal: this sounds like a good policy anyway, so I'm
            # leaving it in.
            try {
                $ret->{$k} = Storable::thaw($ret->{$k});
                CATCH {
                    # so if there was a problem, just treat it as a cache miss.
                    delete $ret.{$k};
                }
            }
        }
    };

    my $read = sub ($sock) {
        my $res;

        # where are we reading into?
        if $state{$sock} { # reading value into $ret
            $res = sysread($sock, $ret.{$key{$sock}},
                           $state{$sock} - $offset{$sock},
                           $offset{$sock});
            return undef
                if !defined($res) and $!==EWOULDBLOCK;
            if ($res == 0) { # catches 0=conn closed or undef=error
                $dead($sock);
                return undef;
            }
            $offset{$sock} += $res;
            if ($offset{$sock} == $state{$sock}) { # finished reading
                $finalize($sock);
                $state{$sock} = 0; # wait for another VALUE line or END
                $offset{$sock} = 0;
            }
            return undef;
        }

        # we're reading a single line.
        # first, read whatever's there, but be satisfied with 2048 bytes
        $res = sysread($sock, $buf{$sock},
                       2048, $offset{$sock});
        return undef
            if !defined($res) and $!==EWOULDBLOCK;
        if $res == 0 {
            $dead($sock);
            return undef;
        }
        $offset{$sock} += $res;

      SEARCH:
        loop { # may have to search many times
            # do we have a complete END line?
            if ($buf{$sock} ~~ /^END\r\n/) {
                # okay, finished with this socket
                delete $reading{$sock};
                $active_changed = 1;
                return undef;
            }

            # do we have a complete VALUE line?
            if $buf{$sock} ~~ m:w/^VALUE (\S+) (\d+) (\d+)\r\n/ {
                ($key{$sock}, $flags{$sock}, $state{$sock}) =
                    (substr($1, $.namespace_len), int($2), $3+2);
                # Note: we use $+[0] and not pos($buf{$sock}) because pos()
                # seems to have problems under perl's taint mode.  nobody
                # on the list discovered why, but this seems a reasonable
                # work-around:
                #my $p = $+[0];
                # XXX Gaal: I bet p6's tainting will be wriwritten, so I'll try
                # my luck (but leave the comment above in case it fails).
                # XXX: here be substrs and bytes and stuff.
                my $p = $buf{$sock}.pos;
                my $len = $buf{$sock}.bytes;
                my $copy = $len-$p > $state{$sock} ? $state{$sock} : $len-$p;
                $ret->{$key{$sock}} = substr($buf{$sock}, $p, $copy)
                    if $copy;
                $offset{$sock} = $copy;
                substr($buf{$sock}, 0, $p+$copy, ''); # delete the stuff we used
                if ($offset{$sock} == $state{$sock}) { # have it all?
                    $finalize($sock);
                    $state{$sock} = 0; # wait for another VALUE line or END
                    $offset{$sock} = 0;
                    next SEARCH; # look again
                }
                last SEARCH; # buffer is empty now
            }

            # if we're here probably means we only have a partial VALUE
            # or END line in the buffer. Could happen with multi-get,
            # though probably very rarely. Exit the loop and let it read
            # more.

            # but first, make sure subsequent reads don't destroy our
            # partial VALUE/END line.
            $offset{$sock} = $buf{$sock}.bytes;
            last SEARCH;
        }

        # we don't have a complete line, wait and read more when ready
        return undef;
    };

    my $write = sub ($sock) {
        my $res;

        $res = $sock.send($buf{$sock}, $FLAG_NOSIGNAL);
        return undef
            if not defined $res and $!==EWOULDBLOCK; # XXX: $! etc.
        unless $res > 0 {
            $dead($sock);
            return undef;
        }
        if $res == $buf{$sock}.bytes { # all sent
            $buf{$sock} = "";
            $offset{$sock} = $state{$sock} = 0;
            # switch the socket from writing state to reading state
            delete $writing{$sock};
            $reading{$sock} = 1;
            $active_changed = 1;
        } else { # we only succeeded in sending some of it
            # XXX: substr
            substr($buf{$sock}, 0, $res, ''); # delete the part we sent
        }
        return;
    };

    # the bitsets for select
    my ($rin, $rout, $win, $wout);
    my $nfound;

    # the big select loop
    loop {
        if $active_changed {
            last unless %reading or %writing; # no sockets left?
            ($rin, $win) = ('', '');
            for keys %reading -> {
                vec($rin, $_.fileno, 1) = 1;
            }
            foreach (keys %writing) {
                vec($win, $_.fileno, 1) = 1;
            }
            $active_changed = 0;
        }
        # TODO: more intelligent cumulative timeout?
        $nfound = select($rout=$rin, $wout=$win, undef,
                         $.select_timeout);
        last unless $nfound;

        # TODO: possible robustness improvement: we could select
        # writing sockets for reading also, and raise hell if they're
        # ready (input unread from last time, etc.)
        # maybe do that on the first loop only?
        for keys %writing -> {
            if vec($wout, $_.fileno, 1) {
                $write($_);
            }
        }
        for keys %reading -> {
            if vec($rout, $_.fileno, 1) {
                $read->($_);
            }
        }
    }

    # if there're active sockets left, they need to die
    for keys %writing -> {
        $dead($_);
    }
    for keys %reading -> {
        $dead($_);
    }

    return undef;
}

sub _hashfunc ($val) {
    return (crc32($val) >> 16) & 0x7fff;
}

# returns array of lines, or () on failure.
method run_command ($self: $sock, $cmd) {
    return () unless $sock;
    my $ret;
    my $line = $cmd;
    while my $res = $self._oneline($sock, $line) {
        undef $line;
        $ret ~= $res;
        last if $ret ~~ /[END|ERROR]\r\n$/;
    }
    $ret.substr(0, -2, ''); # XXX: substr
    return map { "$_\r\n" } split(/\r\n/, $ret);
}

method stats ($self: $types) {
    return 0 unless $:active;
    return 0 unless !ref($types) || ref($types) eq 'Array';
    if (!ref($types)) {
        if (!$types) {
            # I don't much care what the default is, it should just
            # be something reasonable.  Obviously "reset" should not
            # be on the list :) but other types that might go in here
            # include maps, cachedump, slabs, or items.
            $types = [ <misc malloc sizes self> ];
        } else {
            $types = [ $types ];
        }
    }

    $self.init_buckets() unless $:buckets;

    my $stats_hr = { };

    # The "self" stat type is special, it only applies to this very
    # object.
    if grep { /^self$/ } <== $types.elems {
        $stats_hr<self> = \%r( $.stats );
    }

    # Now handle the other types, passing each type to each host server.
    my @hosts = @($:buckets);
    my %malloc_keys = ( );
  HOST: for @hosts -> $host {
        my $sock = $self.sock_to_host($host);
      TYPE: for grep { !/^self$/ } <== $types.elems -> $typename {
            my $type = $typename eq 'misc' ? "" : " $typename";
            my $line = $self._oneline($sock, "stats$type\r\n");
            if !$line {
                _dead_sock($sock);
                next HOST;
            }

            # Some stats are key-value, some are not.  malloc,
            # sizes, and the empty string are key-value.
            # ("self" was handled separately above.)
            if $typename ~~ /^(malloc|sizes|misc)$/ {
                # This stat is key-value.
              LINE: while $line {
                    # We have to munge this data a little.  First, I'm not
                    # sure why, but 'stats sizes' output begins with NUL.
                    $line ~~ s/^\0//; # XXX: does \0 still mean that?

                    # And, most lines end in \r\n but 'stats maps' (as of
                    # July 2003 at least) ends in \n.  An alternative
                    # would be { local $/="\r\n"; chomp } but this works
                    # just as well:
                    $line ~~ s/<[\r\n]>+$//;

                    # OK, process the data until the end, converting it
                    # into its key-value pairs.
                    last LINE if $line eq 'END';
                    my($key, $value) = $line ~~ /^[STAT<?sp>]?(\w+)\s(.*)/;
                    if $key {
                        $stats_hr<hosts>{$host}{$typename}{$key} = $value;
                    }
                    $malloc_keys{$key} = 1 if $typename eq 'malloc';

                    # read the next line
                    $line = $self._oneline($sock);
                }
            } else {
                # This stat is not key-value so just pull it
                # all out in one blob.
              LINE: while $line {
                    $line ~~ s/<[\r\n]>+$//;
                    last LINE if $line eq 'END';
                    $stats_hr<hosts>{$host}{$typename} ||= "";
                    $stats_hr<hosts>{$host}{$typename} ~= "$line\n";

                    # read the next one
                    $line = $self._oneline($sock);
                }
            }
        }
    }

    # Now get the sum total of applicable values.  First the misc values.
    for <
        bytes bytes_read bytes_written
        cmd_get cmd_set connection_structures curr_items
        get_hits get_misses
        total_connections total_items
        > -> $stat {
        $stats_hr<total>{$stat} = 0;
        for @hosts -> $host {
            $stats_hr<total>{$stat} +=
                $stats_hr<hosts>{$host}<misc>{$stat};
        }
    }

    # Then all the malloc values, if any.
    for keys %malloc_keys -> $malloc_stat {
        $stats_hr.<total>{"malloc_$malloc_stat"} = 0;
        for $host @hosts -> $host {
            $stats_hr<total>{"malloc_$malloc_stat"} +=
                $stats_hr<hosts>{$host}{'malloc'}{$malloc_stat};
        }
    }

    return $stats_hr;
}

sub stats_reset ($self: $types) {
    return 0 unless $:active;

    $self.init_buckets() unless $:buckets;

  HOST: for $self->{'buckets'}.elems -> $host {
        my $sock = $self.sock_to_host($host);
        my $ok = $self->_oneline($sock, "stats reset");
        unless $ok eq "RESET\r\n" {
            _dead_sock($sock);
        }
    }
    return 1;
}



1;
__END__

XXX: these docs still refer to the p5 version!

=head1 NAME

Cache::Memcached - client library for memcached (memory cache daemon)

=head1 SYNOPSIS

  use Cache::Memcached;

  $memd = new Cache::Memcached {
    'servers' => [ "10.0.0.15:11211", "10.0.0.15:11212",
                   "10.0.0.17:11211", [ "10.0.0.17:11211", 3 ] ],
    'debug' => 0,
    'compress_threshold' => 10_000,
  };
  $memd->set_servers($array_ref);
  $memd->set_compress_threshold(10_000);
  $memd->enable_compress(0);

  $memd->set("my_key", "Some value");
  $memd->set("object_key", { 'complex' => [ "object", 2, 4 ]});

  $val = $memd->get("my_key");
  $val = $memd->get("object_key");
  if ($val) { print $val->{'complex'}->[2]; }

  $memd->incr("key");
  $memd->decr("key");
  $memd->incr("key", 2);

=head1 DESCRIPTION

This is the Perl API for memcached, a distributed memory cache daemon.
More information is available at:

  http://www.danga.com/memcached/

=head1 CONSTRUCTOR

=over 4

=item C<new>

Takes one parameter, a hashref of options.  The most important key is
C<servers>, but that can also be set later with the C<set_servers>
method.  The servers must be an arrayref of hosts, each of which is
either a scalar of the form C<10.0.0.10:11211> or an arrayref of the
former and an integer weight value.  (The default weight if
unspecified is 1.)  It's recommended that weight values be kept as low
as possible, as this module currently allocates memory for bucket
distribution proportional to the total host weights.

Use C<compress_threshold> to set a compression threshold, in bytes.
Values larger than this threshold will be compressed by C<set> and
decompressed by C<get>.

Use C<no_rehash> to disable finding a new memcached server when one
goes down.  Your application may or may not need this, depending on
your expirations and key usage.

Use C<readonly> to disable writes to backend memcached servers.  Only
get and get_multi will work.  This is useful in bizarre debug and
profiling cases only.

The other useful key is C<debug>, which when set to true will produce
diagnostics on STDERR.

=back

=head1 METHODS

=over 4

=item C<set_servers>

Sets the server list this module distributes key gets and sets between.
The format is an arrayref of identical form as described in the C<new>
constructor.

=item C<set_debug>

Sets the C<debug> flag.  See C<new> constructor for more information.

=item C<set_readonly>

Sets the C<readonly> flag.  See C<new> constructor for more information.

=item C<set_norehash>

Sets the C<no_rehash> flag.  See C<new> constructor for more information.

=item C<set_compress_threshold>

Sets the compression threshold. See C<new> constructor for more information.

=item C<enable_compress>

Temporarily enable or disable compression.  Has no effect if C<compress_threshold>
isn't set, but has an overriding effect if it is.

=item C<get>

my $val = $memd->get($key);

Retrieves a key from the memcache.  Returns the value (automatically
thawed with Storable, if necessary) or undef.

The $key can optionally be an arrayref, with the first element being the
hash value, if you want to avoid making this module calculate a hash
value.  You may prefer, for example, to keep all of a given user's
objects on the same memcache server, so you could use the user's
unique id as the hash value.

=item C<get_multi>

my $hashref = $memd->get_multi(@keys);

Retrieves multiple keys from the memcache doing just one query.
Returns a hashref of key/value pairs that were available.

This method is recommended over regular 'get' as it lowers the number
of total packets flying around your network, reducing total latency,
since your app doesn't have to wait for each round-trip of 'get'
before sending the next one.

=item C<set>

$memd->set($key, $value[, $exptime]);

Unconditionally sets a key to a given value in the memcache.  Returns true
if it was stored successfully.

The $key can optionally be an arrayref, with the first element being the
hash value, as described above.

The $exptime (expiration time) defaults to "never" if unspecified.  If
you want the key to expire in memcached, pass an integer $exptime.  If
value is less than 60*60*24*30 (30 days), time is assumed to be relative
from the present.  If larger, it's considered an absolute Unix time.

=item C<add>

$memd->add($key, $value[, $exptime]);

Like C<set>, but only stores in memcache if the key doesn't already exist.

=item C<replace>

$memd->replace($key, $value[, $exptime]);

Like C<set>, but only stores in memcache if the key already exists.  The
opposite of C<add>.

=item C<delete>

$memd->delete($key[, $time]);

Deletes a key.  You may optionally provide an integer time value (in seconds) to
tell the memcached server to block new writes to this key for that many seconds.
(Sometimes useful as a hacky means to prevent races.)  Returns true if key
was found and deleted, and false otherwise.

=item C<incr>

$memd->incr($key[, $value]);

Sends a command to the server to atomically increment the value for
$key by $value, or by 1 if $value is undefined.  Returns undef if $key
doesn't exist on server, otherwise it returns the new value after
incrementing.  Value should be zero or greater.  Overflow on server
is not checked.  Be aware of values approaching 2**32.  See decr.

=item C<decr>

$memd->decr($key[, $value]);

Like incr, but decrements.  Unlike incr, underflow is checked and new
values are capped at 0.  If server value is 1, a decrement of 2
returns 0, not -1.

=item C<stats>

$memd->stats([$keys]);

Returns a hashref of statistical data regarding the memcache server(s),
the $memd object, or both.  $keys can be an arrayref of keys wanted, a
single key wanted, or absent (in which case the default value is malloc,
sizes, self, and the empty string).  These keys are the values passed
to the 'stats' command issued to the memcached server(s), except for
'self' which is internal to the $memd object.  Allowed values are:

=over 4

=item C<misc>

The stats returned by a 'stats' command:  pid, uptime, version,
bytes, get_hits, etc.

=item C<malloc>

The stats returned by a 'stats malloc':  total_alloc, arena_size, etc.

=item C<sizes>

The stats returned by a 'stats sizes'.

=item C<self>

The stats for the $memd object itself (a copy of $memd->{'stats'}).

=item C<maps>

The stats returned by a 'stats maps'.

=item C<cachedump>

The stats returned by a 'stats cachedump'.

=item C<slabs>

The stats returned by a 'stats slabs'.

=item C<items>

The stats returned by a 'stats items'.

=back

=item C<disconnect_all>

$memd->disconnect_all();

Closes all cached sockets to all memcached servers.  You must do this
if your program forks and the parent has used this module at all.
Otherwise the children will try to use cached sockets and they'll fight
(as children do) and garble the client/server protocol.

=back

=head1 BUGS

When a server goes down, this module does detect it, and re-hashes the
request to the remaining servers, but the way it does it isn't very
clean.  The result may be that it gives up during its rehashing and
refuses to get/set something it could've, had it been done right.

=head1 COPYRIGHT

This module is Copyright (c) 2003 Brad Fitzpatrick.
All rights reserved.

You may distribute under the terms of either the GNU General Public
License or the Artistic License, as specified in the Perl README file.

=head1 WARRANTY

This is free software. IT COMES WITHOUT WARRANTY OF ANY KIND.

=head1 FAQ

See the memcached website:
   http://www.danga.com/memcached/

=head1 AUTHORS

Brad Fitzpatrick <brad@danga.com>

Anatoly Vorobey <mellon@pobox.com>

Brad Whitaker <whitaker@danga.com>

Jamie McCarthy <jamie@mccarthy.vg>
