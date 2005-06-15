
#
# based on Ruby's ActiveSupport 1.0.4
#

sub bytes {
    $_;
}
our &byte:=&bytes;

sub kilobytes {
    $_ * 1024;
}
our &kilobyte:=&kilobytes;

sub megabytes {
    $_ * 1024.kilobytes;
}
our &megabyte:=&megabytes;

sub gigabytes {
    $_ * 1024.megabytes;
}
our &gigabyte:=&gigabytes;

sub terabytes {
    $_ * 1024.gigabytes;
}
our &terabyte:=&terabytes;
