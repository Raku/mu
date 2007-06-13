podhtm = perl ../../../util/podhtm.pl
pod_files = $(wildcard *.pod)
html_files = $(pod_files:.pod=.html)

all: $(html_files)

# XXX for S26, we need to treat it specifically...
%.html: %.pod ; $(podhtm) -o $@ --title $* --index $<

clean: ; rm -f $(html_files)

