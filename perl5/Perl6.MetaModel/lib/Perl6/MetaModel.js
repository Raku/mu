
if (Perl6 == undefined) var Perl6 = {};

Perl6.MetaModel = function () {};

function WALKMETH (dispatcher, label, options) {
    var current;
    while (current = dispatcher.next()) {
        if current.has_method(label) last;
    }
    if (current == undefined) return undefined;
    return current.get_method(label, options);
}

function WALKCLASS (dispacther, options) {
    return dispatcher.next();
}