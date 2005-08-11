
if (Perl6 == undefined) var Perl6 = function () {};

Perl6.Attribute = function (associated_with, label, props) {

    var accessor_name = label.substring(2, label.length);
    var visibility = 'public';
    if (label.indexOf(':') == 1) visibility = 'private';
    if (props == undefined) {
        props = { 'access' : 'ro', 'type' : undefined };
    }
    else {
        if (!props['access']) props['access'] = 'ro';
        if (!props['type'])   props['type']   = undefined;        
    }

    this.associated_with = associated_with;
    this.accessor_name = accessor_name;
    this.visibility = visibility;
    this.properties = props;
    this.label = label;
}

Perl6.Attribute.prototype.is_ro = function () {
    return this.properties['access'] == 'ro';
}

Perl6.Attribute.prototype.is_rw = function () {
    return this.properties['access'] == 'rw';    
}

Perl6.Attribute.prototype.type = function () {
    return this.properties['type'];    
}

Perl6.Attribute.prototype.label = function () {
    return this.label;
}

Perl6.Attribute.prototype.is_array = function () {
    return this.label.charAt(0) == '@';
}

Perl6.Attribute.prototype.is_hash = function () {
    return this.label.charAt(0) == '%';    
}

Perl6.Attribute.prototype.associated_with = function () {
    return this.assocaited_with;
}

Perl6.Attribute.prototype.accessor_name = function () {
    return this.accessor_name;    
}

Perl6.Attribute.prototype.is_private = function () {
    return this.visibility == 'private';        
}

Perl6.Attribute.prototype.is_public = function () {
    return this.visibility == 'public';            
}

Perl6.Attribute.prototype.instantiate_container = function () {
    if (this.is_array()) return [];
    if (this.is_hash())  return {};    
    return undefined;
}

/*


*/
