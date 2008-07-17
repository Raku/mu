class p6opaque {
    has $.instance;
    has $.WHENCE;
    has metadata $.metadata;
    has instance_storage $.instance_storage;
}
class metadata {
    has $.how;
    has $.package;
    has $.isa;
    has $.does;
    has %.class_storage;
    has @.attributes;
    has %.methods of Hash;
    has %.submethods of Hash;
}
class instance_storage {
    has %.attrs of Hash;
}
