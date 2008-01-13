module KindaPerl6::Runtime::Perl5V6::MOP {
    use Class::MOP;
    ::Class::MOP::Attribute := 'Class::MOP::Attribute';
    ::Class::MOP::Class := 'Class::MOP::Class';
    my $Class_cache = Class::MOP::Class.create_anon_class();
    $Class_cache.add_method("add_method",sub ($self,$name,$code) {
    #    say "add_method $name";
        ($self.cache).add_method($name,$code);
    });
    $Class_cache.add_attribute(Class::MOP::Attribute.new('cache','accessor','cache'));
    my $Class_metaobject = $Class_cache.new_object();
    $Class_metaobject.cache($Class_cache);
    $Class_metaobject.add_method('PROTOTYPE',sub ($self) {
    #    say "PROTOTYPE";
        my $proto = ($self.cache).new_object;
        return $proto;
    });
    $Class_metaobject.add_method('HOW',sub ($self) {
        return $Class_metaobject;
    });
    ::Class := $Class_metaobject;
    (::Class.HOW).add_method('add_attribute',sub ($self,$attr_name) {
        (self.cache).add_attribute(Class::MOP::Attribute.new($attr_name,'accessor',$attr_name));
    });
    (::Class.HOW).add_method('new',sub ($self) {
        my $new = ($self.cache).new_object();
        $new.cache(Class::MOP::Class.create_anon_class());
        $new.add_method('HOW',sub {$new});
        $new.add_method('new',sub {$new.new_object()});
        $new;
    });
    (::Class.HOW).add_method('new_object',sub ($self) {
        my $new = ($self.cache).new_object;
        return $new;
    });
}
 
