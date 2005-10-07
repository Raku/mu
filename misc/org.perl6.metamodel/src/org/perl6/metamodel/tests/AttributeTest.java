package org.perl6.metamodel.tests;

import java.util.*;
import junit.framework.*;

import org.perl6.metamodel.*;

public class AttributeTest extends TestCase {

    private MetaClass mc;
    private HashMap props;
    
    public static void main (String[] args) {
        junit.textui.TestRunner.run (suite());
    }

    public static Test suite() {
        return new TestSuite(AttributeTest.class);
    }
    
    protected void setUp() {
        mc = new MetaClass("Foo");
        props = new HashMap();
    }
    
    public void testConstructorDefaultProperties () {
        Attribute a = new Attribute(mc, "$.bar");
        assertEquals(a.associated_with(), mc);
        assertEquals(a.accessor_name(), "bar");
        assertTrue(a.is_ro());
        assertTrue(a.is_public());
        assertNull(a.type());
        assertEquals(a.label(), "$.bar");
    }
    
    public void testConstructorWithProperties () {
        props.put("access", "ro");
        props.put("type", "Baz");
        Attribute a = new Attribute(mc, "$.bar", props);

        assertEquals(a.associated_with(), mc);
        assertTrue(a.is_ro());
        assertTrue(a.is_public());
        assertEquals(a.accessor_name(), "bar");
        assertEquals(a.type(), "Baz");
    }
    
    public void testAttributeVisibility () {    
        Attribute a = new Attribute(mc, "$:bar");
        Attribute b = new Attribute(mc, "$.bar");

        assertTrue(a.is_private());
        assertFalse(a.is_public());
        assertTrue(b.is_public());
        assertFalse(b.is_private());
    }
    
    public void testAttributeAccess () {
        props.put("access", "ro");
        HashMap props2 = new HashMap();
        props2.put("access", "rw");
        Attribute a = new Attribute(mc, "$.bar", props);
        Attribute b = new Attribute(mc, "$.baz", props2);
        
        assertTrue(a.is_ro());
        assertFalse(a.is_rw());
        assertTrue(b.is_rw());
        assertFalse(b.is_ro());
    }
        
    public void testAttributeContainerTypes () {
        Attribute a = new Attribute(mc, "%.bar");
        Attribute b = new Attribute(mc, "@.bar");
        
        assertTrue(a.is_hash());
        assertFalse(a.is_array());
        assertTrue(b.is_array());
        assertFalse(b.is_hash());
    }

    public void testInstantiateContainer () {
        Attribute a = new Attribute(mc, "%.bar");
        Attribute b = new Attribute(mc, "@.bar");
        Attribute c = new Attribute(mc, "$.bar");
        
        assertEquals( a.instantiate_container().getClass().getName(), "java.util.HashMap");
        assertEquals( b.instantiate_container().getClass().getName(), "java.util.ArrayList");
        assertNull(c.instantiate_container());
    }
}