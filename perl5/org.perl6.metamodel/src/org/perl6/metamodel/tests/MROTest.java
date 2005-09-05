
package org.perl6.metamodel.tests;

import java.util.*;
import junit.framework.*;

import org.perl6.metamodel.*;

public class MROTest extends TestCase {

    public static void main (String[] args) {
        junit.textui.TestRunner.run (suite());
    }

    public static Test suite() {
        return new TestSuite(MROTest.class);
    }

    public void testRemoveEmptySeqs () {
        MetaClass m = new MetaClass("Foo");
        
        ArrayList a = new ArrayList();
        a.add(new ArrayList());
        ArrayList a2 = new ArrayList();
        a2.add("Testing");
        a.add(a2);
        a.add(new ArrayList());                
        
        ArrayList non_empty = m._remove_empty_seqs(a);
        assertEquals(non_empty.size(), 1);
        
        assertEquals(((ArrayList) non_empty.get(0)).get(0), "Testing");
    }
    
    public void testInTail () {
        MetaClass m = new MetaClass("Foo");
        MetaClass m2 = new MetaClass("Bar");        
        
        ArrayList a1 = new ArrayList();
        a1.add(m);

        assertFalse(m._in_tail(a1, m));         
        
        ArrayList a2 = new ArrayList();
        a2.add(m2);
        a2.add(m);

        assertTrue(m._in_tail(a2, m));                        
        
        ArrayList a3 = new ArrayList();
        a3.add(m2);
        a3.add(m2);
        a3.add(m2);            
        a3.add(m);

        assertTrue(m._in_tail(a3, m));                                    
    }
    
    public void testRemoveHeadsIf () {
        MetaClass m = new MetaClass("Foo");
        MetaClass m2 = new MetaClass("Bar");        

        ArrayList seq1 = new ArrayList();
        
        ArrayList a1 = new ArrayList();
        a1.add(m);
        a1.add(m2);  
              
        ArrayList a2 = new ArrayList();
        a2.add(m2);
        a2.add(m);                
              
        seq1.add(a1);
        seq1.add(a2);        
        
        assertEquals(seq1.size(), 2);
        assertEquals(((ArrayList) seq1.get(0)).size(), 2);               
        assertEquals(((ArrayList) seq1.get(1)).size(), 2);                       
        
        m._remove_heads_if(m, seq1);         

        assertEquals(seq1.size(), 2);       
        assertEquals(((ArrayList) seq1.get(0)).size(), 1);               
        assertEquals(((ArrayList) seq1.get(1)).size(), 2);                       
    }    
    
    public void testMRO1 () {
        MetaClass a = new MetaClass("A");
        MetaClass b = new MetaClass("B");
        MetaClass c = new MetaClass("C");
        MetaClass d = new MetaClass("D");        

        ArrayList B_super = new ArrayList();
        B_super.add(a);
        b.superclasses(B_super);
        
        ArrayList C_super = new ArrayList();
        C_super.add(a);        
        c.superclasses(C_super);
        
        ArrayList D_super = new ArrayList();
        D_super.add(b);        
        D_super.add(c);                
        d.superclasses(D_super);        

        ArrayList mro = new ArrayList();
        try {
            mro = d.MRO();
        } catch (Exception e) {
            System.out.println("An exception was thrown ");
            e.printStackTrace();
        }
        
        assertEquals(mro.size(), 4);
        assertEquals(((MetaClass) mro.get(0)), d);        
        assertEquals(((MetaClass) mro.get(1)), b);        
        assertEquals(((MetaClass) mro.get(2)), c);        
        assertEquals(((MetaClass) mro.get(3)), a);                                
    } 
    
    public void testMRO2 () {
        MetaClass a = new MetaClass("A");
        MetaClass b = new MetaClass("B");
        MetaClass c = new MetaClass("C");
        MetaClass d = new MetaClass("D"); 
        MetaClass e = new MetaClass("E"); 
        MetaClass f = new MetaClass("F");                        

        ArrayList C_super = new ArrayList();
        C_super.add(d);       
        C_super.add(f);        
        c.superclasses(C_super);

        ArrayList B_super = new ArrayList();
        B_super.add(d);
        B_super.add(e);        
        b.superclasses(B_super);

        ArrayList A_super = new ArrayList();
        A_super.add(b);        
        A_super.add(c);                
        a.superclasses(A_super);        

        ArrayList mro = new ArrayList();
        try {
            mro = a.MRO();
        } catch (Exception x) {
            System.out.println("An exception was thrown ");
            x.printStackTrace();
        }

        assertEquals(mro.size(), 6);
        assertEquals(((MetaClass) mro.get(0)), a);        
        assertEquals(((MetaClass) mro.get(1)), b);        
        assertEquals(((MetaClass) mro.get(2)), c);        
        assertEquals(((MetaClass) mro.get(3)), d);
        assertEquals(((MetaClass) mro.get(4)), e);        
        assertEquals(((MetaClass) mro.get(5)), f);                                        
    }   
    
    public void testMRO3 () {
        MetaClass x = new MetaClass("X");
        MetaClass y = new MetaClass("Y");
        MetaClass xy = new MetaClass("XY");
        MetaClass yx = new MetaClass("YX"); 
        MetaClass z = new MetaClass("Z"); 

        ArrayList XY_super = new ArrayList();
        XY_super.add(x);       
        XY_super.add(y);        
        xy.superclasses(XY_super);

        ArrayList YX_super = new ArrayList();
        YX_super.add(y);       
        YX_super.add(x);        
        yx.superclasses(YX_super);       

        ArrayList Z_super = new ArrayList();
        Z_super.add(xy);       
        Z_super.add(yx);        
        z.superclasses(Z_super);       

        boolean exception_thrown = false;
        try {
            ArrayList mro = z.MRO();
        } catch (Exception e) {
            exception_thrown = true; 
        }                            
        assertTrue(exception_thrown);           
    }    
         
    
    
}