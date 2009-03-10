using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using Tycho.Parser.Tokens;

namespace Tycho.Runtime {
    public abstract class AnyObject : IEnumerable<AnyObject>, IComparable<AnyObject>, IDisposable {
        public abstract AnyObject InvokeMethod (AnyObject self, AnyObject name, params AnyObject [] arguments);

        public virtual IEnumerable<AnyObject> ObjectReferences {
            get { return InvokeMethod (this, Symbols.RuntimeGetProperty, Symbols.RuntimeObjectReferences); }
        }

        #region Runtime Dictionary Methods

        public virtual void Add (AnyObject key, AnyObject value) {
            InvokeMethod (this, Symbols.RuntimeAdd, key, value);
        }

        public virtual bool ContainsKey (AnyObject key) {
            return InvokeMethod (this, Symbols.RuntimeContainsKey, key).ExpectValue<bool> ();
        }

        public virtual bool ContainsValue (AnyObject value) {
            return InvokeMethod (this, Symbols.RuntimeContainsValue, value).ExpectValue<bool> ();
        }

        #endregion

        #region List Methods

        public virtual AnyObject this [AnyObject index] {
            get {
                return InvokeMethod (this, Symbols.RuntimeIndexGet, index);
            }
            set {
                InvokeMethod (this, Symbols.RuntimeIndexSet, index, value);
            }
        }

        public virtual void Add (AnyObject a) {
            InvokeMethod (this, Symbols.RuntimeAdd, a);
        }

        public virtual void Insert (int index, AnyObject item) {
            InvokeMethod (this, Symbols.RuntimeInsert, RuntimeModule.CreateInteger (index), item);
        }

        public virtual bool Remove (AnyObject a) {
            return InvokeMethod (this, Symbols.RuntimeRemove, a).ExpectValue<bool> ();
        }

        public virtual void RemoveIndex (AnyObject index) {
            InvokeMethod (this, Symbols.RuntimeRemoveIndex, index).ExpectValue<bool> ();
        }

        public virtual bool Contains (AnyObject a) {
            return InvokeMethod (this, Symbols.RuntimeContains, a).ExpectValue<bool> ();
        }

        public virtual int Count {
            get { return InvokeMethod (this, Symbols.RuntimeGetProperty, Symbols.RuntimeCount).ExpectValue<int> (); }
        }

        #endregion

        public virtual SourceLocation SourceLocation {
            get {
                throw new TychoException ("this object does not have a source location");
            }
            set {
                throw new TychoException ("this object does not have a source location");
            }
        }

        public virtual bool HasSourceLocation {
            get {
                return false;
            }
        }

        public virtual bool Match (AnyObject results, params AnyObject [] obj) {
            AnyObject [] args = new AnyObject [obj.Length + 1];
            args [0] = results;
            obj.CopyTo (args, 1);
            return InvokeMethod (this, Symbols.RuntimeMatch, args).ExpectValue<bool> ();
        }

        #region Structure Methods

        public bool HasProperty (AnyObject name) {
            return HasProperty (this, name);
        }

        public virtual bool HasProperty (AnyObject self, AnyObject name) {
            return InvokeMethod (self, Symbols.RuntimeHasProperty, name).ExpectValue<bool> ();
        }

        public bool HasMethod (AnyObject name) {
            return HasMethod (this, name);
        }

        public virtual bool HasMethod (AnyObject self, AnyObject name) {
            return InvokeMethod (self, Symbols.RuntimeHasMethod, name).ExpectValue<bool> ();
        }

        public void SetProperty (AnyObject name, AnyObject value) {
            SetProperty (this, name, value);
        }

        public virtual void SetProperty (AnyObject self, AnyObject name, AnyObject value) {
            InvokeMethod (self, Symbols.RuntimeSetProperty, name, value);
        }

        public AnyObject GetProperty (AnyObject name) {
            return GetProperty (this, name);
        }

        public virtual AnyObject GetProperty (AnyObject self, AnyObject name) {
            return InvokeMethod (self, Symbols.RuntimeGetProperty, name);
        }

        public virtual AnyObject Properties {
            get {
                return InvokeMethod(this, Symbols.RuntimeGetProperty, Symbols.RuntimeProperties);
            }
        }

        #endregion

        #region StackFrame Methods

        public virtual void SetVariable (AnyObject self, AnyObject name, int frameIndex, AnyObject value) {
            InvokeMethod (self, Symbols.RuntimeSetVariable, name, RuntimeModule.CreateInteger (frameIndex), value);
        }

        public virtual AnyObject GetVariable (AnyObject self, AnyObject name, int frameIndex) {
            return InvokeMethod (self, Symbols.RuntimeGetVariable, name, RuntimeModule.CreateInteger (frameIndex));
        }

        public virtual AnyObject OuterScope {
            get {
                return GetProperty (Symbols.RuntimeOuterScope);
            }
            set {
                SetProperty (Symbols.RuntimeOuterScope, value);
            }
        }

        public virtual AnyObject Variables {
            get {
                return GetProperty (Symbols.RuntimeVariables);
            }
        }

        #endregion

        #region Prototype Methods

        public virtual void AddPropertySetter (AnyObject name, AnyObject setter, bool overrideMember) {
            InvokeMethod (this, Symbols.RuntimeAddPropertySetter, name, setter, overrideMember);
        }

        public virtual void AddPropertyGetter (AnyObject name, AnyObject getter, bool overrideMember) {
            InvokeMethod (this, Symbols.RuntimeAddPropertyGetter, name, getter, overrideMember);
        }

        public virtual void AddMethod (AnyObject name, AnyObject method) {
            InvokeMethod (this, Symbols.RuntimeAddMethod, name, method);
        }

        public virtual void AddField (AnyObject name, AnyObject value, bool overrideField) {
            InvokeMethod (this, Symbols.RuntimeAddField, name, value, overrideField);
        }

        #endregion

        public virtual AnyObject ParametersSchema {
            get {
                return InvokeMethod (this, Symbols.RuntimeGetProperty, Symbols.RuntimeParametersSchema);
            }
        }

        public virtual AnyObject Invoke (params AnyObject [] arguments) {
            return InvokeMethod (this, Symbols.RuntimeInvoke, arguments);
        }

        public virtual AnyObject Serialize () {
            return new ReferenceObject (this);
        }

        public virtual AnyObject ActuallySerialize () {
            return InvokeMethod (this, Symbols.RuntimeSerialize);
        }

        public virtual AnyObject ProxyTarget {
            get {
                return InvokeMethod (this, Symbols.RuntimeGetProperty, Symbols.RuntimeProxyTarget);
            }
            set {
                InvokeMethod (this, Symbols.RuntimeSetProperty, Symbols.RuntimeProxyTarget, value);
            }
        }

        public virtual bool IsNull { get { return false; } }

        #region Expect

        public T ExpectValue<T> () {
            return this.Expect<ValueObject<T>> ().Value;
        }

        public T Expect<T> () where T : AnyObject {
            T result;
            if (TryCastTo (out result)) {
                return result;
            } else {
                throw new TychoTypeCastException (typeof (T));
            }
        }

        public virtual bool TryCastTo<T> (out T result) where T : AnyObject {
            if (this is T) {
                result = this as T;
                return true;
            } else {
                result = null;
                return false;
            }
        }

        public T ExpectNative<T> () {
            return this.Expect<NativeObject<T>> ().NativeInstance;
        }

        #endregion

        #region Implicit Conversions

        public static implicit operator AnyObject (string s) {
            return RuntimeModule.CreateString (s);
        }

        public static explicit operator string (AnyObject a) {
            return a.ExpectValue<string> ();
        }

        public static implicit operator AnyObject (int i) {
            return RuntimeModule.CreateInteger (i);
        }

        public static explicit operator int (AnyObject a) {
            return a.ExpectValue<int> ();
        }

        public static implicit operator AnyObject (double d) {
            return RuntimeModule.CreateReal (d);
        }

        public static explicit operator double (AnyObject a) {
            return a.ExpectValue<double> ();
        }

        public static implicit operator AnyObject (bool b) {
            return RuntimeModule.CreateBoolean (b);
        }

        public static explicit operator bool (AnyObject a) {
            return a.ExpectValue<bool> ();
        }

        public static implicit operator AnyObject (DateTime d) {
            return RuntimeModule.CreateDateTime (d);
        }

        public static explicit operator DateTime (AnyObject a) {
            return a.ExpectValue<DateTime> ();
        }

        #endregion

        public virtual AnyObject Plus (AnyObject arg) {
            return InvokeMethod (this, Symbols.RuntimePlus, arg);
        }

        public virtual AnyObject Minus (AnyObject arg) {
            return InvokeMethod (this, Symbols.RuntimeMinus, arg);
        }

        public virtual AnyObject Multiply (AnyObject arg) {
            return InvokeMethod (this, Symbols.RuntimeMultiply, arg);
        }

        public virtual AnyObject Divide (AnyObject arg) {
            return InvokeMethod (this, Symbols.RuntimeDivide, arg);
        }

        public virtual AnyObject LessThan (AnyObject arg) {
            return InvokeMethod (this, Symbols.RuntimeLessThan, arg);
        }

        public virtual AnyObject LessThanEqualTo (AnyObject arg) {
            return InvokeMethod (this, Symbols.RuntimeLessThanEqualTo, arg);
        }

        public virtual AnyObject GreaterThan (AnyObject arg) {
            return InvokeMethod (this, Symbols.RuntimeGreaterThan, arg);
        }

        public virtual AnyObject GreaterThanEqualTo (AnyObject arg) {
            return InvokeMethod (this, Symbols.RuntimeGreaterThanEqualTo, arg);
        }

        #region ToString

        public override string ToString () {
            return ToString (new HashSet<AnyObject> ());
        }

        public virtual void Dispose () {
            InvokeMethod (this, Symbols.RuntimeDispose);
        }

        public bool ToStringNoRecurse (HashSet<AnyObject> done) {
            if (done.Contains (this)) {
                return false;
            } else {
                done.Add (this);
                return true;
            }
        }

        public virtual string ToString (HashSet<AnyObject> done) {
            return GetType ().Name;
        }

        #endregion

        #region IEnumerable<AnyObject> Members

        IEnumerator<AnyObject> IEnumerable<AnyObject>.GetEnumerator () {
            return InvokeMethod (this, Symbols.RuntimeGetProperty, Symbols.RuntimeEnumerator).ExpectNative<IEnumerator<AnyObject>> ();
        }

        #endregion

        #region IEnumerable Members

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator () {
            return ((IEnumerable<AnyObject>) this).GetEnumerator ();
        }

        #endregion

        public virtual AnyObject ShallowCopy () {
            throw new NotImplementedException ();
        }

        #region IComparable<AnyObject> Members

        public virtual int CompareTo (AnyObject other) {
            return InvokeMethod (this, Symbols.RuntimeCompareTo, other).ExpectValue<int> ();
        }

        #endregion
    }
}
