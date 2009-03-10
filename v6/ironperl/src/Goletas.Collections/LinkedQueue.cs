//
// Copyright © 2005 - 2007 Maksim Goleta. All rights reserved.
// GOLETAS PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
//

namespace Goletas.Collections
{
	using System;

    /// <summary>
    /// Represents a variable size first-in-first-out [FIFO] collection
    /// of objects of the same arbitrary type.
    /// </summary>
    /// <remarks>
    /// <para>
    /// <see cref="LinkedQueue&lt;T&gt;"/> is implemented as a singly linked list.
    /// </para>
	/// <para>
	/// <see cref="LinkedQueue&lt;T&gt;"/> provides guaranteed O(1) time cost
	/// for the <see cref="Enqueue"/> and <see cref="Dequeue"/> operations.
	/// </para>
    /// <para>
    /// <see cref="LinkedQueue&lt;T&gt;"/> accepts <c>null</c> as a valid
	/// value for reference types and allows duplicate elements.
    /// </para>
    /// </remarks>
	/// <typeparam name="T">
	/// The element type of the <see cref="LinkedQueue&lt;T&gt;"/>.
	/// </typeparam>
    public sealed class LinkedQueue<T> : System.Collections.Generic.IEnumerable<T>
    {
		/// <summary>
		/// Represents a node in the <see cref="LinkedQueue&lt;T&gt;"/>.
		/// </summary>
		/// <remarks>
		/// The <see cref="Node"/> contains a value and a reference to the next node.
		/// </remarks>
		internal sealed class Node
		{
			/// <summary>
			/// The object contained in this node.
			/// </summary>
			public T Item;

			/// <summary>
			/// The reference to the next node in the <see cref="LinkedQueue&lt;T&gt;"/> or <c>null</c>
			/// if this <see cref="Node"/> is the last node in the <see cref="LinkedQueue&lt;T&gt;"/>.
			/// </summary>
			public Node Next;

			/// <summary>
			/// Initializes a new instance of the <see cref="Node"/>
			/// class containing the specified object.
			/// </summary>
			/// <param name="item">
			/// The object to contain in the <see cref="Node"/>.
			/// </param>
			/// <remarks>
			/// The <see cref="Next"/> field is initialized to <c>null</c>.
			/// </remarks>
			public Node(T item)
			{
				this.Item = item;
			}
		}

		/// <summary>
		/// Enumerates the elements of the <see cref="LinkedQueue&lt;T&gt;"/>.
		/// </summary>
		/// <remarks>
		/// <para>
		/// The elements are enumerated in first-in-first-out [FIFO] order, similar to the order
		/// of the elements returned by a succession of calls to the <see cref="Dequeue()"/> method.
		/// </para>
		/// <para>
		/// Initially, the enumerator is positioned before the first element in the collection.
		/// At this position, <see cref="Current"/> is undefined. Therefore, <see cref="MoveNext"/>
		/// must be called to advance the enumerator to the first element of the collection before
		/// reading the value of <see cref="Current"/>.
		/// </para>
		/// <para>
		/// <see cref="Current"/> returns the same object until <see cref="MoveNext"/> is
		/// called. <see cref="MoveNext"/> sets <see cref="Current"/> to the next element.
		/// </para>
		/// <para>
		/// If <see cref="MoveNext"/> passes the end of the collection, the enumerator is
		/// positioned after the last element in the collection and <see cref="MoveNext"/>
		/// returns <c>false</c>. When the enumerator is at this position, subsequent calls to
		/// <see cref="MoveNext"/> also return <c>false</c>. If the last call to
		/// <see cref="MoveNext"/> returned <c>false</c>, <see cref="Current"/> is undefined.
		/// <see cref="Current"/> cannot be set to the first element of the collection again.
		/// A new enumerator instance must be created instead.
		/// </para>
		/// <para>
		/// An enumerator remains valid as long as the collection remains unchanged. If changes
		/// are made to the collection, such as adding, modifying, or deleting elements, the
		/// enumerator behavior is undefined.
		/// </para>
		/// <para>
		/// The enumerator does not have exclusive access to the collection; therefore,
		/// enumerating through a collection is intrinsically not a thread-safe procedure.
		/// To guarantee thread safety during enumeration, the collection can be locked
		/// during the entire enumeration. To allow the collection to be accessed by
		/// multiple threads for reading and writing, a custom synchronization must be
		/// implemented.
		/// </para>
		/// <para>
		/// The <see cref="Enumerator"/> is not designed to provide
		/// any fast-fail safety mechanisms against concurrent modifications.
		/// </para>
		/// </remarks>
        public struct Enumerator : System.Collections.Generic.IEnumerator<T>
		{
			/// <summary>
			/// The <see cref="Node"/> at the current position of the enumerator.
			/// </summary>
			private Node _Next;

			/// <summary>
			/// The element at the current position of the enumerator.
			/// </summary>
			private T _Current;

			/// <summary>
			/// Gets the element in the <see cref="LinkedQueue&lt;T&gt;"/> at
			/// the current position of the enumerator. 
			/// </summary>
			/// <value>
			/// The element in the <see cref="LinkedQueue&lt;T&gt;"/>
			/// at the current position of the enumerator.
			/// </value>
			/// <remarks>
			/// <para>
			/// <see cref="Current"/> is undefined under any of the following conditions:
			/// 1) The enumerator is positioned before the first element in the collection,
			/// immediately after the enumerator is created. <see cref="MoveNext"/> must be
			/// called to advance the enumerator to the first element of the collection
			/// before reading the value of <see cref="Current"/>; 2) The last call to
			/// <see cref="MoveNext"/> returned <c>false</c>, which indicates the end of
			/// the collection; 3) The collection was modified after the enumerator was created.
			/// </para>
			/// <para>
			/// <see cref="Current"/> returns the same object until <see cref="MoveNext"/>
			/// is called. <see cref="MoveNext"/> sets <see cref="Current"/> to the next element.
			/// </para>
			/// </remarks>
			public T Current
			{
				get
				{
					return this._Current;
				}
			}

            /// <summary>
            /// Gets the element in the <see cref="LinkedQueue&lt;T&gt;"/> at
            /// the current position of the enumerator. 
            /// </summary>
            /// <value>
            /// The element in the <see cref="LinkedQueue&lt;T&gt;"/>
            /// at the current position of the enumerator.
            /// </value>
            /// <remarks>
            /// <para>
            /// <see cref="Current"/> is undefined under any of the following conditions:
            /// 1) The enumerator is positioned before the first element in the collection,
            /// immediately after the enumerator is created. <see cref="MoveNext"/> must be
            /// called to advance the enumerator to the first element of the collection
            /// before reading the value of <see cref="Current"/>; 2) The last call to
            /// <see cref="MoveNext"/> returned <c>false</c>, which indicates the end of
            /// the collection; 3) The collection was modified after the enumerator was created.
            /// </para>
            /// <para>
            /// <see cref="Current"/> returns the same object until <see cref="MoveNext"/>
            /// is called. <see cref="MoveNext"/> sets <see cref="Current"/> to the next element.
            /// </para>
            /// </remarks>
            object System.Collections.IEnumerator.Current
            {
                get
                {
                    return this._Current;
                }
            }

			/// <summary>
			/// Advances the enumerator to the next element of the <see cref="LinkedQueue&lt;T&gt;"/>.
			/// </summary>
			/// <returns>
			/// <c>true</c> if the enumerator was successfully advanced to the next element;
			/// <c>false</c> if the enumerator has passed the end of the collection.
			/// </returns>
			/// <remarks>
			/// <para>
			/// After an enumerator is created, the enumerator is positioned before the first element
			/// in the collection, and the first call to <see cref="MoveNext"/> advances the
			/// enumerator to the first element of the collection.
			/// </para>
			/// <para>
			/// If <see cref="MoveNext"/> passes the end of the collection, the enumerator is positioned
			/// after the last element in the collection and <see cref="MoveNext"/> returns <c>false</c>.
			/// When the enumerator is at this position, subsequent calls to <see cref="MoveNext"/>
			/// also return <c>false</c>.
			/// </para> 
			/// </remarks>
            public bool MoveNext()
            {
                if (this._Next == null)
                {
                    return false;
                }

                this._Current = this._Next.Item;
                this._Next = this._Next.Next;

                return true;
            }

            /// <summary>
            /// Sets the enumerator to its initial position,
            /// which is before the first element in the <see cref="LinkedQueue&lt;T&gt;"/>.
            /// This method always throws a <see cref="NotSupportedException"/>.
            /// </summary>
            /// <exception cref="NotSupportedException">
            /// Always thrown since this operation is not supported.
            /// </exception>
            void System.Collections.IEnumerator.Reset()
            {
                throw new NotSupportedException();
            }

            /// <summary>
            /// Releases all resources allocated by the <see cref="Enumerator"/>.
            /// </summary>
            void IDisposable.Dispose()
            {

            }

			/// <summary>
			/// Initializes a new instance of the <see cref="Enumerator"/> structure with
			/// the specified <paramref name="node"/>.
			/// </summary>
			/// <param name="node">
			/// The node from which to start enumerating the
			/// <see cref="LinkedQueue&lt;T&gt;"/> elements.
			/// </param>
            internal Enumerator(Node node)
            {
                this._Next = node;
                this._Current = default(T);
            }
        }

		/// <summary>
		/// The first none in the <see cref="LinkedQueue&lt;T&gt;"/>.
		/// If this field is <c>null</c>, then the queue is empty.
		/// </summary>
		private Node _First;

		/// <summary>
		/// The last node in the <see cref="LinkedQueue&lt;T&gt;"/>
		/// If this field is <c>null</c>, then the queue is empty.
		/// </summary>
		private Node _Last;

		/// <summary>
		/// The number of elements contained in the <see cref="LinkedQueue&lt;T&gt;"/>.
		/// </summary>
        private int _Count;

		/// <summary>
		/// Gets the number of elements contained in the <see cref="LinkedQueue&lt;T&gt;"/>.
		/// </summary>
		/// <value>
		/// The number of elements contained in the <see cref="LinkedQueue&lt;T&gt;"/>.
		/// </value>
		/// <remarks>
		/// Retrieving the value of this property is an O(1) operation.
		/// </remarks>
        public int Count
        {
            get
            {
                return this._Count;
            }
        }

        /// <summary>
        /// Adds an object to the end of the <see cref="LinkedQueue&lt;T&gt;"/>. 
        /// </summary>
        /// <param name="item">
        /// The object to add to the <see cref="LinkedQueue&lt;T&gt;"/>.
		/// The value can be <c>null</c> for reference types.
        /// </param>
		/// <remarks>
		/// This method is an O(1) operation.
		/// </remarks>
        public void Enqueue(T item)
        {
            if (this._First != null)
			{
				this._Last.Next = new Node(item);
				this._Last = this._Last.Next;
			}
			else
			{
				this._First = new Node(item);
				this._Last = this._First;
			}

			this._Count++;
        }

		/// <summary>
		/// Determines whether an element is in the <see cref="LinkedQueue&lt;T&gt;"/>.
		/// </summary>
		/// <param name="item">
		/// The object to locate in the <see cref="LinkedQueue&lt;T&gt;"/>.
		/// The value can be <c>null</c> for reference types.
		/// </param>
		/// <returns>
		/// <c>true</c> if the <paramref name="item"/> is found in the
		/// <see cref="LinkedQueue&lt;T&gt;"/>; otherwise, <c>false</c>.
		/// </returns>
		/// <remarks>
		/// <para>
		/// This method determines equality by calling <see cref="Object.Equals(object)"/>.
		/// To enhance performance, it is recommended that in addition to implementing
		/// <see cref="Object.Equals(object)"/>, any class/struct also implement
		/// <see cref="IEquatable&lt;T&gt;"/> interface for their own type.
		/// </para>
		/// <para>
		/// This method is an O(n) operation, where n is <see cref="Count"/>.
		/// </para>
		/// </remarks>
		public bool Contains(T item)
		{
			Node p = this._First;

			if (item != null)
			{
				while (p != null)
				{
					if (item.Equals(p.Item))
					{
						return true;
					}

					p = p.Next;
				}
			}
			else
			{
				while (p != null)
				{
					if (p.Item == null)
					{
						return true;
					}

					p = p.Next;
				}
			}

			return false;
		}

        /// <summary>
        /// Returns the object at the beginning of the <see cref="LinkedQueue&lt;T&gt;"/>
        /// without removing it.
        /// </summary>
        /// <returns>
        /// The object at the beginning of the <see cref="LinkedQueue&lt;T&gt;"/>.
        /// </returns>
        /// <remarks>
        /// <para>
        /// This method is similar to the <see cref="Dequeue()"/> method, but <see cref="Peek"/>
		/// does not modify the <see cref="LinkedQueue&lt;T&gt;"/>.
        /// </para>
        /// <para>
        /// This method is an O(1) operation.
        /// </para>
        /// </remarks>
		/// <exception cref="InvalidOperationException">
		/// The <see cref="LinkedQueue&lt;T&gt;"/> is empty.
		/// </exception>
        public T Peek()
        {
			if (this._First == null)
			{
				throw new InvalidOperationException();
			}

			return this._First.Item;
        }

        /// <summary>
        /// Removes and returns the object at the beginning of the <see cref="LinkedQueue&lt;T&gt;"/>. 
        /// </summary>
        /// <returns>
        /// The object that is removed from the beginning of the <see cref="LinkedQueue&lt;T&gt;"/>.
        /// </returns>
        /// <remarks>
        /// <para>
        /// This method is similar to the <see cref="Peek()"/> method, but <see cref="Peek()"/>
		/// does not modify the <see cref="LinkedQueue&lt;T&gt;"/>.
        /// </para>
        /// <para>
        /// This method is an O(1) operation.
        /// </para>
        /// </remarks>
		/// <exception cref="InvalidOperationException">
		/// The <see cref="LinkedQueue&lt;T&gt;"/> is empty.
		/// </exception>
        public T Dequeue()
        {
			if (this._First == null)
			{
				throw new InvalidOperationException();
			}

			T item = this._First.Item;

			this._First = this._First.Next;

			if (this._First == null)
			{
				this._Last = null;
			}

			this._Count--;

			return item;
        }

        /// <summary>
        /// Removes all objects from the <see cref="LinkedQueue&lt;T&gt;"/>. 
        /// </summary>
		/// <remarks>
		/// <para>
		/// <see cref="Count"/> is set to zero and references to other objects from
		/// elements of the collection are also released.
		/// </para>
		/// <para>
		/// This method is an O(1) operation.
		/// </para>
		/// </remarks>
        public void Clear()
        {
            this._First = null;
            this._Last = null;
            this._Count = 0;
        }

		/// <summary>
		/// Copies the <see cref="LinkedQueue&lt;T&gt;"/> elements to an existing
		/// one-dimensional <see cref="Array"/>, starting at the specified
		/// array index.
		/// </summary>
		/// <param name="array">
		/// The one-dimensional <see cref="Array"/> that is the destination of the elements
		/// copied from this <see cref="LinkedQueue&lt;T&gt;"/>. The <paramref name="array"/>
		/// must have zero-based indexing.
		/// </param>
		/// <param name="index">
		/// The zero-based index in the <paramref name="array"/> at which copying begins.
		/// </param>
		/// <remarks>
		/// <para>
		/// The elements are copied to the <paramref name="array"/> in first-in-first-out
		/// [FIFO] order, similar to the order of the elements returned by a succession of
		/// calls to the <see cref="Dequeue()"/>
		/// method.
		/// </para>
		/// <para>
		/// This method is an O(n) operation, where n is <see cref="Count"/>.
		/// </para>
		/// </remarks>
		/// <exception cref="ArgumentNullException">
		/// <paramref name="array"/> is a <c>null</c> reference.
		/// </exception>
		/// <exception cref="ArgumentOutOfRangeException">
		/// <paramref name="index"/> is outside of the <paramref name="array"/> bounds.
		/// </exception>
		/// <exception cref="ArgumentException">
		/// The number of elements in the source <see cref="LinkedQueue&lt;T&gt;"/>
		/// is greater than the available space from the <paramref name="index"/> to the end
		/// of the destination <paramref name="array"/>.
		/// </exception>
		public void CopyTo(T[] array, int index)
		{
			if (array == null)
			{
				throw new ArgumentNullException();
			}

			if ((index < 0) || (index >= array.Length))
			{
				throw new ArgumentOutOfRangeException();
			}

			if ((array.Length - index) < this._Count)
			{
				throw new ArgumentException();
			}

            Node p = this._First;

            while (p != null)
            {
                array[index] = p.Item;

                p = p.Next;
                index++;
            }
		}
		
		/// <summary>
		/// Returns an enumerator that iterates through the <see cref="LinkedQueue&lt;T&gt;"/>.
		/// </summary>
		/// <returns>
		/// An <see cref="Enumerator"/> for the <see cref="LinkedQueue&lt;T&gt;"/>.
		/// </returns>
		/// <remarks>
		/// This method is an O(1) operation.
		/// </remarks>
		public Enumerator GetEnumerator()
		{
			return new Enumerator(this._First);
		}

        /// <summary>
        /// Returns an enumerator that iterates through the <see cref="LinkedQueue&lt;T&gt;"/>.
        /// </summary>
        /// <returns>
        /// An <see cref="Enumerator"/> for the <see cref="LinkedQueue&lt;T&gt;"/>.
        /// </returns>
        /// <remarks>
        /// This method is an O(1) operation.
        /// </remarks>
        System.Collections.Generic.IEnumerator<T> System.Collections.Generic.IEnumerable<T>.GetEnumerator()
        {
            return this.GetEnumerator();
        }

        /// <summary>
        /// Returns an enumerator that iterates through the <see cref="LinkedQueue&lt;T&gt;"/>.
        /// </summary>
        /// <returns>
        /// An <see cref="Enumerator"/> for the <see cref="LinkedQueue&lt;T&gt;"/>.
        /// </returns>
        /// <remarks>
        /// This method is an O(1) operation.
        /// </remarks>
        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="LinkedQueue&lt;T&gt;"/> class.
        /// </summary>
		/// <remarks>
		/// This constructor is an O(1) operation.
		/// </remarks>
        public LinkedQueue()
        {

        }

    }
}