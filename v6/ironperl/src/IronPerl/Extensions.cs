
namespace System {

    public delegate TResult Func<T1, T2, T3, T4, T5, TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5);

    public delegate TResult Func<T1, T2, T3, T4, T5, T6, TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6);

    public delegate TResult Func<T1, T2, T3, T4, T5, T6, T7, TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7);
    
}

namespace IronPerl {

    using System;
    using System.Data;
    using System.Collections;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using System.Runtime.CompilerServices;
    using System.Extensions;
    using System.Reflection.Emit;
    using System.IO.Extensions;
    using System.Diagnostics;
    using System.Linq.Expressions;
    using System.Runtime.InteropServices;

    using Rec_int1 = System.Func<System.Func<int, int>, System.Func<int, int>>;

    public static partial class Extensions {



        public static int Bar(this foo f) {
            Console.WriteLine("foofoo" + f.val);
            return f.val;
        }
        public delegate T IO<T>();

        public static IO<R> Bind<T, R>(this IO<T> io, Func<T, IO<R>> func) {
            return func(io());
        }
        public static
            IO<V>
            Bind
            <T, U, V>(
            this IO<T> io,
            Func<T, IO<U>> io0,
            Func<T, U, V> io1) {
            return io.Bind(
                value0 => io0(value0).Bind(
                    value1 => new IO<V>(() => io1(value0, value1))));
        }

        
        public static TValue GetValueOrDefault<TKey, TValue>(this IDictionary<TKey, TValue> dict, TKey key)
        {
            TValue result;
            dict.TryGetValue(key, out result);
            return result;
        }

        public static T ToAnonymousType<T, TValue>(this IDictionary<string, TValue> dict, T anonymousPrototype)
        {
            // get the sole constructor
            var ctor = anonymousPrototype.GetType().GetConstructors().Single();

            // conveniently named constructor parameters make this all possible...
            var args = from p in ctor.GetParameters()
                let val = dict.GetValueOrDefault(p.Name)
                select val != null && p.ParameterType.IsAssignableFrom(val.GetType()) ? (object) val : null;

            return (T) ctor.Invoke(args.ToArray());
        }


        public static T DefaultIfNull<T>([Optional] this T bar) { return (bar.IsNull() ? default(T) : bar); }

        /// <summary>
        /// convert an enumerable to an IEnumerable&lt;T&gt;
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="enumeratorable"></param>
        /// <returns></returns>
        public static
            IEnumerable<T1>
            ToSequence<T2, T1>
            (this T2 enumeratorable)
            where T2 : ICollection, IEnumerable {
            IEnumerator enumr = enumeratorable.GetEnumerator();
            while (enumr.MoveNext()) {
                yield return enumr.Current.CastTo<T1>();
            }
        }


        public static
            T1[]
            ToArray<T2, T1>
            (this T2 enumeratorable)
            where T2 : ICollection, IEnumerable {
            IEnumerator enumr = enumeratorable.GetEnumerator();
            List<T1> arr = new List<T1>(enumeratorable.Count);
            while (enumr.MoveNext()) {
                arr.Add(enumr.Current.CastTo<T1>());
            }
            return arr.ToArray();
        }

        public static
            void
            Test
            (this Func<int, int> func, int range_max) {
            //MethodRental foo = MethodRental.SwapMethodBody(


            func.Test<int>(
                1,
                range_max,
                (i => i + 1),
                (Action<int>)
                    ((int i) => Console.WriteLine(
                        func.Method.ToString() + "(" + i + ") = " + func(i))));
        }

        public static
            void
            Test<T>
            (this Func<T, T> func, T range_min, T range_max, Func<T, T> increment, Action<T> callback)
            where T : IComparable {
            Stopwatch sw = new Stopwatch();
            sw.Start();
            
            range_min.Range(range_max, increment).ForEach(callback);

            sw.Stop();
            Console.WriteLine(range_max + " iterations: " + sw.Elapsed);
        }

        public static
            void
            Test<T>
            (this Action<T> func, T range_min, T range_max, Func<T, T> increment, Action<T> callback)
            where T : IComparable {
            Stopwatch sw = new Stopwatch();
            sw.Start();

            range_min.Range(range_max, increment).ForEach(callback);

            sw.Stop();
            Console.WriteLine(range_max + " iterations: " + sw.Elapsed);
        }
        /*
        public static void Test<T, TIndex>(this Func<T, T> func, T range_min, T range_max, Func<T, T> increment, Action<T, TIndex> callback, Action<TIndex, TIndex> indexing)
            where T : IComparable where TIndex : IComparable, IConvertible<int> {
            Stopwatch sw = new Stopwatch();
            sw.Start();

            range_min.Range(range_max, increment).ForEachIndexed<T, TIndex>(callback, indexing, );

            sw.Stop();
            Console.WriteLine(sw.Elapsed);
        }*/

        /*public static void TestInc<T>(this Func<T, T> func, T range_min, T range_max, Func<T, T> increment) where T : IComparable {
            range_min.Range(range_max, increment).ForEach(i => func.Test(i.UnsafeCast<int>()));
        }*/

        /*public static Func<T, T> YCombinator<T>(this Lambda<T>.hof<Func<T, T>> function) {
            return q => Lambda<T>.ApplyY((Lambda<T>.hof<Func<T, T>>)(function))(q);
        }*/

        /// <summary>
        /// Create a fixed point about F on T
        /// </summary>
        /// <typeparam name="T">Domain and Range of F</typeparam>
        /// <param name="F">Function on T</param>
        /// <returns></returns>
        public static
            Func<T, T>
            Fix<T>
            (this Func<Func<T, T>, Func<T, T>> F) {
            return t => F(Fix<T>(F))(t);
        }

        //SelfApplicable<HigherOrderEvaluator<Func<T, T>>> Y = self => algorithm => t => algorithm(self(self)(algorithm))(t);

        //HigherOrderEvaluator<Func<T, T>> ApplyY = Y(Y);

        public static IEnumerable<T> ForEach<T>(this IEnumerable<T> source, Action<T> action) {
            foreach (T item in source) {
                action(item);
            }
            return source;
        }

        public static IEnumerable<TItem> ForEachIndexed<TItem>(this IEnumerable<TItem> values, Action<TItem> action) {
            return values.ForEachIndexed<TItem, int>((Action<TItem, int>)((TItem item, int i) => action(item)), (i => i + 1), 0);
        }

        public static IEnumerable<TItem> ForEachIndexed<TItem, TIndex>(
            this IEnumerable<TItem> values, Action<TItem, TIndex> action
            , Func<TIndex, TIndex> increment, TIndex index_start) where TIndex : IComparable {
            
            new EachWithIndexIterator<TItem, TIndex>(values, increment, index_start).Do(action);
            return values;
        }

        // from Jimmy Bogard
        public class EachWithIndexIterator<TItem, TIndex> where TIndex : IComparable {
            private readonly IEnumerable<TItem> values;
            private readonly TIndex index_start;
            private readonly Func<TIndex, TIndex> increment;

            internal EachWithIndexIterator(IEnumerable<TItem> values, Func<TIndex, TIndex> increment, TIndex index_start) {
                this.values = values;
                this.index_start = index_start;
                this.increment = increment;
            }

            public void Do(Action<TItem, TIndex> action) {
                TIndex i = index_start;
                foreach (var item in values) {
                    action(item, i);
                    i = this.increment(i);
                }
            }
        }

        public static void NullAction<T>(this T value) { }

        public static T Unit<T>(this T value) {
            return value;
        }

        public static IEnumerable<T> Repeat<T>(this T value) {
            while (true) {
                yield return value;
            }
        }

        public static IEnumerable<T> Repeat<T>(this T value, int count) {
            int i = 0;
            while (true) {
                i++;
                if (i >= count)
                    break;
                yield return value;
            }
        }

        public static void Times<T>(this int count, IEnumerable<T> source) {
            Times(count, source, NullAction);
        }

        public static void Times<T>(this int count, T value) {
            Times(count, Repeat(value));
        }

        /*public static IEnumerable<T> Times<T>(this int count, T value) {
            return Repeat(value, count);
        }*/

        public static void Times<T>(this int count, IEnumerable<T> source, Action<T> action) {
            int i = 0;
            foreach (T item in source) {
                action(item);
                i++;
                if (i == count)
                    break;
            }
        }

        public static IEnumerable<T> Pipe<T>(this IEnumerable<T> source, Action<T> action) {
            foreach (T item in source) {
                action(item);
                yield return item;
            }
        }

        public static IEnumerable<R> Pipe<T, R>(this IEnumerable<T> source, Func<T, R> transform) {
            foreach (T item in source) {
                yield return transform(item);
            }
        }

        public static IEnumerable<T> Filter<T>(this IEnumerable<T> source, Predicate<T> filter) {
            foreach (T item in source) {
                if (filter(item))
                    yield return item;
            }
        }

        public static string Join<T>(this IEnumerable<T> source, object joiner) {
            StringBuilder sb = new StringBuilder();
            bool gotfirst = false;
            foreach (T item in source) {
                if (gotfirst) sb.Append(joiner);
                gotfirst = true;
                sb.Append(item);
            }
            return sb.ToString();
        }

        private static string Join(this IEnumerable<string> strs) {
            return string.Join(" ", strs.ToArray());
        }


        public static T Get1<T>(this IEnumerable<T> source) {
            foreach (T item in source) {
                return item;
            }
            return default(T);
        }


        /// <summary>
        /// perform an action 
        /// </summary>
        /// <typeparam name="TReturn"></typeparam>
        /// <param name="method"></param>
        /// <param name="result"></param>
        /// <returns></returns>
        public static
            TReturn
            DoReturn<TReturn>
            (this Action<TReturn> method, out TReturn result) {
            result = default(TReturn);
            method.DynamicInvoke(result);
            return result;
        }

        /// <summary>
        /// memoize a 2-arity non-recursive function
        /// UNTESTED
        /// </summary>
        /// <typeparam name="TArg1"></typeparam>
        /// <typeparam name="TArg2"></typeparam>
        /// <typeparam name="TResult"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static
            Func<TArg1, TArg2, TResult>
            Memoize<TArg1, TArg2, TResult>
            (Func<TArg1, TArg2, TResult> func) {
            
            var cache = new Dictionary<Tuple<TArg1, TArg2>, TResult>();

            return (TArg1 argument1, TArg2 argument2) =>
            {
                TResult result;
                var arguments = Tuple.From(argument1, argument2);
                return (cache.TryGetValue(arguments, out result)
                    ? result
                    : ((Action<TResult>)((TResult res) => cache.Add(arguments, res))).DoReturn(out result));
            };
        }


        /// <summary>
        ///     memoize a non-recursive lambda expression
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <typeparam name="R"></typeparam>
        /// <param name="f"></param>
        /// <returns></returns>
        public static Func<T, R> Memoize<T, R>(this Func<T, R> f) {
            Dictionary<T, R> cache = new Dictionary<T, R>();
            return (T t) =>
            {
                R result;
                return (cache.TryGetValue(t, out result) ? result : (cache[t] = f(t)));
            };
        }


        // from http://www.ridgesolutions.ie/index.php/2008/08/12/implicit-cast-of-lambda-expression-to-custom-class-in-c-30/

        /// <summary>
        /// encapsulates a 0-arity delegate Func&lt;T&gt;
        /// </summary>
        /// <typeparam name="T"></typeparam>
        public class RuleProp<T> {
            private Func<T> _lambda = null;

            public RuleProp(Func<T> l) {
                _lambda = l;
            }

            /// <summary>
            /// 
            /// </summary>
            /// <param name="l"></param>
            /// <returns></returns>
            public static implicit operator RuleProp<T>(Func<T> l) {
                return new RuleProp<T>(l);
            }

            public RuleProp() {
            }
        }




        // from http://www.ridgesolutions.ie/index.php/2008/08/12/implicit-cast-of-lambda-expression-to-custom-class-in-c-30/
        /// <summary>
        /// encourages a cast from Lambda -&gt; Func&lt;T&gt
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="f"></param>
        /// <returns></returns>
        public static
            Func<T>
            Func0<T>
            (this object obj, Func<T> f) {
            return f;
        }

        public static
            Func<TArg1, TResult>
            Func1<TArg1, TResult>
            (this object obj, Func<TArg1, TResult> f) {
            return f;
        }

        public static
            Func<TArg1, TArg2, TResult>
            Func2<TArg1, TArg2, TResult>
            (this object obj, Func<TArg1, TArg2, TResult> f) {
            return f;
        }

        public static
            Func<TArg1, TArg2, TArg3, TResult>
            Func3<TArg1, TArg2, TArg3, TResult>
            (this object obj, Func<TArg1, TArg2, TArg3, TResult> f) {
            return f;
        }

        public static
            Func<TArg1, TArg2, TArg3, TArg4, TResult>
            Func4<TArg1, TArg2, TArg3, TArg4, TResult>
            (this object obj, Func<TArg1, TArg2, TArg3, TArg4, TResult> f) {
            return f;
        }

        public static
            Func<TArg1, TArg2, TArg3, TArg4, TArg5, TResult>
            Func5<TArg1, TArg2, TArg3, TArg4, TArg5, TResult>
            (this object obj, Func<TArg1, TArg2, TArg3, TArg4, TArg5, TResult> f) {
            return f;
        }

        public static
            Func<TArg1, TArg2, TArg3, TArg4, TArg5, TArg6, TResult>
            Func6<TArg1, TArg2, TArg3, TArg4, TArg5, TArg6, TResult>
            (this object obj, Func<TArg1, TArg2, TArg3, TArg4, TArg5, TArg6, TResult> f) {
            return f;
        }

        public static
            Func<TArg1, TArg2, TArg3, TArg4, TArg5, TArg6, TArg7, TResult>
            Func7<TArg1, TArg2, TArg3, TArg4, TArg5, TArg6, TArg7, TResult>
            (this object obj, Func<TArg1, TArg2, TArg3, TArg4, TArg5, TArg6, TArg7, TResult> f) {
            return f;
        }




        /// <summary>
        /// Wrap a literal (value type, hopefully?) in a lambda.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="val"></param>
        /// <returns></returns>
        public static
            Func<T>
            ToLambda<T>
            (this T val) {
            return (Func<T>)Expression.Lambda<Func<T>>(Expression.Constant(val), null).Compile();
        }

        // from iangblog
        /// <summary>
        /// 
        /// </summary>
        /// <typeparam name="TFuncDomain"></typeparam>
        /// <typeparam name="TFuncReturn"></typeparam>
        /// <param name="maybeT"></param>
        /// <param name="s_expr"></param>
        /// <returns></returns>
        public static
            TFuncReturn
            Select<TFuncDomain, TFuncReturn>
            (this TFuncDomain maybeT, Func<TFuncDomain, TFuncReturn> s_expr) {
            return (maybeT == null)
                ? default(TFuncReturn)
                : s_expr(maybeT);
        }

        public static V SelectMany<T, U, V>(this T maybeT,
                                Func<T, U> k, Func<T, U, V> s) {
            if (maybeT == null) { return default(V); }

            U maybeBoundResult = k(maybeT);
            if (maybeBoundResult == null) { return default(V); }
            return s(maybeT, maybeBoundResult);
        }




        // from wesdyer
        /// <summary>
        ///     Fix a non-recursive function about a fixed point
        /// </summary>
        /// <typeparam name="A"></typeparam>
        /// <typeparam name="R"></typeparam>
        /// <param name="f"></param>
        /// <returns></returns>
        public static
            Func<A, R>
            Fix<A, R>
            (this Func<Func<A, R>, Func<A, R>> f) {
            Func<A, R> g = null;
            g = f(a => g(a));
            return g;
        }
        
        
        /// <summary>
        ///     memoize a 1-arity recursive lambda expression
        ///     e.g.   fib => n => n &lt;= 2 ? 1 : fib(n - 1) + fib(n - 2)
        /// </summary>
        /// <typeparam name="A"></typeparam>
        /// <typeparam name="R"></typeparam>
        /// <param name="f"></param>
        /// <returns></returns>
        public static
            Func<A, R>
            MemoizeFix<A, R>
            (this Func<Func<A, R>, Func<A, R>> f) {
            Func<A, R> g = null;
            g = f(a => g(a));
            g = g.Memoize();
            return g;
        }

        /// <summary>
        ///     generate an enumerable sequence from the first point to the other
        ///     using the mutator lambda.
        /// </summary>
        /// <typeparam name="T">type of sequence item(s)</typeparam>
        /// <param name="from">startpoint</param>
        /// <param name="to">endpoint</param>
        /// <param name="mutator_lambda">sequence mutator lambda</param>
        /// <returns>sequence of T from startpoint to endpoint</returns>
        public static
            IEnumerable<T>
            Range<T>
            (this T from, T to, Func<T, T> mutator_lambda)
            where T : IComparable {
            for (T t = from; t.CompareTo(to) <= 0; t = mutator_lambda(t)) yield return t;
        }

        // Lambda expressions can't slurp their args 
        /*public static Func<object[], TResult> LambdaParams(this Func<object[], TResult> func, params object[] args) {
            return args => { 
        }*/

        public delegate object iv(object target, params object[] arguments);



    }
}
