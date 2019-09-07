<Query Kind="Program">
  <Output>DataGrids</Output>
  <Namespace>System.Security.Cryptography</Namespace>
  <Namespace>System.Threading.Tasks</Namespace>
</Query>

static class TaskMonad
{
	public static Task<T> Wrap<T>(this T x) => Task.FromResult(x);

	/// <summary>
	/// Left Identity:		Wrap(x).Bind(f) == f(x)
	/// Right Identity: 	m.Bind(Wrap) == m
	/// Associative/Chain:	m.Bind(g ° f) == m.Bind(f).Bind(g)
	/// 
	/// To show that the Bind function obeys the 3 rule, let's rewrite the Bind function
	/// as follows:
	///		public static async Task<R> Bind<T, R>(this Task<T> m, Func<T, Task<R>> f)
	///			=> await f(await m);
	///
	/// We have changed nothing by the rewrite.
	/// Now, let's apply the Bind function for all the three rules:
	///
	/// Left Identity: Wrap(x).Bind(f) =
	///		Plug it into the Bind function.
	///
	///		Point:  	await, is the opposite of Wrap(x).  await Wrap(x) is equal to:
	///		Wrap(x).Result which is x.  So: await Wrap(x) = x
	///
	///		Therefore:  
	///			Wrap(x).Bind(f) =
	///			(await f(await Wrap(x))).Wrap() =
	///			(await f(x)).Wrap() =
	///			f(x)
	///		Therefore:
	///			Wrap(x).Bind(f) = f(x)
	///
	/// Right Identity: 		m.Bind(Wrap) =
	///		// Plug it into the Bind function: m.Bind(f) => await f(await m)
	///			(await Wrap(await m)).Wrap() =
	///			(await m).Wrap() =
	///			m
	///		Therefore:
	///			m.Bind(Wrap) is m
	///
	/// Associative/Chain:	m.Bind(f).Bind(g) == m.Bind(g ° f)
	///		m.Bind(f).Bind(g)
	/// 
	///		We will do it in 2 stages.  First state is (m.Bind(f)) then its result
	///		(m.Bind(f)).Bind(g)
	/// 	m.Bind(f) =
	///			// Plug it into the Bind function: m.Bind(f) => await f(await m)
	///			(await f(await m)).Wrap() =
	///			f(await m)
	///
	///		Now add the Bind(g)
	///			m.Bind(f).Bind(g) =
	///			f(wait m).Bind(g) =
	///			// Plug it into the Bind function: n.Bind(g) => await g(await n)
	///			// .. where n = f(await m)
	///			(await g(await f(await m))).Wrap() =
	///			g(await f(await m))
	/// 	Therefore:
	/// 		m.Bind(f).Bind(g) = m.Bind(g ° f)
	///
	///		The above argument does not explain the reverse process:
	///			starting at m.Bind(g ° f) and ending up with m.Bind(f).Bind(g)
	///			// Looking at m.Bind(g ° f), let's note that g ° f = x => f(x).Bind(g)
	///			// Where x relationship to m is: m = Wrap(x)
	///			// Therefore: 
	///			// 		f(x).Bind(g) =
	///			// 		await g(await f(x)).Wrap() =
	///			// 		g(await f(x)) =
	///			//		g ° f
	///		so:
	///			m.Bind(g ° f) =
	///			m.Bind(x => f(x).Bind(g)) =
	///			await x => f(x).Bind(g).Wrap() =
	///			await x => (await g(await f(x)) =
	///			await (x => f(x)).Bind(g) =
	///			m.Bind(f).Bind(g)
	///	<remark>
	///	Hereon, we will decorate the awaited values with ConfigureAwait(false) which will
	/// prevent the compiler from reusing the same thread on the continuation.  This is
	/// only useful when we deal with UI. Libraries in general, use the
	/// ConfigureAwait(false) and leave the responsibility to ensure thread affinity to
	/// the routine.
	/// </remark>
	/// </summary>
	public static async Task<R> Bind<T, R>(this Task<T> m, Func<T, Task<R>> continueWith)
	{
		var src = await m.ConfigureAwait(false);
		return await continueWith(src).ConfigureAwait(false);
	}

	/// <summary>
	/// We already know from the sequence monad that Bind(..) maps to SelectMany(..)
	///
	/// Equivalent, Sequence Monad, SelectMany:
	/// public static IEnumerable<TResult> SelectMany<TSource, TResult>(
	///		this IEnumerable<TSource> source, Func<TSource, IEnumerable<TResult>> selector)
	/// </summary>
	public static Task<R> SelectMany<T, R>(this Task<T> m, Func<T, Task<R>> continueWith)
		=> m.Bind(continueWith);

	/// <summary>
	/// public static IEnumerable<TResult> SelectMany<TSource, TCollection, TResult>(
	///		this IEnumerable<TSource> source,
	///		Func<TSource, IEnumerable<TCollection>> collectionSelector,
	///		Func<TSource, TCollection, TResult> resultSelector)
	/// </summary>
	public static async Task<R> SelectMany<TIn, TMid, R>(
		this Task<TIn> src, Func<TIn, Task<TMid>> f, Func<TIn, TMid, R> projection)
	{
		TIn outer = await src.ConfigureAwait(false);
		TMid inner = await f(outer).ConfigureAwait(false);
		return projection(outer, inner);
	}

	/// <summary>
	/// Equivalent, Sequence Monad, Where:
	/// public static IEnumerable<TSource> Where<TSource>(
	///		this IEnumerable<TSource> source,
	///		Func<TSource, bool> predicate)
	/// </summary>
	public static async Task<T> Where<T>(this Task<T> source, Func<T, bool> predicate)
	{
		T t = await source.ConfigureAwait(false);
		if (!predicate(t)) return default;
		return t;
	}

	/// <summary>
	/// No equivalent LINQ method.  Map is equivalent to the Select method, the name Map is
	/// the name used in functional languages.
	/// </summary>
	public static async Task<R> Map<T, R>(this Task<T> m, Func<T, R> map)
		=> map(await m.ConfigureAwait(false));

	/// <summary>
	/// Equivalent, Sequence Monad, Select:
	/// public static IEnumerable<TResult> Select<TSource, TResult>(
	///		this IEnumerable<TSource> source, 
	///		Func<TSource, int, TResult> selector)
	/// </summary>
	public static async Task<U> Select<T, U>(this Task<T> source, Func<T, U> selector)
		=> selector(await source.ConfigureAwait(false));

	/// <summary>
	/// Enumerable.Join from: https://referencesource.microsoft.com/#System.Core/System/Linq/Enumerable.cs,c483e0663f3b76e5
	/// Join is the SQL inner-join equivalent.
	///
	/// Equivalent, Sequence Monad, Join:
	/// public static IEnumerable<TResult> Join<TOuter, TInner, TKey, TResult>(
	///		this IEnumerable<TOuter> outer,
	///		IEnumerable<TInner> inner,
	///		Func<TOuter, TKey> outerKeySelector,
	///		Func<TInner, TKey> innerKeySelector,
	///		Func<TOuter, TInner, TResult> resultSelector)
	/// </summary>
	public static async Task<TRes> Join<TIn, TOut, TKey, TRes>(
		this Task<TIn> src,
		Task<TOut> inner,
		Func<TIn, TKey> outerKeySelector,
		Func<TOut, TKey> innerKeySelector,
		Func<TIn, TOut, TRes> resultSelector)
	{
		await Task.WhenAll(src, inner).ConfigureAwait(false);
		if (!EqualityComparer<TKey>.Default.Equals(
				outerKeySelector(src.Result), innerKeySelector(inner.Result)))
			return default;

		return resultSelector(src.Result, inner.Result);
	}

	/// <summary>
	/// Enumerable.GroupJoin, from: https://referencesource.microsoft.com/#System.Core/System/Linq/Enumerable.cs,d67aa32a2dfc9b85
	/// GroupJoin is the SQL Left outer Join equivalent
	///
	/// Equivalent, Sequence Monad, GroupJoin:
	/// public static IEnumerable<TResult> GroupJoin<TOuter, TInner, TKey, TResult>(
	///		this IEnumerable<TOuter> outer,
	///		IEnumerable<TInner> inner,
	///		Func<TOuter, TKey> outerKeySelector,
	///		Func<TInner, TKey> innerKeySelector,
	///		Func<TOuter, IEnumerable<TInner>, TResult> resultSelector)
	/// </summary>
	public static async Task<TRes> GroupJoin<TIn, TOut, TKey, TRes>(
		this Task<TIn> source,
		Task<TOut> inner,
		Func<TIn, TKey> outerKeySelector,
		Func<TOut, TKey> innerKeySelector,
		Func<TIn, Task<TOut>, TRes> resultSelector)
	{
		TIn t = await source.ConfigureAwait(false);
		return resultSelector(t,
			inner.Where(u => EqualityComparer<TKey>.Default.Equals(
				outerKeySelector(t), innerKeySelector(u))));
	}

	/// <summary>
	/// The Tap operator is useful to bridge void functions (such as logging) 
	/// in a composition without having to create additional code
	/// </summary>
	public static async Task<T> Tap<T>(this Task<T> m, Func<T, Task> action)
	{
		var src = await m.ConfigureAwait(false);
		await action(src).ConfigureAwait(false);
		return src;
	}

	/// <summary>
	/// This method, potentially not belonging to a Task Monad collection.  Potentially it
	/// belongs in a static AsyncExt class.
	///
	/// This method is destructive of input parameter
	/// </summary>
	public static IEnumerable<T> WaitAllOneByOne<T>(this List<T> tsks) where T : Task
	{
		while (tsks.Count > 0)
		{
			// WaitAny does not throw exception.  The calling program will need 
			// to try/catch the resulting task.Result or .Wait() statements.
			var inx = Task.WaitAny(tsks.ToArray());

			yield return tsks[inx];
			tsks.RemoveAt(inx);
		}
	}
}

public static class Ext
{
	// Monad Transformer
	public static Func<IEnumerable<TSource>> Lift<TSource>(this Func<TSource> f)
	where TSource : class
	{
		return () => new List<TSource> { f() };
	}
}

class Program
{
	static void Main()
	{
		// Predictor of total cost of pizza, of code camp 2019
		// *	Predictor of number of attendies, N: a random number between 100 - 500
		// *	Every attendee has
		// *		Id:	 (a guid).
		// *		Count: Number of slices consumed, a randomly generated number 1 - 7
		// *	Cost per person: [Number of slices consumed] * [cost per slice (2.50)]
		var pplCount = Pseudorandom.NextInt(100, 401);
		Console.WriteLine($"People count: {pplCount:#,##0}");

		var ppl = new List<Person>();
		Console.Write("Generating people...  ");
		var sw = Stopwatch.StartNew();
		Enumerable.Range(0, pplCount).AsParallel()           // Generate the N people
			.ToList().ForEach(_ => ppl.Add(Person.GetPerson()));
		sw.Stop();
		Console.WriteLine($"Done.  {sw.ElapsedMilliseconds / 1000.0:#,##0.0} sec");

		Console.WriteLine("First 5 People");
		WritePeople(ppl, 5);

		Console.WriteLine();
		Console.Write("Calculating cost of pizza...");
		sw.Restart();
		// Note that this is *not* a SelectMany(..)!  It is *not* a
		//		from person in people
		//		from t in Task(calculate cost)
		//  	select ..
		// Because this will require the same Monad type and we are mixing Monads
		// IEnumerable<T> for the sequence monad: from person in people
		// and Task<T> for the async monad: from t in Task.Run(..)
		var tsks = (
			from person in ppl
			let personCostAndid = from pCost in Task.Run(() => person.CalcCost())
						select (person, pCost, Thread.CurrentThread.ManagedThreadId)
			select personCostAndid
		).ToList();

//		var tsksDoNotUse = (
//			from person in ppl
//			from perCost in Ext.Lift(() =>
//				from pCost in Task.Run(() => person.CalcCost())
//				select (person, pCost, Thread.CurrentThread.ManagedThreadId))()
//			select perCost).ToList();
//		var tsks = ppl.SelectMany(person => Ext.Lift(() =>
//			Task.Run(() => person.CalcCost())
//				.Select(pCost => (person, pCost, Thread.CurrentThread.ManagedThreadId))
//			)()
//		).ToList();

		// WaitAllOneByOne
		Console.WriteLine($"starting to aggregate results for N: {pplCount:#,##0}");
		var totalCost = 0m;
		var tskCntCheck = 0;
		var thCount = new Dictionary<int, int>();       // We keep track of thread use
		var costCache = new List<(Person, decimal, int)>();
		foreach (var tsk in tsks.WaitAllOneByOne())
		{
			++tskCntCheck;      // Task count
			(Person p, decimal pizzaCost, int tId) = tsk.Result;
			costCache.Add((p, pizzaCost, tId));
			Console.WriteLine($"Task: {tskCntCheck,4}.  Person ID: {p.InternalId}, " +
							  $"Slices consumed: {p.SlicesConsumed}, " +
							  $"cost: {pizzaCost,5}, Ran on tread: {tId,3}");
			totalCost += pizzaCost;

			// Thread
			if (!thCount.Keys.Contains(tId)) thCount.Add(tId, 0);
			++thCount[tId];
		}
		sw.Stop();

		Console.WriteLine($"       Total PizzaCost: {totalCost,10:#,##0.00}");
		Console.WriteLine($"Seconds: {(double)sw.ElapsedMilliseconds / 1000.0:#,##0.00}");

		Console.WriteLine();
		Console.WriteLine("Analytics:");
		Console.WriteLine($"Threads used: {thCount.Count()}.  " +
						  $"Processor count: {Environment.ProcessorCount}");
		foreach (var th in thCount.OrderByDescending(t => t.Value))
			Console.WriteLine($"Thread[{th.Key,4}]: {th.Value,3}");
		Console.WriteLine("--");
		Console.WriteLine($"Attendee count = {pplCount:#,##0}.  " +
						  $"{tskCntCheck:#,##0} = Task Count Check");
	}

	private static void WritePeople(List<Person> ppl, int personPrintCount)
	{
		int i = 0;
		foreach (var person in ppl)
		{
			if (i++ >= personPrintCount) break;
			Console.WriteLine($"Person Id: {person.InternalId.ToString()}, " +
							  $"SlicesConsumed: {person.SlicesConsumed}.");
		}
	}
}

class Person
{
	const decimal SliceCost = 2.50m;
	public Person(Guid id, int slicesConsumed)
	{
		InternalId = id;
		SlicesConsumed = slicesConsumed;
	}
	public Guid InternalId { get; set; }
	public int SlicesConsumed { get; set; }
	public override string ToString() => $"{InternalId}: {SlicesConsumed}";

	public decimal CalcCost()
	{
		// delayTime is between 10 and 250 milliseconds
		var delayTime = 10 + BetterRandom.NextInt() % 241;
		Thread.Sleep(delayTime);
		
		return SlicesConsumed * SliceCost;
	}

	public static Person GetPerson()
	{
		// *	Name predictor of every atendee: a random lenth name between 5 and 25 alpha
		// 			random generated string with 1 or 2 space characters in it.
		Guid id = Guid.NewGuid();
		int slicesConsumed = BetterRandom.NextInt() % 7 + 1;
		return new Person(id, slicesConsumed);
	}
}

public static class Pseudorandom
{
	private readonly static ThreadLocal<Random> prng
		= new ThreadLocal<Random>(() => new Random(BetterRandom.NextInt()));

	public static int NextInt() => prng.Value.Next();

	public static int NextInt(int lowInclusive, int hiExclusive)
		=> lowInclusive + prng.Value.Next() % (hiExclusive - lowInclusive);

	public static double NextDouble() => prng.Value.NextDouble();
}

/// <summary>
/// For more information about BetterRandom see:
/// 	https://ericlippert.com/2019/01/31/fixing-random-part-1/
/// </summary>
public static class BetterRandom
{
	// First, all the state is thread-local, so we trade a small amount of per-thread 
	// overhead and per-call indirection for thread safety, which is probably a good 
	// tradeoff. 
	private static readonly ThreadLocal<RandomNumberGenerator> crng =
		new ThreadLocal<RandomNumberGenerator>(RandomNumberGenerator.Create);

	// We’re going to be using the same four-byte buffer over and over again, so I figured 
	// it is probably worthwhile to cache it and avoid the hit to collection pressure; 
	// however this is just a guess and I would want to verify that with empirical tests.
	private static readonly ThreadLocal<byte[]> bytes =
		new ThreadLocal<byte[]>(() => new byte[sizeof(int)]);

	public static int NextInt()
	{
		crng.Value.GetBytes(bytes.Value);
		return BitConverter.ToInt32(bytes.Value, 0) & int.MaxValue;
	}

	public static double NextDouble()
	{
		while (true)
		{
			// Only positive integers; clearing the top bit by and-ing with 0x7FFFFFF does
			// that nicely without changing the distribution.
			long x = NextInt() & 0x001FFFFF;
			x <<= 31;
			x |= (long)NextInt();

			// For a random double, we know that doubles have 52 bits of precision, so we
			// generate a random 52 bit integer and make that the numerator of a fraction. 
			// We want to guarantee that 1.0 is never a possible output, so we reject that
			// possibility; one in every few billion calls we’ll do some additional calls 
			// to NextInt. I can live with that.
			double n = x;
			const double d = 1L << 52;
			double q = n / d;
			if (q != 1.0) return q;
		}
	}
}