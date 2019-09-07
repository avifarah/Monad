<Query Kind="Program">
  <Output>DataGrids</Output>
</Query>

/// <summary>
/// The Maybe pattern is implemented in the .Net framework as Nullable<T>
/// Nullable has restrictions, for example Nullable cannot operate on Nullable<T>.
/// The compiler lifts base type to the Nullable<T> type:
///		For example: Nullable<int> x = 5;
///		5 is of type int (not type Nullable<int>)
///		The compiler lifts 5 to Nullable<int> in order to assign it to x.
/// Therefore, The compiler puts restrictions on Nullable, for this reason 
/// my examples are not on Nullable.
///
/// At times you see the Maybe<T> pattern named Option<T> after the F# option.
/// In which case we have
/// 	Option<T>.Some  equivalent to  Nullable<T>.Value
///		Option<T>.None  equivalent to  Nullable<T>.Null
///		Option<T>.Match equivalent to  Nullable<T>.HasValue
/// </summary>
struct Maybe<T>
{
	// ctor
	public Maybe(T value) { _value = value; HasValue = true; }

	// Having no value:  Maybe-Null
	public static readonly Maybe<T> Null = default(Maybe<T>);

	public T Value { get { if (!HasValue) throw new Exception(); return _value; } }

	public override string ToString() => HasValue ? Value.ToString() : "Null";

	public bool HasValue { get; private set; }

	private T _value;
}

static class MaybeExtensions
{
	public static Maybe<T> Wrap<T>(this T value) { return new Maybe<T>(value); }

	// In order for Bind<T, R>(..) to be a monadic Bind, it must abide by 3 rules:
	// Left Identity:	Wrap(x).Bind(f) == f(x)
	// Right Identity:	m.Bind(Wrap) == m
	// Associative:  	m.Bind(g ° f) == m.Bind(f).Bind(g)
	//
	// Left Idendity: Wrap(x).Bind(f) =
	//		Plug in Wrap and Bind:
	//			If x is null then Wrap(null) == NULL	(see definintion of Bind below)
	//			and Null.Bind(..) = NULL
	//		otherwise:
	//		1.	Wrap(x) == new Maybe<T>(x)
	//		2.	m.Bind(f) == m.Bind(t => f(t))
	//		Therefore:
	//			Wrap(x).Bind(f) =
	//			(new Maybe<T>(x)).Bind(t => f(t)) =
	//			(new Maybe<T>(x)).HasValue ? f((new Maybe<T>(x)).Value) : Null =
	//			(new Maybe<T>(x)).HasValue ? f(x) : Null =
	//			f(x)		// x is not a null
	//
	// We will plug into the the Right Identity Wrap and Bind declations:
	// Right Identity:	m.Bind(Wrap) =
	//		if m is null then Bind(..) returns Null (see the deinition of Bind below)
	//		else Bind(..) returns function(m.Value)
	// Now, if m is not null then:
	// Since the function is Wrap(x)
	// then
	//		m.Bind(Wrap) = 
	//		Wrap(m.Value) = 
	//		Wrap(x) = 
	//		m
	// Therefore, m.Bind(Wrap) = m
	//
	//	Associative/Chain:	m.Bind(f).Bind(g)
	//		We will plug into the definitions of Wrap and Bind
	//		We will first evaluate m.Bind(f) then we will apply the result to Bind(g)
	//		The definition of the Bind(..) is:
	//		m.Bind(f) = m.HasValue ? f(m.Value) : Maybe<R>.Null
	//		so if m is Null then m.Bind(f) is Null else m.Bind(f) is f(m.Value)
	//
	//		To complete the m.Bind(f).Bind(g): we will take the previous result and plug
	//		the result to Bind(g): 
	//		(if m is Null then m.Bind(f) is Null else m.Bind(f) is f(m.Value)).Bind(g)
	//		Like before, plug the result into the below Bind(..)
	//		If m.Bind(f) is Null then m.Bind(f).Bind(g) is Null
	//		else f(m.Value).Bind(g) =
	//			Let  (x == m.Value)
	//			Then f(x).Bind(g) = g(f(x).Value)
	//		Therefore, f(x).Bind(g) = 
	//			if the original m is Null then return Null,
	//			else if f(m.Value) is Null then return Null
	//			else return g(f(m.Value).Value)
	//		Therefore m.Bind(f).Bind(g) = m.Bind(g ° f)
	public static Maybe<R> Bind<T, R>(this Maybe<T> m, Func<T, Maybe<R>> function) => m.HasValue ? function(m.Value) : Maybe<R>.Null;
}

class Program
{
	static Func<int, Maybe<double>> log = x => x > 0 ? new Maybe<double>(Math.Log(x)) : Maybe<double>.Null;
	static Func<double, Maybe<decimal>> toDecimal = y => (Math.Abs(y) < (double)decimal.MaxValue) ? new Maybe<decimal>((decimal)y) : Maybe<decimal>.Null;
	
	//
	// The composition!!!
	//
	static Func<X, Maybe<Z>> Compose<X, Y, Z>(Func<X, Maybe<Y>> f, Func<Y, Maybe<Z>> g) => x => f(x).Bind(g);

	static void Main()
	{
		Func<int, Maybe<decimal>> combine = Compose(log, toDecimal);
		var c = combine(2);     // 0.693147180559945
		var expected = (decimal)Math.Log(2.0);
		Console.WriteLine($"Expected  : {expected}{Environment.NewLine}" +
						  $"Actual    : {c}{Environment.NewLine}" +
						  $"Exp - Act : {Math.Abs(expected - c.Value)}");
	}
}
//