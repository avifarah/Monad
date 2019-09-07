<Query Kind="Program">
  <Output>DataGrids</Output>
</Query>

class Person
{
	public string Name { get; set; }
	public string State { get; set; }
}

class StateOfUs
{
	public string Name { get; set; }
	public string USPS { get; set; }
}

static class JoinExample
{
	public static IEnumerable<TInner> JoinHelper<TOuter, TInner, TKey>(
		this TOuter o,
		IEnumerable<TInner> inners,
		Func<TOuter, TKey> outerKeySelector,
		Func<TInner, TKey> innerKeySelector)
	{
		return inners.Where(i => outerKeySelector(o).Equals(innerKeySelector(i)));
	}

	/// <summary>
	/// See: sourceof.net search for Enumerable.Joins
	/// public static IEnumerable<TResult> Join<TOuter, TInner, TKey, TResult>(
	///			this IEnumerable<TOuter> outer, 
	/// 		IEnumerable<TInner> inner, 
	///			Func<TOuter, TKey> outerKeySelector, 
	///			Func<TInner, TKey> innerKeySelector, 
	///			Func<TOuter, TInner, TResult> resultSelector
	/// )
	/// </summary>
	public static IEnumerable<TResult> JoinEx<TOuter, TInner, TKey, TResult>(
		this IEnumerable<TOuter> outer,
		IEnumerable<TInner> inner,
		Func<TOuter, TKey> outerKeySelector,
		Func<TInner, TKey> innerKeySelector,
		Func<TOuter, TInner, TResult> resultSelector)
	{
		return outer.SelectMany(o => o.JoinHelper(inner, outerKeySelector, innerKeySelector), (o, i) => resultSelector(o, i));
	}
}

class Program
{
	static void Main()
	{
		var persons = new Person[]
		{
			new Person { Name = "Angela", State = "NY" },
			new Person { Name = "Bob", State = "NJ" },
			new Person { Name = "Cynthia", State = "CT" },
			new Person { Name = "Dan", State = "MA" }
		};

		var statesOfUs = new StateOfUs[]
		{
			new StateOfUs { Name = "New York", USPS = "NY" },
			new StateOfUs { Name = "New Jersey", USPS = "NJ" },
			new StateOfUs { Name = "Connecticut", USPS = "CT" },
			new StateOfUs { Name = "Massachusetts", USPS = "MA" }
		};

		{
			Console.WriteLine("---- persons, State Join using Join(..) LINQ function");
			var personsToState = persons.Join(statesOfUs, p => p.State, s => s.USPS, (p, s) => new { Per = p, St = s });
			foreach (var p in personsToState)
				Console.WriteLine($"{p.Per.Name}: {p.St.Name}");
		}

		{
			Console.WriteLine();
			Console.WriteLine("---- persons, State Join using SelectMany");
			var personsToState = persons.SelectMany(p => p.JoinHelper(statesOfUs, per => per.State, s => s.USPS), (p, s) => new { Per = p, St = s });
			foreach (var p in personsToState)
				Console.WriteLine($"{p.Per.Name}: {p.St.Name}");
		}

		{
			Console.WriteLine();
			Console.WriteLine("---- persons, State Join using our version of Join (JoinEx)");
			var personsToState = persons.JoinEx(statesOfUs, p => p.State, s => s.USPS, (p, s) => new { Per = p, St = s });
			foreach (var p in personsToState)
				Console.WriteLine($"{p.Per.Name}: {p.St.Name}");
		}
	}
}