<Query Kind="Program">
  <Output>DataGrids</Output>
</Query>

static class SequenceMonad
{
	public static IEnumerable<T> Wrap<T>(this T x) { yield return x; }

	// Left Identity:		Wrap(x).Bind(f) == f(x)
	// Right Identity: 		m.Bind(Wrap) == m
	// Associative/Chain:	m.Bind(f).Bind(g) == m.Bind(g ° f)
	// 
	// Left Identity:
	// Wrap(x) is an IEnumerable of a single item, x of type T.  Plugging it into
	// the below Bind(..) the
	//		foreach (item in m)
	// boils down to a single iteration where item is x.  Therefore:
	// Wrap(x).Bind(f) =
	// 		var rs = f(x)
	//		foreach (var r in rs)
	//			yield return r
	//
	// Which is f(x).
	// Therefore Wrap(x).Bind(f) = f(t)
	//
	// Right Identity: 		m.Bind(Wrap) == m, the function in the Bind(..)
	// Plugging the Wrap function in for *f* in the Bind(..) below, yields:
	//		foreach (var x in m)
	//			var rs = Wrap(x)	// Which is IEnumerable of a single item x
	//			foreach (var r in rs)	// boils down to a single iteration where
	//									// r = x. So yield return r is yield return x
	//
	// Therefore:
	//		foreach (var x in m)
	//			return x
	// Therefore: m.Bind(Wrap) = m
	//
	// Associative/Chain:	m.Bind(f).Bind(g) == m.Bind(g ° f)
	// Plug in the values into the Bind(..) function below:
	// m.Bind(f)
	//		foreach (var x in m)
	//			var rs = f(x)
	//			foreach (var r in rs)
	//				yield return r;
	// which is an IEnumerable<R> or IEnumerable of: { forall i,j: f(x[i])[j] }.
	// The resulting IEnumerable is over a single index.  It is a flattened out
	// version of the 2 indexes.
	//
	// Now m.Bind(f).Bind(g)
	//		foreach (var y in m.Bind(f))
	//			var rs = g(y)
	// is equivalent to:
	//		foreach (var y in { f(x[i])[j] })
	//			foreach (var s in { g(f(x[i])[j]) }
	//				yield return s;
	// Which is: m.Bind(g ° f)
	public static IEnumerable<R> Bind<T, R>(this IEnumerable<T> m, Func<T, IEnumerable<R>> f)
	{
		foreach (var x in m)
		{
			var rs = f(x);
			foreach (var r in rs)
				yield return r;
		}
	}
}

class Person
{
	public string Name;
	public string State;
}

class State { public string Name; };

class StateOfUs : State
{
	public string Usps;
	public override string ToString() => $"{Name}: {Usps}";
}

class Program
{
	static void Main()
	{
		// Wrap() lifts the type to a sequence, IEnumerable { p }
		var p = new Person { Name = "Joe", State = "NY" };
		var seqP = p.Wrap();
		Console.WriteLine($"seqP is IEnumerable<>: {seqP is IEnumerable<Person>}");

		Console.WriteLine();
		IEnumerable<Person> persons = new List<Person>
		{
			new Person { Name ="Angela", State = "NJ" },
			new Person { Name = "Bob", State = "NY" },
			new Person { Name = "Cynthia", State = "MA" },
			new Person { Name = "Dan", State = "MA" }
		};
		
		// Base f : Person -> (name, bool /*if the person is from NY*/)
		(string name, bool isFromNy) f(Person per) => (per.Name, per.State == "NY");
		
		// Monadic monFunc: Person -> IEnumerable<ValueType(name, bool)>
		IEnumerable<(string name, bool isFromNy)> monFunc(Person per)
		{
			yield return f(per);
		}

		var rs = persons.Bind(monFunc);
		Console.WriteLine($"rs is IEnumerable<(string, bool)>: {rs is IEnumerable<(string, bool)>}");
		foreach (var r in rs)
			Console.WriteLine($"{r.name}, {(r.isFromNy ? "From NY" : "Not from NY")}");
	}

	static void WorkIt()
	{
		var persons = new List<Person> {
			new Person { Name ="Albert", State = "NJ" },
			new Person { Name = "Bernulli", State = "NY" },
			new Person { Name = "Charles", State = "MA" },
		};
		var statesOfUs = new List<StateOfUs> {
			new StateOfUs { Name = "New York", Usps = "NY" },
			new StateOfUs { Name = "New Jersey", Usps = "NJ" },
			new StateOfUs { Name = "Connecticut", Usps = "CT" },
			new StateOfUs { Name = "Massachusetts", Usps = "MA" },
		};

		{
			// Result without selector
			// person.Bind(f)
			// where f = the join of persons and statesOfUs over the state postal name
			Console.WriteLine("---- Results without selector: using person.Bind(f), f takes a Person and return IEnumerable<StateOfUs>");
			Func<Person, IEnumerable<StateOfUs>> f = p => statesOfUs.Where(s => p.State == s.Usps);
			var states = persons.Bind(f);
			states.ToList().ForEach(s => Console.WriteLine(s.ToString()));
		}

		{
			// Use the same f as before but not as a function in the Bind(..) method but as a
			// condition in a Where condition part of SelectMany.
			Console.WriteLine();
			Console.WriteLine("---- Using SelectMany(p => selection of appropriate state).  It results similar to the Bind(f) of previous example");
			var states2 = persons.SelectMany(p => statesOfUs.Where(s => p.State == s.Usps));
			states2.ToList().ForEach(s => Console.WriteLine(s.ToString()));
		}

		{
			// Result with selector
			// Using the same SelectMany in a SQL syntax
			// Havint the same condition person.State == state.Usps
			Console.WriteLine();
			Console.WriteLine("---- Results with selector using SQL syntax.  Now we changed the state to its full name");
			var pTos =
				from person in persons
				from state in statesOfUs
				where person.State == state.Usps
				select new { person, State = state };
			foreach (var item in pTos)
				DoWork((item.person, item.State.Name));
		}

		{
			// Writing the function f explicitly with a foreach loop to return the 
			// set of states meeting the condition of person.State == state.Usps
			Console.WriteLine();
			Console.WriteLine("---- Results without explicit selector: using person.Bind(f), f is expressed as foreach selection");
			// IEnumerable<R> Bind<T, R>(this IEnumerable<T> m, Func<T, IEnumerable<R>> f)
			Func<Person, IEnumerable<(Person, StateOfUs)>> f = p => {
				var res = new List<(Person, StateOfUs)>();
				foreach (var s in statesOfUs)
					if (p.State == s.Usps) res.Add((p, s));
				return res;
			};
			var pTos2 = persons.Bind(f);
			foreach (var item in pTos2)
			{
				(Person person, StateOfUs state) = item;
				DoWork((person, state.Name));
			}
		}

		{
			Console.WriteLine();
			Console.WriteLine("---- Using a different overload of SelectMany(..) that has a selector");
			// IEnumerable<TResult> SelectMany<TSource,TCollection,TResult>(
			//		this IEnumerable<TSource> source,
			//		Func<TSource,IEnumerable<TCollection>> collectionSelector,
			//		Func<TSource,TCollection,TResult> resultSelector)
			var pTos3 = persons.SelectMany(p => 
				statesOfUs.Where(s => p.State == s.Usps),	// collectionSelector
				(p, s) => (p, s)							// resultSelector
			);
			foreach (var item in pTos3)
			{
				(Person person, StateOfUs state) = item;
				DoWork((person, state.Name));
			}
		}
	}

	static void DoWork((Person, string) item)
	{
		(Person p, string state) = item;
		Console.WriteLine($"Name: {p.Name}, state: {state}");
	}
}
//