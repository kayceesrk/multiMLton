structure HashTable =
struct
datatype ('a, 'b) hash_table = HT of {
	entries : ('a * 'b) HashSet.t ref,
	hash_fn : 'a -> word,
	eq_fn : ('a * 'a) -> bool,
    lock : Lock.cmlLock
	}

exception InconsistentState

structure S = MLtonThread
(* The HashTable requires functions that just work on the index
* But the underlying entries need a hash function that works
* on the key, value pair. this creates one out of the other.
*)
fun hashSet_hash_fn f (key, _(*val*)) = f key

(* Create a new table; the int is a size hint and the exception
* is to be raised by find.
*)
fun size (HT{entries, ...}) = HashSet.size (!entries)

fun new (sizeHint, hash_fn, eq_fn) = HT{
	entries = ref (HashSet.newOfSize {
				hash = hashSet_hash_fn hash_fn,
				size = sizeHint}),
	hash_fn = hash_fn,
	eq_fn = eq_fn,
    lock = Lock.initCmlLock ()
	}

fun put (HT{entries, eq_fn, hash_fn, lock}) (key, value) =
	let
      val hashSet = !entries
      val _ = S.atomicBegin ()
      val _ = Lock.getCmlLock lock (Basic.tidNum ())
      val res = ignore (HashSet.insertIfNew(hashSet,
					hash_fn key,
					fn (k, _) => eq_fn (key, k),
					fn () => (key, value),
					fn ((*k, v*) _, _) =>
                                           let val _ = HashSet.remove(hashSet, hash_fn key, fn (k, _) => eq_fn (key, k))
                                               val _ = HashSet.insertIfNew(hashSet, hash_fn key,
                                                                           fn (k, _) => eq_fn (key, k),
                                                                           fn () => (key, value),
                                                                           fn ((*k, v*) _, _) => raise InconsistentState)
                                           in ()
                                           end))
      val _ = Lock.releaseCmlLock lock (Basic.tidNum ())
      val _ = S.atomicEnd ()
    in
      res
	end

fun fold (HT{entries, lock, ...}) init f =
let
  val _ = S.atomicBegin ()
  val _ = Lock.getCmlLock lock (Basic.tidNum ())
  val res = HashSet.fold (!entries, init, f)
  val _ = Lock.releaseCmlLock lock (Basic.tidNum ())
  val _ = S.atomicEnd ()
in
  res
end

(* Used to prepend channel nodes to the thread list to save when pruning the graph. *)
fun foldValues (HT{entries, lock, ...}) init f =
let
  val _ = S.atomicBegin ()
  val _ = Lock.getCmlLock lock (Basic.tidNum ())
  val res = HashSet.fold (!entries, init, fn ((_(*key*), v), acc) => f (v, acc))
  val _ = Lock.releaseCmlLock lock (Basic.tidNum ())
  val _ = S.atomicEnd ()
in
  res
end


fun getValues table =
	foldValues table [] (fn(v, acc)=> v::acc)

fun get (HT{entries, eq_fn, hash_fn, lock, ...}) key =
  let
      val _ = S.atomicBegin ()
      val _ = Lock.getCmlLock lock (Basic.tidNum ())
      val entOpt = HashSet.peek(!entries, hash_fn key, fn (k, _) => eq_fn (key, k))
      val res = case entOpt
		of NONE => NONE
		| SOME (_(*key*),v) => SOME (v)
      val _ = Lock.releaseCmlLock lock (Basic.tidNum ())
      val _ = S.atomicEnd ()
 in
   res
 end

fun remove (HT{entries, hash_fn, eq_fn, lock, ...}) key =
let
  val _ = S.atomicBegin ()
  val _ = Lock.getCmlLock lock (Basic.tidNum ())
  val res = HashSet.remove (!entries, hash_fn key, fn (k, _) => eq_fn (key, k)) handle _ => ()
    (*exception raised because entry was not in the table *)
  val _ = Lock.releaseCmlLock lock (Basic.tidNum ())
  val _ = S.atomicEnd ()
in
  res
end

fun putCond table replaceCond (key, value) =
  case (get table key)
    of NONE => put table (key, value)
     | SOME v2 => if replaceCond(value, v2)
                  then put table (key, value)
                  else ()

end
