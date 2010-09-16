structure ThreadID : THREAD_ID_EXTRA =
struct
  structure Assert = LocalAssert(val assert = false)
  structure R = RepTypes
  structure L = Lock

  datatype thread_id = datatype R.thread_id
  datatype thread_id' = datatype thread_id
  datatype thread_type = datatype R.thread_type

  type procNum = int

  fun sameTid (TID{id=a, ...}, TID{id=b, ...}) = a = b
  fun compareTid (TID{id=a, ...}, TID{id=b, ...}) = Int.compare (a, b)
  fun hashTid (TID{id, ...}) = Word.fromInt id

  fun tidToString (TID{id, ...}) = concat["[", StringCvt.padLeft #"0" 6 (Int.toString id), "]"]
  fun tidToInt (TID{id, ...}) = id

  fun exnHandler (_ : exn) = ()
  val defaultExnHandler = ref exnHandler

  fun new' (n, procNum) =
      TID {id = n,
          alert = ref false,
          done_comm = ref false,
          exnHandler = ref (!defaultExnHandler),
          props = ref [],
          dead = CVar.new (),
          threadType = ref HOST,
          parasiteBottom = ref 0,
          preemptParasite = ref true,
          processorId = procNum}

  local
      val tidCounter = ref 0
  in
      fun new () =
        let
            val _ = Assert.assertAtomic' ("ThreadID.newTid", NONE)
            val n = PacmlFFI.fetchAndAdd(tidCounter, 1)
        in
          new' (n, n mod PacmlFFI.numberOfProcessors)
        end

      fun reset () = tidCounter := 0
  end

  fun bogus s =
      let
        val n = CharVector.foldr (fn (c, n) => 2 * n - Char.ord c) 0 s
      in
        new' (n, ~1)
      end


  fun mark (TID{done_comm, ...}) =
      (Assert.assertAtomic' ("ThreadID.mark", NONE)
      ; done_comm := true)
  fun unmark (TID{done_comm, ...}) =
      (Assert.assertAtomic' ("ThreadID.unmark", NONE)
      ; done_comm := false)
  fun isMarked (TID{done_comm, ...}) = !done_comm

  fun getProcId (TID {processorId, ...}) = processorId

  fun sameProcessor (TID{processorId = p1, ...},
                     TID{processorId = p2, ...}) =
                     p1 = p2

  val dummyTid = bogus "dummy"

  val curTid : thread_id array = Array.tabulate(PacmlFFI.numberOfProcessors, fn _ => dummyTid)

  fun getCurThreadId () = Array.sub (curTid, PacmlFFI.processorNumber ())
  fun setCurThreadId tid = Array.update (curTid, PacmlFFI.processorNumber (), tid)

  fun tidMsg () = tidToString (getCurThreadId ())
  fun tidNum () = tidToInt (getCurThreadId ())

end
