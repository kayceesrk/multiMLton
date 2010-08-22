structure safeRef: SAFEREF =
  struct
    structure G = StableGraph
    structure T = Thread
    structure MT = MLton.Thread
    structure C = MLton.Cont

    
    exception readERROR
    exception writeERROR
    exception revertERROR

    val count1 = ref 0
    datatype sref = datatype RepTypes.sref

    

    fun newSR(element) = 
      	 let val bs = ref false
            val con = ref fn () => () (*(G.COPYTHREAD)*)
            val cpyF = fn h:'a =>
                    C.callcc(fn z =>
                             (
                              (*G.debug("SCOPY: about to assign ref\n");*) 
                              con:= fn () => (bs:= true;C.throw(z, h);()); 
                              (*G.debug("Assign sucessful!\n");*)
                              
                              h))
             val _ = cpyF()
         in if !bs
            then newSR(element)
            else let val _ = MT.atomicBegin()
                     val count = !count1
                     val _ = count1 := (count +1)
                     val tid = T.getTid() 
                     val _ = G.schedThread(con, tid)
                     val a = G.write2Ref(count, tid)
                     val ret = SREF{history = ref [(a, element)],
                                         id = count}          
                     val _ = MT.atomicEnd()
                 in ret
                 end
         end

    fun readSR(SREF{history, id}) =
      let val _ = MT.atomicBegin()
          val _ = G.readFromRef(id, T.getTid()) 
          val ret = 
            case (!history)
              of (num, x)::xs => x
               | _ => raise readERROR
          val _ = MT.atomicEnd()
      in ret
      end

    fun writeSR(element, s as SREF{history, id}) =
      let val bs = ref false
            val con = ref fn () => () (*(G.COPYTHREAD)*)
            val cpyF = fn h:'a =>
                    C.callcc(fn z =>
                             (
                              (*G.debug("SCOPY: about to assign ref\n");*) 
                              con:= fn () => (bs:= true;C.throw(z, h);()); 
                              (*G.debug("Assign sucessful!\n");*)
                              
                              h))
             val _ = cpyF()
         in if !bs
            then writeSR(element, s)
            else let val _ = MT.atomicBegin()
                     val tid = T.getTid()
                     val _ = G.schedThread(con, tid)
                     val a = G.write2Ref(id, tid)
                     val _ = 
                       case (!history)
                         of x::xs => history := ((a, element)::(!history))
                          | _ => raise writeERROR
                     val _ = MT.atomicEnd()
                 in ()
                 end
         end

   

  end