structure NBTextIO : NB_TEXT_IO =
struct

  open MLton.PCML
  open TextIO

  datatype ichan = CLOSE_IN of (instream)
                 | CLOSE_OUT of (outstream)
                 | FLUSH_OUT of (outstream)
                 | INPUT1 of (instream)
                 | INPUT of (instream)
                 | INPUT_ALL of (instream)
                 | INPUT_LINE of (instream)
                 | INPUT_N of (instream * int)
                 | LOOKAHEAD of (instream)
                 | OUTPUT1 of (outstream * char)
                 | OUTPUT of (outstream * string)
                 | OUTPUT_SUBSTR of (outstream * substring)
                 | PRINT of string

  datatype ochan = RES_INPUT1 of (char option)
                 | RES_INPUT of (string)
                 | RES_INPUT_ALL of (string)
                 | RES_INPUT_LINE of (string option)
                 | RES_INPUT_N of (string)
                 | RES_LOOKAHEAD of (char option)

  val inputChan : ichan chan = channel ()
  val outputChan: ochan chan = channel ()

  fun main () =
    case recv (inputChan) of
         CLOSE_IN (istrm) =>  TextIO.closeIn (istrm)
       | CLOSE_OUT (ostrm) => TextIO.closeOut (ostrm)
       | FLUSH_OUT (ostrm) => TextIO.flushOut (ostrm)
       | INPUT1 (istrm) => send (outputChan, RES_INPUT1 (TextIO.input1 istrm))
       | INPUT (istrm) =>  send (outputChan, RES_INPUT (TextIO.input istrm))
       | INPUT_ALL (istrm) => send (outputChan, RES_INPUT_ALL (inputAll istrm))
       | INPUT_LINE (istrm) =>  send (outputChan, RES_INPUT_LINE (inputLine istrm))
       | OUTPUT (ostrm, s) => output (ostrm, s)
       | PRINT (str) => print (str)
       | _ => raise Fail "Not implemented"

  fun print s = send (inputChan, PRINT (s))
  fun closeIn istrm = send (inputChan, CLOSE_IN (istrm))
  fun closeOut ostrm = send (inputChan, CLOSE_OUT (ostrm))
  fun flushOut ostrm = send (inputChan, FLUSH_OUT ostrm)

  fun input1 istrm =
  let
    val _ = send (inputChan, INPUT1 istrm)
    val RES_INPUT1 (res) = recv (outputChan)
  in
    res
  end

  fun input istrm =
  let
    val _ = send (inputChan, INPUT istrm)
    val RES_INPUT (res) = recv (outputChan)
  in
    res
  end

  fun inputAll istrm =
  let
    val _ = send (inputChan, INPUT_ALL istrm)
    val RES_INPUT_ALL (res) = recv (outputChan)
  in
    res
  end

  fun inputLine istrm =
  let
    val _ = send (inputChan, INPUT_LINE istrm)
    val RES_INPUT_LINE (res) = recv (outputChan)
  in
    res
  end

  fun inputN (istrm, n) =
  let
    val _ = send (inputChan, INPUT_N (istrm, n))
    val RES_INPUT_N (res) = recv (outputChan)
  in
    res
  end

  fun lookahead istrm =
  let
    val _ = send (inputChan, LOOKAHEAD istrm)
    val RES_LOOKAHEAD (res) = recv (outputChan)
  in
    res
  end

  fun ouput1 (ostrm, e) = send (inputChan, OUTPUT1 (ostrm, e))
  fun output (ostrm, v) = send (inputChan, OUTPUT (ostrm, v))
  fun outputSubstr (ostrm, substr) = send (inputChan, OUTPUT_SUBSTR (ostrm, substr))

end
