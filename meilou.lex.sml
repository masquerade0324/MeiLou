functor MeiLouLexFun (structure Tokens: MeiLou_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
COMMENT | INITIAL
    structure UserDeclarations = 
      struct

structure T = Tokens

type pos = int
type svalue = T.svalue
type ('a, 'b) token = ('a, 'b) T.token
type lexresult = (svalue, pos) T.token

val lineno = ref 1
fun eof () = T.EOF (0, 0)

fun println s = print (s ^ "\n")


      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm; ( continue () ))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      ( lineno := !lineno + 1; continue () ))
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        ( println yytext; T.ANDALSO (yypos, yypos + size yytext) )
      end
fun yyAction3 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( println yytext; T.ELSE (yypos, yypos + size yytext) )
      end
fun yyAction4 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( println yytext; T.END (yypos, yypos + size yytext) )
      end
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( println yytext; T.FUN (yypos, yypos + size yytext) )
      end
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( println yytext; T.IF (yypos, yypos + size yytext) )
      end
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( println yytext; T.IN (yypos, yypos + size yytext) )
      end
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( println yytext; T.LET (yypos, yypos + size yytext) )
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        ( println yytext; T.ORELSE (yypos, yypos + size yytext) )
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( println yytext; T.THEN (yypos, yypos + size yytext) )
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( println yytext; T.VAL (yypos, yypos + size yytext) )
      end
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        ( println yytext; T.LPAREN (yypos, yypos + size yytext) )
      end
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        ( println yytext; T.RPAREN (yypos, yypos + size yytext) )
      end
fun yyAction14 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        ( println yytext; T.SEMICOLON (yypos, yypos + size yytext) )
      end
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( println yytext; T.LT (yypos, yypos + size yytext) )
      end
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( println yytext; T.LE(yypos, yypos + size yytext) )
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( println yytext; T.GT (yypos, yypos + size yytext) )
      end
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( println yytext; T.GE (yypos, yypos + size yytext) )
      end
fun yyAction19 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( println yytext; T.EQ (yypos, yypos + size yytext) )
      end
fun yyAction20 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( println yytext; T.NEQ (yypos, yypos + size yytext) )
      end
fun yyAction21 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( println yytext; T.PLUS (yypos, yypos + size yytext) )
      end
fun yyAction22 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        ( println yytext; T.MINUS (yypos, yypos + size yytext) )
      end
fun yyAction23 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        ( println yytext; T.TIMES (yypos, yypos + size yytext) )
      end
fun yyAction24 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( println yytext; T.DIV (yypos, yypos + size yytext) )
      end
fun yyAction25 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( println yytext; T.NOT (yypos, yypos + size yytext) )
      end
fun yyAction26 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        ( println yytext; T.BOOL (true, yypos, yypos + size yytext) )
      end
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        ( println yytext; T.BOOL (false, yypos, yypos + size yytext) )
      end
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        ( println yytext; T.INT (valOf (Int.fromString yytext),
                               yypos,
                               yypos + size yytext) )
      end
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        ( println yytext; T.ID (yytext, yypos, yypos + size yytext) )
      end
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ10(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"0"
              then yyAction28(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ10(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ10(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ10(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ15(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                      else yyAction11(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction11(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ15(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction11(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ15(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ28(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ27(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ15(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                      else yyAction26(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ15(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction26(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ15(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ32(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"v"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"v"
              then if inp = #"u"
                  then yyQ31(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ15(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                      else yyAction10(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction10(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ15(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction10(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ15(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ34(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ33(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #"A"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"A"
                  then if inp <= #"9"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"a"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"a"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp = #"h"
                  then yyQ29(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"s"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"r"
                  then yyQ30(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ15(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction9(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                      else yyAction9(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction9(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ15(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction9(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ15(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ39(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ38(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ37(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ36(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"r"
                  then yyQ35(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ15(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction25(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                      else yyAction25(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction25(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ15(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyAction25(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction25(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction25(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ15(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyAction25(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"t"
                  then yyQ41(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ40(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ15(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                      else yyAction8(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction8(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ15(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ15(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"t"
                  then yyQ43(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ42(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ15(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                      else yyAction7(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ15(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ15(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ15(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                      else yyAction6(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ15(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ15(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #"A"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"A"
                  then if inp <= #"9"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"g"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"g"
              then if inp = #"a"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"a"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp = #"f"
                  then yyQ44(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"o"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ45(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ15(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                      else yyAction5(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction5(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ15(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ15(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ48(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ15(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                      else yyAction27(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ15(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction27(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ15(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ51(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ50(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ49(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #"A"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"A"
                  then if inp <= #"9"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ47(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"a"
                  then yyQ46(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp = #"`"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ15(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                      else yyAction4(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ15(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ15(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"d"
                  then yyQ54(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ15(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction3(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                      else yyAction3(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction3(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ15(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction3(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ15(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ56(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ55(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #"A"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"A"
                  then if inp <= #"9"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"a"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"a"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp = #"l"
                  then yyQ52(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"o"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"o"
              then yyQ53(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ15(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction24(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                      else yyAction24(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction24(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ15(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyAction24(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction24(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction24(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ15(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyAction24(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"w"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"w"
              then if inp = #"v"
                  then yyQ58(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"j"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"j"
              then if inp = #"i"
                  then yyQ57(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ15(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ15(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ15(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ64(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ63(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ62(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ61(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"d"
                  then yyQ60(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ59(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ15(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ65(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ67(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ66(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ2(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ2(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ15(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"+"
                  then yyQ8(strm', lastMatch)
                else if inp < #"+"
                  then if inp = #"\^N"
                      then if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                    else if inp < #"\^N"
                      then if inp = #"\n"
                          then yyQ3(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ2(strm', lastMatch)
                            else if yyInput.eof(!(yystrm))
                              then UserDeclarations.eof(yyarg)
                              else yystuck(lastMatch)
                        else if inp = #"\r"
                          then yyQ4(strm', lastMatch)
                        else if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                    else if inp = #"("
                      then yyQ5(strm', lastMatch)
                    else if inp < #"("
                      then if inp = #" "
                          then yyQ2(strm', lastMatch)
                        else if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                    else if inp = #")"
                      then yyQ6(strm', lastMatch)
                      else yyQ7(strm', lastMatch)
                else if inp = #";"
                  then yyQ11(strm', lastMatch)
                else if inp < #";"
                  then if inp = #"."
                      then if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                    else if inp < #"."
                      then if inp = #","
                          then if yyInput.eof(!(yystrm))
                              then UserDeclarations.eof(yyarg)
                              else yystuck(lastMatch)
                          else yyQ9(strm', lastMatch)
                    else if inp = #"0"
                      then yyQ10(strm', lastMatch)
                    else if inp < #"0"
                      then if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                    else if inp = #":"
                      then if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                      else yyQ10(strm', lastMatch)
                else if inp = #">"
                  then yyQ14(strm', lastMatch)
                else if inp < #">"
                  then if inp = #"<"
                      then yyQ12(strm', lastMatch)
                      else yyQ13(strm', lastMatch)
                else if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if inp = #"m"
              then yyQ15(strm', lastMatch)
            else if inp < #"m"
              then if inp = #"e"
                  then yyQ18(strm', lastMatch)
                else if inp < #"e"
                  then if inp = #"a"
                      then yyQ16(strm', lastMatch)
                    else if inp < #"a"
                      then if inp <= #"Z"
                          then yyQ15(strm', lastMatch)
                        else if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                    else if inp = #"d"
                      then yyQ17(strm', lastMatch)
                      else yyQ15(strm', lastMatch)
                else if inp = #"i"
                  then yyQ20(strm', lastMatch)
                else if inp < #"i"
                  then if inp = #"f"
                      then yyQ19(strm', lastMatch)
                      else yyQ15(strm', lastMatch)
                else if inp = #"l"
                  then yyQ21(strm', lastMatch)
                  else yyQ15(strm', lastMatch)
            else if inp = #"v"
              then yyQ25(strm', lastMatch)
            else if inp < #"v"
              then if inp = #"p"
                  then yyQ15(strm', lastMatch)
                else if inp < #"p"
                  then if inp = #"n"
                      then yyQ22(strm', lastMatch)
                      else yyQ23(strm', lastMatch)
                else if inp = #"t"
                  then yyQ24(strm', lastMatch)
                  else yyQ15(strm', lastMatch)
            else if inp = #"~"
              then yyQ26(strm', lastMatch)
            else if inp < #"~"
              then if inp <= #"z"
                  then yyQ15(strm', lastMatch)
                else if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of COMMENT => yyQ0(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ1(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end
