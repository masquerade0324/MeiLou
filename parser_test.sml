structure ParserTest :
sig
  val compile  : string -> Absyn.t
end =
struct
  structure MeiLouLrVals =
    MeiLouLrValsFun (structure Token = LrParser.Token)

  structure MeiLouLex =
    MeiLouLexFun (structure Tokens = MeiLouLrVals.Tokens)

  structure MeiLouParser =
    Join (structure ParserData = MeiLouLrVals.ParserData
          structure Lex        = MeiLouLex
          structure LrParser   = LrParser)

  structure P = MeiLouParser

  fun compile file =
    let
      val instrm = TextIO.openIn file
      fun grab n = if TextIO.endOfStream instrm then ""
                   else TextIO.inputN (instrm, n)
      fun printError (msg, posl, posr) = print (msg ^ "\n")
      val (ast, _) = P.parse (15,
                              P.makeLexer grab,
                              printError,
                              ())
    in
      ast
    end
end

structure PT = ParserTest
structure A  = Absyn
