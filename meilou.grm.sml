functor MeiLouLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : MeiLou_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Absyn


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\003\000\091\000\008\000\091\000\009\000\091\000\012\000\091\000\
\\015\000\091\000\024\000\091\000\027\000\091\000\029\000\091\000\
\\034\000\091\000\041\000\091\000\045\000\046\000\049\000\045\000\
\\050\000\044\000\051\000\043\000\052\000\042\000\053\000\041\000\
\\054\000\040\000\055\000\039\000\056\000\038\000\057\000\037\000\
\\063\000\091\000\000\000\
\\001\000\003\000\092\000\008\000\092\000\009\000\092\000\012\000\092\000\
\\015\000\092\000\024\000\092\000\027\000\092\000\029\000\092\000\
\\034\000\092\000\041\000\092\000\063\000\092\000\000\000\
\\001\000\003\000\095\000\008\000\095\000\009\000\095\000\012\000\095\000\
\\015\000\095\000\018\000\031\000\024\000\095\000\027\000\095\000\
\\029\000\095\000\033\000\030\000\034\000\095\000\041\000\095\000\
\\045\000\095\000\049\000\095\000\050\000\095\000\051\000\095\000\
\\052\000\095\000\053\000\095\000\054\000\095\000\055\000\095\000\
\\056\000\095\000\057\000\095\000\060\000\015\000\061\000\014\000\
\\062\000\028\000\063\000\095\000\000\000\
\\001\000\003\000\096\000\008\000\096\000\009\000\096\000\012\000\096\000\
\\015\000\096\000\024\000\096\000\027\000\096\000\029\000\096\000\
\\034\000\096\000\041\000\096\000\045\000\096\000\049\000\096\000\
\\050\000\096\000\051\000\043\000\052\000\042\000\053\000\096\000\
\\054\000\096\000\055\000\096\000\056\000\096\000\057\000\096\000\
\\063\000\096\000\000\000\
\\001\000\003\000\097\000\008\000\097\000\009\000\097\000\012\000\097\000\
\\015\000\097\000\024\000\097\000\027\000\097\000\029\000\097\000\
\\034\000\097\000\041\000\097\000\045\000\097\000\049\000\097\000\
\\050\000\097\000\051\000\043\000\052\000\042\000\053\000\097\000\
\\054\000\097\000\055\000\097\000\056\000\097\000\057\000\097\000\
\\063\000\097\000\000\000\
\\001\000\003\000\098\000\008\000\098\000\009\000\098\000\012\000\098\000\
\\015\000\098\000\024\000\098\000\027\000\098\000\029\000\098\000\
\\034\000\098\000\041\000\098\000\045\000\098\000\049\000\098\000\
\\050\000\098\000\051\000\098\000\052\000\098\000\053\000\098\000\
\\054\000\098\000\055\000\098\000\056\000\098\000\057\000\098\000\
\\063\000\098\000\000\000\
\\001\000\003\000\099\000\008\000\099\000\009\000\099\000\012\000\099\000\
\\015\000\099\000\024\000\099\000\027\000\099\000\029\000\099\000\
\\034\000\099\000\041\000\099\000\045\000\099\000\049\000\099\000\
\\050\000\099\000\051\000\099\000\052\000\099\000\053\000\099\000\
\\054\000\099\000\055\000\099\000\056\000\099\000\057\000\099\000\
\\063\000\099\000\000\000\
\\001\000\003\000\100\000\008\000\100\000\009\000\100\000\012\000\100\000\
\\015\000\100\000\024\000\100\000\027\000\100\000\029\000\100\000\
\\034\000\100\000\041\000\100\000\045\000\100\000\049\000\045\000\
\\050\000\044\000\051\000\043\000\052\000\042\000\053\000\100\000\
\\054\000\100\000\055\000\100\000\056\000\100\000\057\000\100\000\
\\063\000\100\000\000\000\
\\001\000\003\000\101\000\008\000\101\000\009\000\101\000\012\000\101\000\
\\015\000\101\000\024\000\101\000\027\000\101\000\029\000\101\000\
\\034\000\101\000\041\000\101\000\045\000\101\000\049\000\045\000\
\\050\000\044\000\051\000\043\000\052\000\042\000\053\000\101\000\
\\054\000\101\000\055\000\101\000\056\000\101\000\057\000\101\000\
\\063\000\101\000\000\000\
\\001\000\003\000\102\000\008\000\102\000\009\000\102\000\012\000\102\000\
\\015\000\102\000\024\000\102\000\027\000\102\000\029\000\102\000\
\\034\000\102\000\041\000\102\000\045\000\102\000\049\000\045\000\
\\050\000\044\000\051\000\043\000\052\000\042\000\053\000\102\000\
\\054\000\102\000\055\000\102\000\056\000\102\000\057\000\102\000\
\\063\000\102\000\000\000\
\\001\000\003\000\103\000\008\000\103\000\009\000\103\000\012\000\103\000\
\\015\000\103\000\024\000\103\000\027\000\103\000\029\000\103\000\
\\034\000\103\000\041\000\103\000\045\000\103\000\049\000\045\000\
\\050\000\044\000\051\000\043\000\052\000\042\000\053\000\103\000\
\\054\000\103\000\055\000\103\000\056\000\103\000\057\000\103\000\
\\063\000\103\000\000\000\
\\001\000\003\000\104\000\008\000\104\000\009\000\104\000\012\000\104\000\
\\015\000\104\000\024\000\104\000\027\000\104\000\029\000\104\000\
\\034\000\104\000\041\000\104\000\045\000\104\000\049\000\045\000\
\\050\000\044\000\051\000\043\000\052\000\042\000\053\000\104\000\
\\054\000\104\000\055\000\104\000\056\000\104\000\057\000\104\000\
\\063\000\104\000\000\000\
\\001\000\003\000\105\000\008\000\105\000\009\000\105\000\012\000\105\000\
\\015\000\105\000\024\000\105\000\027\000\105\000\029\000\105\000\
\\034\000\105\000\041\000\105\000\045\000\105\000\049\000\045\000\
\\050\000\044\000\051\000\043\000\052\000\042\000\053\000\105\000\
\\054\000\105\000\055\000\105\000\056\000\105\000\057\000\105\000\
\\063\000\105\000\000\000\
\\001\000\003\000\106\000\008\000\106\000\009\000\106\000\012\000\106\000\
\\015\000\106\000\018\000\106\000\024\000\106\000\027\000\106\000\
\\029\000\106\000\033\000\106\000\034\000\106\000\041\000\106\000\
\\045\000\106\000\049\000\106\000\050\000\106\000\051\000\106\000\
\\052\000\106\000\053\000\106\000\054\000\106\000\055\000\106\000\
\\056\000\106\000\057\000\106\000\060\000\106\000\061\000\106\000\
\\062\000\106\000\063\000\106\000\000\000\
\\001\000\003\000\107\000\008\000\107\000\009\000\107\000\012\000\107\000\
\\015\000\107\000\018\000\107\000\024\000\107\000\027\000\107\000\
\\029\000\107\000\033\000\107\000\034\000\107\000\041\000\107\000\
\\045\000\107\000\049\000\107\000\050\000\107\000\051\000\107\000\
\\052\000\107\000\053\000\107\000\054\000\107\000\055\000\107\000\
\\056\000\107\000\057\000\107\000\060\000\107\000\061\000\107\000\
\\062\000\107\000\063\000\107\000\000\000\
\\001\000\003\000\108\000\008\000\108\000\009\000\108\000\012\000\108\000\
\\015\000\108\000\018\000\108\000\024\000\108\000\027\000\108\000\
\\029\000\108\000\033\000\108\000\034\000\108\000\041\000\108\000\
\\045\000\108\000\049\000\108\000\050\000\108\000\051\000\108\000\
\\052\000\108\000\053\000\108\000\054\000\108\000\055\000\108\000\
\\056\000\108\000\057\000\108\000\060\000\108\000\061\000\108\000\
\\062\000\108\000\063\000\108\000\000\000\
\\001\000\003\000\109\000\008\000\109\000\009\000\109\000\012\000\109\000\
\\015\000\109\000\018\000\109\000\024\000\109\000\027\000\109\000\
\\029\000\109\000\033\000\109\000\034\000\109\000\041\000\109\000\
\\045\000\109\000\049\000\109\000\050\000\109\000\051\000\109\000\
\\052\000\109\000\053\000\109\000\054\000\109\000\055\000\109\000\
\\056\000\109\000\057\000\109\000\060\000\109\000\061\000\109\000\
\\062\000\109\000\063\000\109\000\000\000\
\\001\000\003\000\110\000\008\000\110\000\009\000\110\000\012\000\110\000\
\\015\000\110\000\018\000\110\000\024\000\110\000\027\000\110\000\
\\029\000\110\000\033\000\110\000\034\000\110\000\041\000\110\000\
\\045\000\110\000\049\000\110\000\050\000\110\000\051\000\110\000\
\\052\000\110\000\053\000\110\000\054\000\110\000\055\000\110\000\
\\056\000\110\000\057\000\110\000\060\000\110\000\061\000\110\000\
\\062\000\110\000\063\000\110\000\000\000\
\\001\000\003\000\111\000\008\000\111\000\009\000\111\000\012\000\111\000\
\\015\000\111\000\018\000\111\000\024\000\111\000\027\000\111\000\
\\029\000\111\000\033\000\111\000\034\000\111\000\041\000\111\000\
\\045\000\111\000\049\000\111\000\050\000\111\000\051\000\111\000\
\\052\000\111\000\053\000\111\000\054\000\111\000\055\000\111\000\
\\056\000\111\000\057\000\111\000\060\000\111\000\061\000\111\000\
\\062\000\111\000\063\000\111\000\000\000\
\\001\000\003\000\112\000\008\000\112\000\009\000\112\000\012\000\112\000\
\\015\000\112\000\018\000\112\000\024\000\112\000\027\000\112\000\
\\029\000\112\000\033\000\112\000\034\000\112\000\041\000\112\000\
\\045\000\112\000\049\000\112\000\050\000\112\000\051\000\112\000\
\\052\000\112\000\053\000\112\000\054\000\112\000\055\000\112\000\
\\056\000\112\000\057\000\112\000\060\000\112\000\061\000\112\000\
\\062\000\112\000\063\000\112\000\000\000\
\\001\000\003\000\115\000\008\000\115\000\009\000\115\000\012\000\115\000\
\\015\000\115\000\018\000\115\000\024\000\115\000\027\000\115\000\
\\029\000\115\000\033\000\115\000\034\000\115\000\041\000\115\000\
\\045\000\115\000\049\000\115\000\050\000\115\000\051\000\115\000\
\\052\000\115\000\053\000\115\000\054\000\115\000\055\000\115\000\
\\056\000\115\000\057\000\115\000\060\000\115\000\061\000\115\000\
\\062\000\115\000\063\000\115\000\000\000\
\\001\000\003\000\116\000\008\000\116\000\009\000\116\000\012\000\116\000\
\\015\000\116\000\018\000\116\000\024\000\116\000\027\000\116\000\
\\029\000\116\000\033\000\116\000\034\000\116\000\041\000\116\000\
\\045\000\116\000\049\000\116\000\050\000\116\000\051\000\116\000\
\\052\000\116\000\053\000\116\000\054\000\116\000\055\000\116\000\
\\056\000\116\000\057\000\116\000\060\000\116\000\061\000\116\000\
\\062\000\116\000\063\000\116\000\000\000\
\\001\000\003\000\048\000\008\000\093\000\009\000\093\000\012\000\093\000\
\\015\000\093\000\024\000\093\000\027\000\093\000\029\000\093\000\
\\034\000\093\000\041\000\093\000\063\000\093\000\000\000\
\\001\000\003\000\048\000\008\000\094\000\009\000\094\000\012\000\094\000\
\\015\000\094\000\024\000\047\000\027\000\094\000\029\000\094\000\
\\034\000\094\000\041\000\094\000\063\000\094\000\000\000\
\\001\000\003\000\048\000\008\000\074\000\024\000\047\000\000\000\
\\001\000\003\000\048\000\009\000\113\000\024\000\047\000\041\000\113\000\000\000\
\\001\000\003\000\048\000\009\000\114\000\024\000\047\000\041\000\114\000\000\000\
\\001\000\003\000\048\000\012\000\083\000\015\000\083\000\024\000\047\000\
\\029\000\083\000\041\000\083\000\063\000\083\000\000\000\
\\001\000\003\000\048\000\012\000\084\000\015\000\084\000\024\000\047\000\
\\029\000\084\000\041\000\084\000\063\000\084\000\000\000\
\\001\000\003\000\048\000\024\000\047\000\027\000\068\000\000\000\
\\001\000\003\000\048\000\024\000\047\000\034\000\066\000\000\000\
\\001\000\009\000\073\000\041\000\072\000\000\000\
\\001\000\012\000\081\000\015\000\081\000\029\000\081\000\041\000\081\000\
\\063\000\081\000\000\000\
\\001\000\012\000\082\000\015\000\082\000\029\000\082\000\041\000\082\000\
\\063\000\082\000\000\000\
\\001\000\012\000\006\000\015\000\078\000\029\000\005\000\041\000\004\000\000\000\
\\001\000\012\000\006\000\015\000\078\000\029\000\005\000\041\000\004\000\
\\063\000\078\000\000\000\
\\001\000\012\000\006\000\029\000\005\000\041\000\004\000\063\000\078\000\000\000\
\\001\000\014\000\032\000\018\000\031\000\033\000\030\000\058\000\029\000\
\\060\000\015\000\061\000\014\000\062\000\028\000\000\000\
\\001\000\015\000\079\000\063\000\079\000\000\000\
\\001\000\015\000\080\000\063\000\080\000\000\000\
\\001\000\015\000\067\000\000\000\
\\001\000\018\000\031\000\033\000\030\000\058\000\029\000\060\000\015\000\
\\061\000\014\000\062\000\028\000\000\000\
\\001\000\018\000\031\000\033\000\030\000\060\000\015\000\061\000\014\000\
\\062\000\028\000\000\000\
\\001\000\033\000\086\000\034\000\086\000\045\000\086\000\060\000\086\000\
\\061\000\086\000\062\000\086\000\000\000\
\\001\000\033\000\087\000\034\000\087\000\045\000\087\000\060\000\087\000\
\\061\000\087\000\062\000\087\000\000\000\
\\001\000\033\000\088\000\034\000\088\000\045\000\088\000\060\000\088\000\
\\061\000\088\000\062\000\088\000\000\000\
\\001\000\033\000\089\000\045\000\089\000\060\000\089\000\061\000\089\000\
\\062\000\089\000\000\000\
\\001\000\033\000\090\000\045\000\090\000\060\000\090\000\061\000\090\000\
\\062\000\090\000\000\000\
\\001\000\033\000\016\000\045\000\035\000\060\000\015\000\061\000\014\000\
\\062\000\013\000\000\000\
\\001\000\033\000\016\000\060\000\015\000\061\000\014\000\062\000\013\000\000\000\
\\001\000\034\000\085\000\045\000\085\000\000\000\
\\001\000\034\000\033\000\000\000\
\\001\000\045\000\019\000\000\000\
\\001\000\062\000\018\000\000\000\
\\001\000\063\000\000\000\000\000\
\"
val actionRowNumbers =
"\036\000\035\000\035\000\049\000\
\\053\000\039\000\038\000\043\000\
\\050\000\052\000\032\000\044\000\
\\020\000\021\000\049\000\033\000\
\\049\000\037\000\051\000\048\000\
\\046\000\016\000\013\000\002\000\
\\000\000\027\000\017\000\042\000\
\\037\000\034\000\037\000\045\000\
\\047\000\037\000\014\000\041\000\
\\041\000\041\000\041\000\041\000\
\\041\000\041\000\041\000\041\000\
\\041\000\037\000\037\000\015\000\
\\030\000\040\000\029\000\028\000\
\\012\000\011\000\010\000\009\000\
\\008\000\006\000\005\000\004\000\
\\003\000\007\000\022\000\001\000\
\\018\000\037\000\037\000\031\000\
\\025\000\024\000\037\000\019\000\
\\037\000\026\000\023\000\054\000"
val gotoT =
"\
\\001\000\075\000\002\000\001\000\000\000\
\\001\000\005\000\002\000\001\000\000\000\
\\001\000\006\000\002\000\001\000\000\000\
\\003\000\010\000\005\000\009\000\006\000\008\000\013\000\007\000\000\000\
\\004\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\018\000\006\000\008\000\013\000\007\000\000\000\
\\000\000\
\\006\000\020\000\007\000\019\000\013\000\007\000\000\000\
\\008\000\025\000\009\000\024\000\010\000\023\000\011\000\022\000\
\\013\000\021\000\000\000\
\\000\000\
\\006\000\032\000\013\000\007\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\034\000\013\000\021\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\047\000\013\000\021\000\000\000\
\\008\000\048\000\009\000\024\000\010\000\023\000\011\000\022\000\
\\013\000\021\000\000\000\
\\001\000\049\000\002\000\001\000\000\000\
\\008\000\050\000\009\000\024\000\010\000\023\000\011\000\022\000\
\\013\000\021\000\000\000\
\\000\000\
\\000\000\
\\008\000\051\000\009\000\024\000\010\000\023\000\011\000\022\000\
\\013\000\021\000\000\000\
\\000\000\
\\009\000\052\000\010\000\023\000\011\000\022\000\013\000\021\000\000\000\
\\009\000\053\000\010\000\023\000\011\000\022\000\013\000\021\000\000\000\
\\009\000\054\000\010\000\023\000\011\000\022\000\013\000\021\000\000\000\
\\009\000\055\000\010\000\023\000\011\000\022\000\013\000\021\000\000\000\
\\009\000\056\000\010\000\023\000\011\000\022\000\013\000\021\000\000\000\
\\009\000\057\000\010\000\023\000\011\000\022\000\013\000\021\000\000\000\
\\009\000\058\000\010\000\023\000\011\000\022\000\013\000\021\000\000\000\
\\009\000\059\000\010\000\023\000\011\000\022\000\013\000\021\000\000\000\
\\009\000\060\000\010\000\023\000\011\000\022\000\013\000\021\000\000\000\
\\009\000\061\000\010\000\023\000\011\000\022\000\013\000\021\000\000\000\
\\008\000\062\000\009\000\024\000\010\000\023\000\011\000\022\000\
\\013\000\021\000\000\000\
\\008\000\063\000\009\000\024\000\010\000\023\000\011\000\022\000\
\\013\000\021\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\068\000\009\000\024\000\010\000\023\000\011\000\022\000\
\\012\000\067\000\013\000\021\000\000\000\
\\008\000\069\000\009\000\024\000\010\000\023\000\011\000\022\000\
\\013\000\021\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\073\000\009\000\024\000\010\000\023\000\011\000\022\000\
\\013\000\021\000\000\000\
\\000\000\
\\008\000\074\000\009\000\024\000\010\000\023\000\011\000\022\000\
\\013\000\021\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 76
val numrules = 39
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | ID of unit ->  (string) | INT of unit ->  (int)
 | BOOL of unit ->  (bool) | scon of unit ->  (A.t)
 | expseq of unit ->  (A.t list) | atexp of unit ->  (A.t)
 | appexp of unit ->  (A.t) | infexp of unit ->  (A.t)
 | exp of unit ->  (A.t) | atpats of unit ->  (A.t list)
 | atpat of unit ->  (A.t) | pat of unit ->  (A.t)
 | funbind of unit ->  (A.t) | valbind of unit ->  (A.t)
 | dec of unit ->  (A.t) | decs of unit ->  (A.t)
end
type svalue = MlyValue.svalue
type result = A.t
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 62) => true | _ => false
val showTerminal =
fn (T 0) => "ABSTYPE"
  | (T 1) => "AND"
  | (T 2) => "ANDALSO"
  | (T 3) => "AS"
  | (T 4) => "CASE"
  | (T 5) => "DATATYPE"
  | (T 6) => "DO"
  | (T 7) => "ELSE"
  | (T 8) => "END"
  | (T 9) => "EXCEPTION"
  | (T 10) => "FN"
  | (T 11) => "FUN"
  | (T 12) => "HANDLE"
  | (T 13) => "IF"
  | (T 14) => "IN"
  | (T 15) => "INFIX"
  | (T 16) => "INFIXR"
  | (T 17) => "LET"
  | (T 18) => "LOCAL"
  | (T 19) => "NONFIX"
  | (T 20) => "OF"
  | (T 21) => "OP"
  | (T 22) => "OPEN"
  | (T 23) => "ORELSE"
  | (T 24) => "RAISE"
  | (T 25) => "REC"
  | (T 26) => "THEN"
  | (T 27) => "TYPE"
  | (T 28) => "VAL"
  | (T 29) => "WITH"
  | (T 30) => "WITHTYPE"
  | (T 31) => "WHILE"
  | (T 32) => "LPAREN"
  | (T 33) => "RPAREN"
  | (T 34) => "LBRACKET"
  | (T 35) => "RBRACKET"
  | (T 36) => "LBRACE"
  | (T 37) => "RBRACE"
  | (T 38) => "COMMA"
  | (T 39) => "COLON"
  | (T 40) => "SEMICOLON"
  | (T 41) => "PERIODS"
  | (T 42) => "UNDERBAR"
  | (T 43) => "BAR"
  | (T 44) => "EQ"
  | (T 45) => "DARROW"
  | (T 46) => "ARROW"
  | (T 47) => "NUMBER"
  | (T 48) => "PLUS"
  | (T 49) => "MINUS"
  | (T 50) => "TIMES"
  | (T 51) => "DIV"
  | (T 52) => "NEQ"
  | (T 53) => "LT"
  | (T 54) => "LE"
  | (T 55) => "GT"
  | (T 56) => "GE"
  | (T 57) => "NOT"
  | (T 58) => "APP"
  | (T 59) => "BOOL"
  | (T 60) => "INT"
  | (T 61) => "ID"
  | (T 62) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 62) $$ (T 58) $$ (T 57) $$ (T 56) $$ (T 55) $$ (T 54) $$ (T 53)
 $$ (T 52) $$ (T 51) $$ (T 50) $$ (T 49) $$ (T 48) $$ (T 47) $$ (T 46)
 $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39)
 $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32)
 $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25)
 $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18)
 $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ 
(T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( rest671)) => let val  result = MlyValue.decs (fn _ => (
 A.Decs [] ))
 in ( LrTable.NT 0, ( result, defaultPos, defaultPos), rest671)
end
|  ( 1, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( _, 
SEMICOLON1left, _)) :: rest671)) => let val  result = MlyValue.decs
 (fn _ => let val  (decs as decs1) = decs1 ()
 in ( decs )
end)
 in ( LrTable.NT 0, ( result, SEMICOLON1left, decs1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( 
MlyValue.dec dec1, dec1left, _)) :: rest671)) => let val  result = 
MlyValue.decs (fn _ => let val  (dec as dec1) = dec1 ()
 val  (decs as decs1) = decs1 ()
 in ( case decs of A.Decs l => A.Decs (dec :: l) | xs => xs )
end)
 in ( LrTable.NT 0, ( result, dec1left, decs1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.valbind valbind1, _, valbind1right)) :: ( _,
 ( _, VAL1left, _)) :: rest671)) => let val  result = MlyValue.dec (fn
 _ => let val  (valbind as valbind1) = valbind1 ()
 in ( A.Val valbind )
end)
 in ( LrTable.NT 1, ( result, VAL1left, valbind1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.funbind funbind1, _, funbind1right)) :: ( _,
 ( _, FUN1left, _)) :: rest671)) => let val  result = MlyValue.dec (fn
 _ => let val  (funbind as funbind1) = funbind1 ()
 in ( A.Fun funbind )
end)
 in ( LrTable.NT 1, ( result, FUN1left, funbind1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.pat pat1, pat1left, _)) :: rest671)) => let val  result = 
MlyValue.valbind (fn _ => let val  (pat as pat1) = pat1 ()
 val  (exp as exp1) = exp1 ()
 in ( A.VBind (pat, exp) )
end)
 in ( LrTable.NT 2, ( result, pat1left, exp1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.atpats atpats1, _, _)) :: ( _, ( MlyValue.ID ID1, ID1left, _)
) :: rest671)) => let val  result = MlyValue.funbind (fn _ => let val 
 (ID as ID1) = ID1 ()
 val  (atpats as atpats1) = atpats1 ()
 val  (exp as exp1) = exp1 ()
 in ( A.FBind (A.Vid ID, atpats, exp) )
end)
 in ( LrTable.NT 3, ( result, ID1left, exp1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.atpat atpat1, atpat1left, atpat1right)) :: 
rest671)) => let val  result = MlyValue.pat (fn _ => let val  (atpat
 as atpat1) = atpat1 ()
 in ( atpat )
end)
 in ( LrTable.NT 4, ( result, atpat1left, atpat1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.scon scon1, scon1left, scon1right)) :: 
rest671)) => let val  result = MlyValue.atpat (fn _ => let val  (scon
 as scon1) = scon1 ()
 in ( scon )
end)
 in ( LrTable.NT 5, ( result, scon1left, scon1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.atpat (fn _ => let val  (ID as ID1) = ID1
 ()
 in ( A.Vid ID )
end)
 in ( LrTable.NT 5, ( result, ID1left, ID1right), rest671)
end
|  ( 10, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.pat pat1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.atpat (fn _ => let val  (pat as pat1) = pat1 ()
 in ( pat )
end)
 in ( LrTable.NT 5, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.atpat atpat1, atpat1left, atpat1right)) :: 
rest671)) => let val  result = MlyValue.atpats (fn _ => let val  (
atpat as atpat1) = atpat1 ()
 in ( [atpat] )
end)
 in ( LrTable.NT 6, ( result, atpat1left, atpat1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.atpat atpat1, _, atpat1right)) :: ( _, ( 
MlyValue.atpats atpats1, atpats1left, _)) :: rest671)) => let val  
result = MlyValue.atpats (fn _ => let val  (atpats as atpats1) = 
atpats1 ()
 val  (atpat as atpat1) = atpat1 ()
 in ( atpats @ [atpat] )
end)
 in ( LrTable.NT 6, ( result, atpats1left, atpat1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.infexp infexp1, infexp1left, infexp1right))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
infexp as infexp1) = infexp1 ()
 in ( infexp )
end)
 in ( LrTable.NT 7, ( result, infexp1left, infexp1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ( A.And (exp1, exp2) )
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ( A.Or (exp1, exp2) )
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: 
( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.exp
 (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in ( A.If (exp1, exp2, exp3) )
end)
 in ( LrTable.NT 7, ( result, IF1left, exp3right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.appexp appexp1, appexp1left, appexp1right))
 :: rest671)) => let val  result = MlyValue.infexp (fn _ => let val  (
appexp as appexp1) = appexp1 ()
 in ( appexp )
end)
 in ( LrTable.NT 8, ( result, appexp1left, appexp1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.infexp infexp2, _, infexp2right)) :: _ :: (
 _, ( MlyValue.infexp infexp1, infexp1left, _)) :: rest671)) => let
 val  result = MlyValue.infexp (fn _ => let val  infexp1 = infexp1 ()
 val  infexp2 = infexp2 ()
 in ( A.Add (infexp1, infexp2) )
end)
 in ( LrTable.NT 8, ( result, infexp1left, infexp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.infexp infexp2, _, infexp2right)) :: _ :: (
 _, ( MlyValue.infexp infexp1, infexp1left, _)) :: rest671)) => let
 val  result = MlyValue.infexp (fn _ => let val  infexp1 = infexp1 ()
 val  infexp2 = infexp2 ()
 in ( A.Sub (infexp1, infexp2) )
end)
 in ( LrTable.NT 8, ( result, infexp1left, infexp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.infexp infexp2, _, infexp2right)) :: _ :: (
 _, ( MlyValue.infexp infexp1, infexp1left, _)) :: rest671)) => let
 val  result = MlyValue.infexp (fn _ => let val  infexp1 = infexp1 ()
 val  infexp2 = infexp2 ()
 in ( A.Mul (infexp1, infexp2) )
end)
 in ( LrTable.NT 8, ( result, infexp1left, infexp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.infexp infexp2, _, infexp2right)) :: _ :: (
 _, ( MlyValue.infexp infexp1, infexp1left, _)) :: rest671)) => let
 val  result = MlyValue.infexp (fn _ => let val  infexp1 = infexp1 ()
 val  infexp2 = infexp2 ()
 in ( A.Div (infexp1, infexp2) )
end)
 in ( LrTable.NT 8, ( result, infexp1left, infexp2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.infexp infexp2, _, infexp2right)) :: _ :: (
 _, ( MlyValue.infexp infexp1, infexp1left, _)) :: rest671)) => let
 val  result = MlyValue.infexp (fn _ => let val  infexp1 = infexp1 ()
 val  infexp2 = infexp2 ()
 in ( A.Eq (infexp1, infexp2) )
end)
 in ( LrTable.NT 8, ( result, infexp1left, infexp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.infexp infexp2, _, infexp2right)) :: _ :: (
 _, ( MlyValue.infexp infexp1, infexp1left, _)) :: rest671)) => let
 val  result = MlyValue.infexp (fn _ => let val  infexp1 = infexp1 ()
 val  infexp2 = infexp2 ()
 in ( A.Not (A.Eq (infexp1, infexp2)) )
end)
 in ( LrTable.NT 8, ( result, infexp1left, infexp2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.infexp infexp2, _, infexp2right)) :: _ :: (
 _, ( MlyValue.infexp infexp1, infexp1left, _)) :: rest671)) => let
 val  result = MlyValue.infexp (fn _ => let val  infexp1 = infexp1 ()
 val  infexp2 = infexp2 ()
 in ( A.Not (A.LE (infexp2, infexp1)) )
end)
 in ( LrTable.NT 8, ( result, infexp1left, infexp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.infexp infexp2, _, infexp2right)) :: _ :: (
 _, ( MlyValue.infexp infexp1, infexp1left, _)) :: rest671)) => let
 val  result = MlyValue.infexp (fn _ => let val  infexp1 = infexp1 ()
 val  infexp2 = infexp2 ()
 in ( A.LE (infexp1, infexp2) )
end)
 in ( LrTable.NT 8, ( result, infexp1left, infexp2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.infexp infexp2, _, infexp2right)) :: _ :: (
 _, ( MlyValue.infexp infexp1, infexp1left, _)) :: rest671)) => let
 val  result = MlyValue.infexp (fn _ => let val  infexp1 = infexp1 ()
 val  infexp2 = infexp2 ()
 in ( A.Not (A.LE (infexp1, infexp2)) )
end)
 in ( LrTable.NT 8, ( result, infexp1left, infexp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.infexp infexp2, _, infexp2right)) :: _ :: (
 _, ( MlyValue.infexp infexp1, infexp1left, _)) :: rest671)) => let
 val  result = MlyValue.infexp (fn _ => let val  infexp1 = infexp1 ()
 val  infexp2 = infexp2 ()
 in ( A.LE (infexp2, infexp1) )
end)
 in ( LrTable.NT 8, ( result, infexp1left, infexp2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.atexp atexp1, atexp1left, atexp1right)) :: 
rest671)) => let val  result = MlyValue.appexp (fn _ => let val  (
atexp as atexp1) = atexp1 ()
 in ( atexp )
end)
 in ( LrTable.NT 9, ( result, atexp1left, atexp1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.atexp atexp1, _, atexp1right)) :: ( _, ( 
MlyValue.appexp appexp1, appexp1left, _)) :: rest671)) => let val  
result = MlyValue.appexp (fn _ => let val  (appexp as appexp1) = 
appexp1 ()
 val  (atexp as atexp1) = atexp1 ()
 in ( A.App (appexp, atexp) )
end)
 in ( LrTable.NT 9, ( result, appexp1left, atexp1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.atexp atexp1, _, atexp1right)) :: ( _, ( _,
 NOT1left, _)) :: rest671)) => let val  result = MlyValue.appexp (fn _
 => let val  (atexp as atexp1) = atexp1 ()
 in ( A.Not atexp )
end)
 in ( LrTable.NT 9, ( result, NOT1left, atexp1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.scon scon1, scon1left, scon1right)) :: 
rest671)) => let val  result = MlyValue.atexp (fn _ => let val  (scon
 as scon1) = scon1 ()
 in ( scon )
end)
 in ( LrTable.NT 10, ( result, scon1left, scon1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.atexp (fn _ => let val  (ID as ID1) = ID1
 ()
 in ( A.Vid ID )
end)
 in ( LrTable.NT 10, ( result, ID1left, ID1right), rest671)
end
|  ( 33, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.atexp (fn _ => let val  (exp as exp1) = exp1 ()
 in ( exp )
end)
 in ( LrTable.NT 10, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 34, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.expseq expseq1,
 _, _)) :: _ :: ( _, ( MlyValue.decs decs1, _, _)) :: ( _, ( _, 
LET1left, _)) :: rest671)) => let val  result = MlyValue.atexp (fn _
 => let val  (decs as decs1) = decs1 ()
 val  (expseq as expseq1) = expseq1 ()
 in ( A.Let (decs, expseq) )
end)
 in ( LrTable.NT 10, ( result, LET1left, END1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.expseq (fn _ => let val  (exp as exp1)
 = exp1 ()
 in ( [exp] )
end)
 in ( LrTable.NT 11, ( result, exp1left, exp1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.expseq expseq1, expseq1left, _)) :: rest671)) => let val  
result = MlyValue.expseq (fn _ => let val  (expseq as expseq1) = 
expseq1 ()
 val  (exp as exp1) = exp1 ()
 in ( expseq @ [exp] )
end)
 in ( LrTable.NT 11, ( result, expseq1left, exp1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.scon (fn _ => let val  (INT as INT1) =
 INT1 ()
 in ( A.Int INT )
end)
 in ( LrTable.NT 12, ( result, INT1left, INT1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.BOOL BOOL1, BOOL1left, BOOL1right)) :: 
rest671)) => let val  result = MlyValue.scon (fn _ => let val  (BOOL
 as BOOL1) = BOOL1 ()
 in ( A.Bool BOOL )
end)
 in ( LrTable.NT 12, ( result, BOOL1left, BOOL1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.decs x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : MeiLou_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ABSTYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun ANDALSO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun AS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun CASE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun DATATYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun EXCEPTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun HANDLE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun INFIX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun INFIXR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun LOCAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NONFIX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun OP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun OPEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun ORELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun RAISE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun REC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun WITH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun WITHTYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun PERIODS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun UNDERBAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun BAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun DARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun NUMBER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 51,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 52,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 53,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 54,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 55,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 56,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 57,(
ParserData.MlyValue.VOID,p1,p2))
fun APP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 58,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 59,(
ParserData.MlyValue.BOOL (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 60,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 61,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 62,(
ParserData.MlyValue.VOID,p1,p2))
end
end
