structure Absyn =
struct
  datatype t = Int of int
             | Bool of bool
             | Vid of string
             | Let of t * t list
             | App of t * t
             | Not of t
             | Add of t * t
             | Sub of t * t
             | Mul of t * t
             | Div of t * t
             | Eq of t * t
             | LE of t * t
             | And of t * t
             | Or of t * t
             | If of t * t * t
             | VBind of t * t
             | FBind of t * t list * t
             | Val of t
             | Fun of t
             | Decs of t list

  fun toString (Int i)              = "Int " ^ Int.toString i
    | toString (Bool b)             = "Bool " ^ Bool.toString b
    | toString (Vid s)              = "Vid (" ^ s ^ ")"
    | toString (Let (t, ts))        = "Let (" ^ toString t ^ ", [" ^ ( String.concatWith ", " (map toString ts)) ^ "])"
    | toString (App (t1, t2))       = "App (" ^ toString t1 ^ ", " ^ toString t2 ^ ")"
    | toString (Not t)              = "Not " ^ toString t
    | toString (Add (t1, t2))       = "Add (" ^ toString t1 ^ ", " ^ toString t2 ^ ")"
    | toString (Sub (t1, t2))       = "Sub (" ^ toString t1 ^ ", " ^ toString t2 ^ ")"
    | toString (Mul (t1, t2))       = "Mul (" ^ toString t1 ^ ", " ^ toString t2 ^ ")"
    | toString (Div (t1, t2))       = "Div (" ^ toString t1 ^ ", " ^ toString t2 ^ ")"
    | toString (Eq (t1, t2))        = "Eq (" ^ toString t1 ^ ", " ^ toString t2 ^ ")"
    | toString (LE (t1, t2))        = "LE (" ^ toString t1 ^ ", " ^ toString t2 ^ ")"
    | toString (And (t1, t2))       = "And (" ^ toString t1 ^ ", " ^ toString t2 ^ ")"
    | toString (Or (t1, t2))        = "Or (" ^ toString t1 ^ ", " ^ toString t2 ^ ")"
    | toString (If (t1, t2, t3))    = "If (" ^ toString t1 ^ ", " ^ toString t2 ^ ", " ^ toString t3 ^ ")"
    | toString (VBind (t1, t2))     = "VBind (" ^ toString t1 ^ ", " ^ toString t2 ^ ")"
    | toString (FBind (t1, ts, t2)) = "FBind (" ^ toString t1 ^ ", [" ^ String.concatWith ", " (map toString ts) ^ "], " ^ toString t2 ^ ")"
    | toString (Val t)              = "Val (" ^ toString t ^ ")"
    | toString (Fun t)              = "Fun (" ^ toString t ^ ")"
    | toString (Decs ts)            = "Decs ([" ^ (String.concatWith ", " (map toString ts)) ^ "])"

  fun println ast = print (toString ast ^ "\n")
end
