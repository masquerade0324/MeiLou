# MeiLou

MeiLou (明朗), that means bright and cheerful in Japanese, is a toy compiler for the minimal subset of Standard ML.

I'll implement this compiler with reference to "[MinCaml](http://esumii.github.io/min-caml/)" and "Modern Compiler Implementation in ML [Andrew W. Appel]".

# Grammar

MeiLou grammar is the minimal subset of Standard ML full grammar, see "[The Definition of Standard ML (Revised)](https://mitpress.mit.edu/books/definition-standard-ml-revised-edition)".

```
<prog> ::= <dec>

<dec> ::= "val" <valbind>
        | "fun" <funbind>
        |
        | <dec1> ";"? <dec2>

<valbind> ::= <pat> "=" <exp>

<pat> ::= <atpat>

<atpat> ::= <scon>
          | <vid>
          | "(" <pat> ")"

<exp> ::= <infexp>
        | <exp1> "andalso" <exp2>
        | <exp1> "orelse" <exp2>
        | "if" <exp1> "then" <exp2> "else" <exp3>

<infexp> ::= <appexp>
           | <infexp1> "+" <infexp2>
           | <infexp1> "-" <infexp2>
           | <infexp1> "*" <infexp2>
           | <infexp1> "div" <infexp2>
           | <infexp1> "<" <infexp2>
           | <infexp1> "<=" <infexp2>
           | <infexp1> ">" <infexp2>
           | <infexp1> ">=" <infexp2>

<appexp> ::= <atexp>
           | <appexp> <atexp>
           | "not" <atexp>

<atexp> ::= <scon>
          | <vid>
          | "(" <exp> ")"
          | "let" <dec> "in" <exp> (";" <exp>)* "end"

<scon> ::= <int>
         | <bool>
```
