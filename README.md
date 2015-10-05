An Untyped Lambda Calculus interpreter. The syntax of the language is:

<form> = <assign> | <term>

<assign> = <identifier> "=" <term>
<term>   = <parenterm> | <variable> | <lambda> | <application>

<parenterm>   = "(" <term> ")"
<atom>        = <identifier>
<lambda>      = "\" <identifier> "." <term>
<application> = <term> <term>

<identifier> = <character> { <character> }
<character>  = <letter> | <digit> | <symbol>
<symbol>     = "~" | "!" | "@" | "#" | "$" | "%" | "^" | "&" | "*" | "-" | "_" | "+"
             | "|" | ";" | ":" | "'" | "," | "/" | "?" | "[" | "]" | "<" | ">" 

Some example forms include
id = \x . x
true = \x . \y . x
false = \x . \y . y
if = \c . \t . \f . ((c t) f)

The interpreter also includes syntactic sugar to simplify some forms.

Nested lambdas can be written as
true = \x y . x
false = \x y . y
if = \c t f . ((c t) f)

Application is left associative, so 'if' can be written as
if = \c t f . c t f

Assignment can be written in the style of a function definition, like
if c t f = c t f
which desugars back to
if = \c t f . c t f

Finally, the interpeter has a single directive for importing a file
:i <filename>
which executes each line in the file sequentially.
