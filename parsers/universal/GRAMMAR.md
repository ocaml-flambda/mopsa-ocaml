# Universal Grammar

This file reports the grammar for the Universal language.
A Universal program is a block of code preceded by a sequence of variable/function declarations.
```haskell
<program>     ::= <declaration>* <block>

<declaration> ::= <vardecl>
                | <fundecl>

<vardecl>     ::= <type> <id>;
                | <type> <id> = <expr>;

<fundecl>     ::= <type> <id>(<type> <id>, ...) { <block> };

<type>        ::= "int"
                | "real"
                | "string"

<block>       ::= <stmt>*

<stmt>        ::= <lval> = <expr>;
                |  "if" (<expr>) { <block> }
                |  "if" (<expr>) { <block> } "else" { <block> }
                |  "while" (<expr>) { <block> }
                |  "break";
                |  "continue";
                |  "return" <expr>;
                |  "print" ();

<expr>         ::= <int>                    -- Integer number
                 | <real>                   -- Real number
                 | "true"                   -- True constant
                 | "false"                  -- False constant
                 | <id>                     -- Variable access
                 | <unop> <expr>            -- Unary expression
                 | <expr> <binop> <expr>    -- Binary expression
                 | "rand"(<int>, <int>)     -- Random integer
                 | "randf"(<real>, <real>)  -- Random real
                 | "rand"                   -- Random string
                 | <id>(<expr>, ...)        -- Function call
                 | |<expr>|                 -- String length
                 | ()                       -- Void constructor

<unop>         ::= "+" | "-" | "!"
<binop>        ::= "+" | "-" | "*" | "/" | "<" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "@"
```

The following is an example of a valid Universal program:
```c
int x = 0;
int y; // Variables can be uninitialized.

// This is a function that computes the successor of an integer.
int successor(int n) {
    return n + 1;
}

// Floating points numbers have type `real`.
real f = 0.1;
real square(real to_square) {
    return to_square * to_square;
}

// Strings have type `string`.
string s1 = "a";
string s2 = "bb";

// The declarations are over, and we have the main code block.

x = successor(x);    // Variables can be set to arbitrary expressions.
f = randf(0.1, 0.2); // `randf(f1, f2)` returns a random real number in the range [f1,f2].
s1 = rand();         // Creates a random string
s1 = s2 @ s2;        // Strings can be concatenated with the `@` and `+` operators.
x = |s1|;            // The |str| operator returns the length of strings.
y = true;            // Boolean constants are converted to integers. `true` is 1 and `false` is 0.
print();             // `print()` makes the analyzer print the abstract state.
```
