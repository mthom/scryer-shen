#lang brag

term   : clause | variable | atom | number | list | string

clause : atom /"(" term (/"," term)* /")"

atom   : ATOM

list   : /"[" term (/"," term)* /"]" | /"[" term (/"," term)* BAR term /"]" | "'.'" /"(" term /"," term /")"

string : STRING

number : NUMBER

variable : VARIABLE
