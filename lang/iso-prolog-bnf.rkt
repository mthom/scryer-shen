#lang brag

term   : clause | variable | atom | number | list | string

clause : atom /"(" term (/"," term)* /")"

list   : /"[" term (/"," term)* /"]" | /"[" term (/"," term)* BAR term /"]" | "'.'" /"(" term /"," term /")"

atom   : ATOM

string : STRING

number : NUMBER

variable : VARIABLE
