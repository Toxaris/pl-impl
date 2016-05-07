# Source code for Implementation of Programming Languages course

This repository contains the source code relevant for the
"Implementation of Programming Languages" course at Tübingen
University. It is (or will be when finished) an implementation of
a simple programming language.

## Object language

The object language (the language that we implement here) is a
small subset of Scala. Currently:

|           |                                                                      |
|  :-----:  |  :------------------------------------------------------------------ |
|  *P* ::=  |  `"object"` *N* `"extends"` *N* `"{"` *S* * `"}"`                    |
|  *S* ::=  |  `"var"` *N* `"="` *E* `";"`                                         |
|      \|   |  *N* `"="` *E* `";"`                                                 |
|      \|   |  `"print"` `"("` *E* `")"` `";"`                                     |
|      \|   |  `"while"` `"("` *E* `")"` `"{"` *S* * `"}"`                         |
|      \|   |  `"if"` `"("` *E* `")"` `"{"` *S* * `"}"`                            |
|      \|   |  `"if"` `"("` *E* `")"` `"{"` *S* * `"}"` `"else"` `"{"` *S* * `"}"` |
|      \|   |  `"{"` S * `"}"`                                                     |
|  *E* ::=  |  *N*                                                                 |
|      \|   |  *L*                                                                 |
|      \|   |  *E* `"+"` *E*                                                       |
|      \|   |  *E* `"-"` *E*                                                       |
|      \|   |  *E* `"*"` *E*                                                       |
|  *N* ::=  |  *variable names*                                                    |
|  *L* ::=  |  *integer literals*                                                  |
