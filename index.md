---
layout: default
title: Source code for Implementation of Programming Languages course
---
# Source code for Implementation of Programming Languages course

The [Toxaris/pl-impl](http://github.com/Toxaris/pl-impl) repository on GitHub contains the source code relevant for the
"Implementation of Programming Languages" course at Tübingen
University. It is (or will be when finished) an implementation of
a simple programming language.
More information is available in the
[API documentation](http://toxaris.github.io/pl-impl/api)

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
