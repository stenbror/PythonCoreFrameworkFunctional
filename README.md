# PythonCoreFrameworkFunctional

For now this only support parsing from EvalInput start rule for expression handling in Python 3.10 grammar.

Example:

let node = "a and b or c" |> PythonCoreTokenizer.TokenizeFromString |> PythonCoreParser.ParseEvalInput

Replace code in string with any expressions in Python 3.10 and see the annotated syntax tree with token and trivia.

It will support the whole Python 3.10 grammar with statements and 'match' statement added in 3.10 eventually.
