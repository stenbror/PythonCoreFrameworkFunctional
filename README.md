# PythonCoreFrameworkFunctional

For now this only support parsing og EvalInput start rule for expression handling in Python 3.10 grammar.

Example:

let node = "a and b or c" |> PythonCoreTokenizer.TokenizeFromString |> PythonCoreParser.ParseEvalInput

Replace code in string with ant expressions in Python 3.10 and see the annotated syntax tree with token and trivia.
