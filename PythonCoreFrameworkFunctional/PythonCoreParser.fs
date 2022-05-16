namespace PythonCoreFrameworkFunctional

open PythonCoreFrameworkFunctional.ParserUtilities

module PythonCoreParser =
    
    let ParseEvalInput(stream: TokenStream) : ASTNode =
        let spanStart = GetStartPosition stream
        let node, rest = PythonCoreExpressionParser.ParseTestList stream
        let mutable newlines : Token List = List.Empty
        let mutable rest2 = rest
        while   match TryToken rest2 with
                |  Some(Token.Newline( _ , _ , _ , _ , _ ), rest3) ->
                      newlines <- List.head rest2 :: newlines
                      rest2 <- rest3
                      true
                |  _ -> false
            do ()
        match TryToken rest2 with
        |  Some(Token.EOF( _ ), _ ) -> 
              ASTNode.EvalInput(spanStart, GetStartPosition rest2, node, List.toArray(List.rev newlines), List.head rest2)
        |  _ ->  raise (SyntaxError(GetStartPosition rest2, "Expecting End of file!"))
        
    