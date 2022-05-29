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
        
    let ParseFuncTypeInput(stream: TokenStream) : ASTNode =
        let spanStart = GetStartPosition stream
        let left, rest = PythonCoreFunctionParser.ParseFuncType stream
        let mutable newlines : Token List = List.Empty
        let mutable restAgain = rest
        while   match TryToken restAgain with
                |  Some(Token.Newline( _ , _ , _ , _ , _ ), rest2 ) ->
                    newlines <- List.head restAgain :: newlines
                    restAgain <- rest2
                    true
                |  _ -> false
           do ()
        match TryToken restAgain with
        |   Some(Token.EOF( _ ), _ ) ->
                ASTNode.FuncTypeInput(spanStart, GetStartPosition restAgain, left, List.toArray(List.rev newlines), List.head restAgain)
        |  _ ->  raise (SyntaxError(GetStartPosition restAgain, "Expecting end of file!"))
        
    let ParseFileInput(stream: TokenStream) : ASTNode =
        let spanStart = GetStartPosition stream
        let mutable nodes : ASTNode List = List.Empty
        let mutable newlines : Token List = List.Empty
        let mutable restAgain = stream
        let mutable flows : (uint * uint) = (0u, 0u)
        
        while   match TryToken restAgain with
                |   Some(Token.Newline( _ , _ , _ , _ , _ ), rest) ->
                        newlines <- List.head restAgain :: newlines
                        restAgain <- rest
                        true
                |   Some(Token.EOF( _ ), _ ) -> false
                |   _ ->
                        let res, rest2, flows2 = PythonCoreStatementParser.ParseStmt(restAgain, flows)
                        nodes <- res :: nodes
                        restAgain <- rest2
                        flows <- flows2
                        true
            do ()
        
        match TryToken restAgain with
        |   Some(Token.EOF( _ ), _ ) ->
                ASTNode.FileInput(spanStart, GetStartPosition restAgain,
                                  List.toArray(List.rev nodes), List.toArray(List.rev newlines), List.head restAgain)
        |  _ ->  raise (SyntaxError(GetStartPosition restAgain, "Expecting end of file!"))
        
    let ParseSingleInput(stream: TokenStream) : ASTNode =
        ASTNode.Empty
           