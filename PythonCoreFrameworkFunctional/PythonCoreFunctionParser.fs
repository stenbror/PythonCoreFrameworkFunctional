namespace PythonCoreFrameworkFunctional

open PythonCoreFrameworkFunctional.ParserUtilities

module PythonCoreFunctionParser =
    
    let rec ParseFuncType(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |  Some(Token.PyLeftParen( _ , _ , _ ), rest ) ->
            let op1 = List.head stream
            let left, rest3 =   match TryToken rest with
                                |   Some(Token.PyRightParen( _ , _ , _ ), rest2 ) ->   ASTNode.Empty, rest
                                |   _ ->  ParseTypeList rest
            let op2, rest4 =    match TryToken rest3 with
                                |   Some(Token.PyRightParen( _ , _ , _ ), rest5 ) ->
                                       List.head rest3, rest5
                                |   _ ->   raise (SyntaxError(GetStartPosition rest3, "Missing ')' in function definition!"))
            let op3, rest6 =    match TryToken rest4 with
                                |   Some(Token.PyArrow( _ , _ , _ ), rest7 ) ->
                                        List.head rest4, rest7
                                |   _ ->   raise (SyntaxError(GetStartPosition rest4, "Expecting '->' in function definition!"))
            let right, rest8 =  PythonCoreExpressionParser.ParseTest rest6
            ASTNode.FuncType(spanStart, GetStartPosition rest8, op1, left, op2, op3, right), rest8
        |  _ -> raise (SyntaxError(GetStartPosition stream, "Expecting '(' in function type!"))
        
    and ParseTypeList(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        ASTNode.Empty, stream
