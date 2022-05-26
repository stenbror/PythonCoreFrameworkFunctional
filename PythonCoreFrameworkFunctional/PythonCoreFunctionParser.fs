namespace PythonCoreFrameworkFunctional

open PythonCoreFrameworkFunctional.ParserUtilities

module PythonCoreFunctionParser =
    
    let rec ParseFuncType(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |  Some(Token.PyLeftParen( _ , _ , _ ), rest ) ->
            ASTNode.Empty, stream
        |  _ -> raise (SyntaxError(GetStartPosition stream, "Expecting '(' in function type!"))
        
    and ParseTypeList(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        ASTNode.Empty, stream
