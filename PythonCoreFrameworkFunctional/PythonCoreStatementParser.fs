namespace PythonCoreFrameworkFunctional

open PythonCoreFrameworkFunctional.ParserUtilities

module PythonCoreStatementParser =
    
    let rec ParseStmt(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        match TryToken stream with
        |   Some(Token.Name( _ , _ , "match" , _ ), _ )
        |   Some(Token.PyIf( _ , _ , _ ), _ ) 
        |   Some(Token.PyWhile( _ , _ , _ ), _ )
        |   Some(Token.PyFor( _ , _ , _ ), _ )
        |   Some(Token.PyWith( _ , _ , _ ), _ ) 
        |   Some(Token.PyTry( _ , _ , _ ), _ ) 
        |   Some(Token.PyClass( _ , _ , _ ), _ )
        |   Some(Token.PyDef( _ , _ , _ ), _ )
        |   Some(Token.PyAsync( _ , _ , _ ), _ )
        |   Some(Token.PyMatrice( _ , _ , _ ), _ ) ->    ParseCompound (stream, flows)
        |   _ ->   ParseSimpleStmt (stream, flows)
        
    and ParseSimpleStmt(stream: TokenStream, lows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)

    and ParseSmallStmt(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseExpr(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseDel(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |   Some(Token.PyDel( _ , _ , _ ), rest ) ->
                let op = List.head stream
                let node, rest2 = PythonCoreExpressionParser.ParseExprList rest
                ASTNode.DelStmt(spanStart, GetStartPosition rest2, op, node), rest2
        |   _ ->    raise (SyntaxError(GetStartPosition stream, "Expecting 'del' in del statement!"))
        
    and ParsePass(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseFlow(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseBreak(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseContinue(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseReturn(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseYield(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseRaise(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseImport(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseImportName(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseImportFrom(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseImportAsName(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseDottedAsName(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseImportAsNames(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseDottedAsNames(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseDottedName(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseGlobal(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseNonLocal(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseAssert(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseCompound(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        match TryToken stream with
        |   Some(Token.PyIf( _ , _ , _ ), _ ) ->    ParseIf (stream, flows)
        |   Some(Token.PyWhile( _ , _ , _ ), _ ) -> ParseWhile (stream, flows)
        |   Some(Token.PyFor( _ , _ , _ ), _ ) ->   ParseFor (stream, flows)
        |   Some(Token.PyWith( _ , _ , _ ), _ ) ->  ParseWith (stream, flows)
        |   Some(Token.PyTry( _ , _ , _ ), _ ) ->   ParseTry (stream, flows)
        |   Some(Token.PyClass( _ , _ , _ ), _ ) -> ParseClass (stream, flows)
        |   Some(Token.PyDef( _ , _ , _ ), _ ) ->   ParseFuncDef (stream, flows)
        |   Some(Token.PyAsync( _ , _ , _ ), _ ) -> ParseAsync (stream, flows)
        |   Some(Token.PyMatrice( _ , _ , _ ), _ ) -> ParseDecorated (stream, flows)
        |   Some(Token.Name( _ , _ , "match" , _), _ ) ->
                PythonCoreMatchParser.ParseMatch (stream, flows)
        |   _ ->    raise (SyntaxError(GetStartPosition stream, "Expecting compound statement!"))
        
    and ParseIf(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseElif(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseElse(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseWhile(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseFor(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseTry(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseWith(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseWithItem(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseAsync(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseSuiteExcept(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseFuncSuite(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseDecorator(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseDecorators(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseDecorated(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseAsyncFuncDef(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseFuncDef(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
        
    and ParseParameters(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseTypedArgsList(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseTFPDef(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseClass(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
