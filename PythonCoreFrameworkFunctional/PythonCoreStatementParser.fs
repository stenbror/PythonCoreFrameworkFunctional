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
        
    and ParseSimpleStmt(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        let spanStart = GetStartPosition stream
        let mutable nodes : ASTNode List = List.Empty
        let mutable separeators : Token List = List.Empty
        let mutable restAgain = stream
        let mutable flowsAgain = flows
        
        let node, rest5, flows2 = ParseSmallStmt(restAgain, flowsAgain)
        nodes <- node :: nodes
        restAgain <- rest5
        flowsAgain <- flows2
        
        while   match TryToken restAgain with
                |   Some(Token.PySemiColon( _ , _ , _ ), rest6 ) ->
                        separeators <- List.head restAgain :: separeators
                        match TryToken rest6 with
                        |   Some(Token.Newline( _ , _ , _ , _ , _ ), _ ) -> false
                        |   _ ->
                                let node, rest7, flows3 = ParseSmallStmt(rest6, flowsAgain)
                                nodes <- node :: nodes
                                restAgain <- rest7
                                flowsAgain <- flows3
                                true
                |   _ -> false
            do ()
        
        let op, rest3 =     match TryToken restAgain with
                            |   Some(Token.Newline( _ , _ , _ , _ , _ ), rest4 ) -> List.head restAgain, rest4
                            |   _ ->    raise (SyntaxError(GetStartPosition restAgain, "Expecting Newline!"))
        restAgain <- rest3
        
        ASTNode.SimpleStmt(spanStart, GetStartPosition restAgain, List.toArray(List.rev nodes),
                           List.toArray(List.rev separeators), op), restAgain, flowsAgain

    and ParseSmallStmt(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        match TryToken stream with
        |   Some(Token.PyDel( _ , _ , _ ), _ ) ->
                let node, rest = ParseDel stream
                node, rest, flows
        |   Some(Token.PyPass( _ , _ , _ ), _ ) ->
                let node, rest = ParsePass stream
                node, rest, flows
        |   Some(Token.PyBreak( _ , _ , _ ), _ )
        |   Some(Token.PyContinue( _ , _ , _ ), _ )
        |   Some(Token.PyRaise( _ , _ , _ ), _ )
        |   Some(Token.PyYield( _ , _ , _ ), _ )
        |   Some(Token.PyReturn( _ , _ , _ ), _ ) ->
                ParseFlow(stream, flows)
        |   Some(Token.PyFrom( _ , _ , _ ), _ )
        |   Some(Token.PyImport( _ , _ , _ ), _ ) ->
                let node, rest = ParseImport stream
                node, rest, flows
        |   Some(Token.PyGlobal( _ , _ , _ ), _ ) ->
                let node, rest = ParseGlobal stream
                node, rest, flows
        |   Some(Token.PyNonLocal( _ , _ , _ ), _ ) ->
                let node, rest = ParseNonLocal stream
                node, rest, flows
        |   Some(Token.PyAssert( _ , _ , _ ), _ ) ->
                let node, rest = ParseAssert stream
                node, rest, flows
        |   _ ->
                let node, rest = ParseExpr stream
                node, rest, flows
        
    and ParseExpr(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        let left, rest1 = PythonCoreExpressionParser.ParseTestListStarExpr stream
        match TryToken rest1 with
        |   Some(Token.PyPlusAssign( _ , _ , _ ), rest2) ->
                let op = List.head rest1
                match TryToken rest2 with
                |   Some(Token.PyYield( _ , _ , _ ), _ ) ->
                        let right, rest4 = PythonCoreExpressionParser.ParseYieldExpr rest2
                        ASTNode.PlusAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
                |   _ ->
                        let right, rest4 = PythonCoreExpressionParser.ParseTestList rest2
                        ASTNode.PlusAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
        |   Some(Token.PyMinusAssign( _ , _ , _ ), rest2) ->
                let op = List.head rest1
                match TryToken rest2 with
                |   Some(Token.PyYield( _ , _ , _ ), _ ) ->
                        let right, rest4 = PythonCoreExpressionParser.ParseYieldExpr rest2
                        ASTNode.MinusAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
                |   _ ->
                        let right, rest4 = PythonCoreExpressionParser.ParseTestList rest2
                        ASTNode.MinusAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
        |   Some(Token.PyMulAssign( _ , _ , _ ), rest2) ->
                let op = List.head rest1
                match TryToken rest2 with
                |   Some(Token.PyYield( _ , _ , _ ), _ ) ->
                        let right, rest4 = PythonCoreExpressionParser.ParseYieldExpr rest2
                        ASTNode.MulAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
                |   _ ->
                        let right, rest4 = PythonCoreExpressionParser.ParseTestList rest2
                        ASTNode.MulAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
        |   Some(Token.PyPowerAssign( _ , _ , _ ), rest2) ->
                let op = List.head rest1
                match TryToken rest2 with
                |   Some(Token.PyYield( _ , _ , _ ), _ ) ->
                        let right, rest4 = PythonCoreExpressionParser.ParseYieldExpr rest2
                        ASTNode.PowerAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
                |   _ ->
                        let right, rest4 = PythonCoreExpressionParser.ParseTestList rest2
                        ASTNode.PowerAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
        |   Some(Token.PyDivAssign( _ , _ , _ ), rest2) ->
                let op = List.head rest1
                match TryToken rest2 with
                |   Some(Token.PyYield( _ , _ , _ ), _ ) ->
                        let right, rest4 = PythonCoreExpressionParser.ParseYieldExpr rest2
                        ASTNode.DivAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
                |   _ ->
                        let right, rest4 = PythonCoreExpressionParser.ParseTestList rest2
                        ASTNode.DivAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
        |   Some(Token.PyFloorDivAssign( _ , _ , _ ), rest2) ->
                let op = List.head rest1
                match TryToken rest2 with
                |   Some(Token.PyYield( _ , _ , _ ), _ ) ->
                        let right, rest4 = PythonCoreExpressionParser.ParseYieldExpr rest2
                        ASTNode.FloorDivAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
                |   _ ->
                        let right, rest4 = PythonCoreExpressionParser.ParseTestList rest2
                        ASTNode.FloorDivAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
        |   Some(Token.PyModuloAssign( _ , _ , _ ), rest2) ->
                let op = List.head rest1
                match TryToken rest2 with
                |   Some(Token.PyYield( _ , _ , _ ), _ ) ->
                        let right, rest4 = PythonCoreExpressionParser.ParseYieldExpr rest2
                        ASTNode.ModuloAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
                |   _ ->
                        let right, rest4 = PythonCoreExpressionParser.ParseTestList rest2
                        ASTNode.ModuloAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
        |   Some(Token.PyMatriceAssign( _ , _ , _ ), rest2) ->
                let op = List.head rest1
                match TryToken rest2 with
                |   Some(Token.PyYield( _ , _ , _ ), _ ) ->
                        let right, rest4 = PythonCoreExpressionParser.ParseYieldExpr rest2
                        ASTNode.MatriceAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
                |   _ ->
                        let right, rest4 = PythonCoreExpressionParser.ParseTestList rest2
                        ASTNode.MatriceAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
        |   Some(Token.PyShiftLeftAssign( _ , _ , _ ), rest2) ->
                let op = List.head rest1
                match TryToken rest2 with
                |   Some(Token.PyYield( _ , _ , _ ), _ ) ->
                        let right, rest4 = PythonCoreExpressionParser.ParseYieldExpr rest2
                        ASTNode.ShiftLeftAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
                |   _ ->
                        let right, rest4 = PythonCoreExpressionParser.ParseTestList rest2
                        ASTNode.ShiftLeftAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
        |   Some(Token.PyShiftRightAssign( _ , _ , _ ), rest2) ->
                let op = List.head rest1
                match TryToken rest2 with
                |   Some(Token.PyYield( _ , _ , _ ), _ ) ->
                        let right, rest4 = PythonCoreExpressionParser.ParseYieldExpr rest2
                        ASTNode.ShiftRightAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
                |   _ ->
                        let right, rest4 = PythonCoreExpressionParser.ParseTestList rest2
                        ASTNode.ShiftRightAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
        |   Some(Token.PyBitAndAssign( _ , _ , _ ), rest2) ->
                let op = List.head rest1
                match TryToken rest2 with
                |   Some(Token.PyYield( _ , _ , _ ), _ ) ->
                        let right, rest4 = PythonCoreExpressionParser.ParseYieldExpr rest2
                        ASTNode.BitAndAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
                |   _ ->
                        let right, rest4 = PythonCoreExpressionParser.ParseTestList rest2
                        ASTNode.BitAndAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
        |   Some(Token.PyBitOrAssign( _ , _ , _ ), rest2) ->
                let op = List.head rest1
                match TryToken rest2 with
                |   Some(Token.PyYield( _ , _ , _ ), _ ) ->
                        let right, rest4 = PythonCoreExpressionParser.ParseYieldExpr rest2
                        ASTNode.BitOrAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
                |   _ ->
                        let right, rest4 = PythonCoreExpressionParser.ParseTestList rest2
                        ASTNode.BitOrAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
        |   Some(Token.PyBitXorAssign( _ , _ , _ ), rest2) ->
                let op = List.head rest1
                match TryToken rest2 with
                |   Some(Token.PyYield( _ , _ , _ ), _ ) ->
                        let right, rest4 = PythonCoreExpressionParser.ParseYieldExpr rest2
                        ASTNode.BitXorAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
                |   _ ->
                        let right, rest4 = PythonCoreExpressionParser.ParseTestList rest2
                        ASTNode.BitXorAssign(spanStart, GetStartPosition rest4, left, op, right), rest4
        |   Some(Token.PyColon( _ , _ , _ ), rest2) ->
                let op = List.head rest1
                let right, rest3 = PythonCoreExpressionParser.ParseTest rest2
                let resLeft = ASTNode.Annotation(spanStart, GetStartPosition rest3, left, op, right)
                match TryToken rest3 with
                |   Some(Token.PyAssign( _ , _ , _ ), rest4) ->
                        let op2 = List.head rest3
                        match TryToken rest4 with
                        |   Some(Token.PyYield( _ , _ , _ ), _ ) ->
                                let right, rest5 = PythonCoreExpressionParser.ParseYieldExpr rest4
                                ASTNode.Assign(spanStart, GetStartPosition rest5, resLeft, op, right, Token.Empty), rest5
                        |   _ ->
                                let right, rest5 = PythonCoreExpressionParser.ParseTestListStarExpr rest4
                                ASTNode.Assign(spanStart, GetStartPosition rest5, resLeft, op, right, Token.Empty), rest5       
                |   _ ->    resLeft, rest3
        |   Some(Token.PyAssign( _ , _ , _ ), _ ) ->
                let mutable res = left
                let mutable restAgain = rest1
                while   match TryToken restAgain with
                        |   Some(Token.PyAssign( _ , _ , _ ), rest6) ->
                                let op2 = List.head restAgain
                                match TryToken rest6 with
                                |   Some(Token.PyYield( _ , _ , _ ), rest7 ) ->
                                        let right, rest5 = PythonCoreExpressionParser.ParseYieldExpr rest7
                                        match TryToken rest5 with
                                        |   Some(Token.TypeComment( _ , _ , _ , _ ), rest6) ->
                                                let op3 = List.head rest5
                                                res <- ASTNode.Assign(spanStart, GetStartPosition rest6, res, op2, right, op3)
                                                restAgain <- rest6
                                                false
                                        |   _ ->
                                                res <- ASTNode.Assign(spanStart, GetStartPosition rest6, res, op2, right, Token.Empty)
                                                restAgain <- rest5
                                                true
                                |   _ ->
                                        let right, rest5 = PythonCoreExpressionParser.ParseTestListStarExpr rest6
                                        match TryToken rest5 with
                                        |   Some(Token.TypeComment( _ , _ , _ , _ ), rest6) ->
                                                let op3 = List.head rest5
                                                res <- ASTNode.Assign(spanStart, GetStartPosition rest6, res, op2, right, op3)
                                                restAgain <- rest6
                                                false
                                        |   _ ->
                                                res <- ASTNode.Assign(spanStart, GetStartPosition rest6, res, op2, right, Token.Empty)
                                                restAgain <- rest5
                                                true
                        |   _ ->    false
                    do ()
                res, restAgain       
        |   _ ->    left, rest1
        
    and ParseDel(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |   Some(Token.PyDel( _ , _ , _ ), rest ) ->
                let op = List.head stream
                let node, rest2 = PythonCoreExpressionParser.ParseExprList rest
                ASTNode.DelStmt(spanStart, GetStartPosition rest2, op, node), rest2
        |   _ ->    raise (SyntaxError(GetStartPosition stream, "Expecting 'del' in del statement!"))
        
    and ParsePass(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |   Some(Token.PyPass( _ , _ , _ ), rest ) ->
                ASTNode.PassStmt(spanStart, GetStartPosition rest, List.head stream), rest
        |   _ ->    raise (SyntaxError(GetStartPosition stream, "Expecting 'pass' in pass statement!"))
        
    and ParseFlow(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        match TryToken stream with
        |   Some(Token.PyBreak( _ , _ , _ ), _ ) ->
                match flows with
                |   0u, _ -> raise (SyntaxError(GetStartPosition stream, "Found 'break' outside of loop statement!"))
                |   _ , _ ->    ParseBreak (stream, flows)
        |   Some(Token.PyContinue( _ , _ , _ ), _ ) ->
                match flows with
                |   0u, _ -> raise (SyntaxError(GetStartPosition stream, "Found 'continue' outside loop statement!"))
                |   _ , _ ->    ParseBreak (stream, flows)
        |   Some(Token.PyYield( _ , _ , _ ), _ ) ->
                match flows with
                |   0u, _ -> raise (SyntaxError(GetStartPosition stream, "Found 'yield' outside loop statement!"))
                |   _ , _ ->    ParseBreak (stream, flows)
        |   Some(Token.PyReturn( _ , _ , _ ), _ ) ->
                match flows with
                |   _, 0u -> raise (SyntaxError(GetStartPosition stream, "Found 'return' outside of function statement!"))
                |   _ , _ ->    ParseBreak (stream, flows)
        |   Some(Token.PyRaise( _ , _ , _ ),  _ ) ->    ParseRaise (stream, flows)
        |   _ ->    raise (SyntaxError(GetStartPosition stream, "Expected flow statement!"))
                
    and ParseBreak(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |   Some(Token.PyBreak( _ , _ , _ ), rest ) ->
                ASTNode.BreakStmt(spanStart, GetStartPosition rest, List.head stream), rest, flows
        |   _ ->    raise (SyntaxError(GetStartPosition stream, "Expecting 'break' in break statement!"))
        
    and ParseContinue(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |   Some(Token.PyContinue( _ , _ , _ ), rest ) ->
                ASTNode.ContinueStmt(spanStart, GetStartPosition rest, List.head stream), rest, flows
        |   _ ->    raise (SyntaxError(GetStartPosition stream, "Expecting 'continue' in continue statement!"))
        
    and ParseReturn(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |   Some(Token.PyReturn( _ , _ , _ ), rest ) ->
                let op = List.head stream
                let node, rest2 =   match TryToken rest with
                                    |   Some(Token.Newline( _ , _ , _ , _  , _ ), _ )
                                    |   Some(Token.EOF( _ ), _ )
                                    |   Some(Token.PySemiColon( _ , _ , _ ), _ ) -> ASTNode.Empty, rest
                                    |   _ ->    PythonCoreExpressionParser.ParseTestListStarExpr rest
                ASTNode.ReturnStmt(spanStart, GetStartPosition rest2, op, node), rest2, flows
        |   _ ->    raise (SyntaxError(GetStartPosition stream, "Expecting 'return' in return statement!"))
        
    and ParseYield(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        match TryToken stream with
        |   Some(Token.PyYield( _ , _ , _ ), _ ) ->
                let node, rest = PythonCoreExpressionParser.ParseYieldExpr stream
                node, rest, flows
        |   _ ->    raise (SyntaxError(GetStartPosition stream, "Expecting 'yield' in yield statement!"))
        
    and ParseRaise(stream: TokenStream, flows: uint * uint) : (ASTNode * TokenStream * (uint * uint)) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |   Some(Token.PyRaise( _ , _ , _ ), rest ) ->
                let op = List.head stream
                match TryToken rest with
                |   Some(Token.PySemiColon( _ , _ , _ ), _ )
                |   Some(Token.Newline( _ , _ , _ , _ , _ ), _ )
                |   Some(Token.EOF( _ ), _ ) ->
                        ASTNode.RaiseStmt(spanStart, GetStartPosition rest, op, ASTNode.Empty, Token.Empty, ASTNode.Empty), rest, flows
                |   _ ->
                        let first, rest2 = PythonCoreExpressionParser.ParseTest rest
                        match TryToken rest2 with
                        |   Some(Token.PyFrom( _ , _ , _ ), rest3 ) ->
                                let op2 = List.head rest2
                                let second, rest4 = PythonCoreExpressionParser.ParseTest rest2
                                ASTNode.RaiseStmt(spanStart, GetStartPosition rest4, op, first, op2, second), rest4, flows
                        |   _ ->    ASTNode.RaiseStmt(spanStart, GetStartPosition rest2, op, first, Token.Empty, ASTNode.Empty), rest2, flows
        |   _ ->    raise (SyntaxError(GetStartPosition stream, "Expecting 'raise' in raise statement!"))
        
    and ParseImport(stream: TokenStream) : (ASTNode * TokenStream) =
        match TryToken stream with
        |   Some(Token.PyImport( _ , _ , _ ), _ ) ->    ParseImportName stream
        |   Some(Token.PyFrom( _ , _ , _ ), _ ) ->  ParseImportFrom stream
        |   _ ->    raise (SyntaxError(GetStartPosition stream, "Expecting 'import' or 'from' statement!"))
        
    and ParseImportName(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |   Some(Token.PyImport( _ , _ , _), rest) ->
                let op = List.head stream
                let right, rest2 = ParseDottedAsNames rest
                ASTNode.ImportNameStmt(spanStart, GetStartPosition rest2, op, right), rest2
        |   _ ->    raise (SyntaxError(GetStartPosition stream, "Expecting 'import' in import Statement!"))
        
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
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |   Some(Token.Name( _ , _ , _, _ ), rest ) ->
                let mutable name = List.head stream
                let mutable nodes : Token List = List.Empty
                let mutable restAgain = rest
                let mutable dots : Token List = List.Empty
                nodes <- name :: nodes
                
                while   match TryToken restAgain with
                        |   Some(Token.PyDot( _ , _ , _ ), rest2) ->
                                dots <- List.head restAgain :: dots
                                match TryToken rest2 with
                                |   Some(Token.Name( _ , _ , _ , _ ), rest3) ->
                                        nodes <- List.head rest2 :: nodes
                                        restAgain <- rest3
                                        true
                                |   _ ->
                                        raise (SyntaxError(GetStartPosition rest2, "Expecting name literal after '.' in import name!"))
                        |   _ ->    false
                     do ()
                
                ASTNode.DottedName(spanStart, GetStartPosition restAgain, List.toArray(List.rev nodes), List.toArray(List.rev dots)), restAgain
        |   _ ->    raise (SyntaxError(GetStartPosition stream, "Expecting name literal in import name!"))
        
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
