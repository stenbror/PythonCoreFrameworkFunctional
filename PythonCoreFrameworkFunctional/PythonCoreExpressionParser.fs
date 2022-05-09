namespace PythonCoreFrameworkFunctional

open System.Net.Security

   
exception SyntaxError of uint * string


module PythonCoreExpressionParser =
    
    let TryToken (stream: TokenStream) : (Token * TokenStream) option =
        match stream with
        |   symbol :: rest ->   Some(symbol, rest)
        |   _ ->    Option.None
        
    let GetStartPosition(stream: TokenStream) : uint =
            if stream.Length > 0 then
                Token.GetStartPosition(stream.Head)
            else 0u

    let rec ParseAtom(stream: TokenStream) : (ASTNode * TokenStream) =
        match TryToken stream with
        |   Some(Token.PyFalse(s, e, _ ), rest) ->
                let op = List.head stream
                ASTNode.False(s, e, op), rest
        |   Some(Token.PyNone(s, e, _ ), rest) ->
                let op = List.head stream
                ASTNode.None(s, e, op), rest
        |   Some(Token.PyTrue(s, e, _ ), rest) ->
                let op = List.head stream
                ASTNode.True(s, e, op), rest
        |   Some(Token.PyEllipsis(s, e, _ ), rest) ->
                let op = List.head stream
                ASTNode.Ellipsis(s, e, op), rest
        |   Some(Token.Name(s, e, _  , _ ), rest) ->
                let op = List.head stream
                ASTNode.Name(s, e, op), rest
        |   Some(Token.Number(s, e, _  , _ ), rest) ->
                let op = List.head stream
                ASTNode.Number(s, e, op), rest
        |   Some(Token.String(s, e, _ , _ ), rest) ->
                let spanStart = GetStartPosition(stream)
                let mutable restAgain = rest
                let mutable nodes : Token list = List.Empty
                nodes <- stream.Head :: nodes
                while   restAgain.Length > 0 &&
                                match TryToken restAgain with
                                |  Some(Token.String(s, e, _ , _ ), restNow) ->
                                        nodes <- restAgain.Head :: nodes
                                        restAgain <- restNow
                                        true
                                |       Option.None -> false
                                |       _ ->    false
                        do ()
                ASTNode.String(spanStart, GetStartPosition(restAgain), List.toArray(List.rev nodes)), restAgain
        |   Some(Token.PyLeftParen(s, e, _ ), rest) ->
                let spanStart = GetStartPosition(stream)
                let op1 = List.head stream
                match TryToken rest with
                |   Some(Token.PyRightParen( _ , _ , _ ), rest2) ->
                        let op2 = List.head rest
                        ASTNode.Tuple(spanStart, GetStartPosition(rest2), op1, ASTNode.Empty, op2), rest2
                |   _ ->        ASTNode.Empty, rest // TODO Fix later!
        |   Some(Token.PyLeftBracket(s, e, _ ), rest) ->
                let spanStart = GetStartPosition(stream)
                let op1 = List.head stream
                match TryToken rest with
                |   Some(Token.PyRightBracket( _ , _ , _ ), rest2) ->
                        let op2 = List.head rest
                        ASTNode.List(spanStart, GetStartPosition(rest2), op1, ASTNode.Empty, op2), rest2
                |   _ ->        ASTNode.Empty, rest // TODO Fix later!
        |   Some(Token.PyLeftCurly(s, e, _ ), rest) ->
                let spanStart = GetStartPosition(stream)
                let op1 = List.head stream
                match TryToken rest with
                |   Some(Token.PyRightCurly( _ , _ , _ ), rest2) ->
                        let op2 = List.head rest
                        ASTNode.Dictionary(spanStart, GetStartPosition(rest2), op1, ASTNode.Empty, op2), rest2
                |   _ ->        ASTNode.Empty, rest // TODO Fix later!
        |   _   ->  raise ( SyntaxError(GetStartPosition(stream), "Expecting an atom literal!") )

    and ParseAtomExpr(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition(stream)
        let mutable restAgain = stream
        let awaitOp = match TryToken stream with
                      |  Some(Token.PyAwait( _ , _ , _ ), rest) ->
                          let op = List.head stream
                          restAgain <- rest
                          op
                      |  _ -> Token.Empty
        let right, rest2 = ParseAtom restAgain // This is the Atom rule.
        let mutable trailerList : ASTNode list = []
        // TODO Handle trailers here later
    
    
        match awaitOp, trailerList with
        |   Token.Empty, [] ->
                right, rest2
        |   _  -> // TOIDO Fix rest2 with correct token stream
                ASTNode.AtomExpr(spanStart, GetStartPosition(rest2), awaitOp, right, List.toArray(List.rev trailerList)), rest2

    and ParsePower(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition(stream)
        let left, rest = ParseAtomExpr stream
        match TryToken rest with
        |  Some(Token.PyPower( _ , _ , _ ), rest2) ->
                let op = List.head rest
                let right, rest3 = ParseFactor rest2
                ASTNode.Power(spanStart, GetStartPosition(rest3), left, op, right), rest3
        |  _ -> left, rest
        
    and ParseFactor(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition(stream)
        match TryToken stream with
        |  Some(Token.PyPlus( _ , _ , _ ), rest) ->
                let op = List.head stream
                let right, rest2 = ParseFactor rest
                ASTNode.UnaryPlus(spanStart, GetStartPosition(rest2), op, right), rest2
        |  Some(Token.PyMinus( _ , _ , _ ), rest) ->
                let op = List.head stream
                let right, rest2 = ParseFactor rest
                ASTNode.UnaryMinus(spanStart, GetStartPosition(rest2), op, right), rest2
        |  Some(Token.PyBitInvert( _ , _ , _ ), rest) ->
                let op = List.head stream
                let right, rest2 = ParseFactor rest
                ASTNode.UnaryBitInvert(spanStart, GetStartPosition(rest2), op, right), rest2
        |  _ ->
                ParseAtomExpr stream
        
    and ParseTerm(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition(stream)
        let mutable left, rest = ParseFactor stream
        while   match TryToken rest with
                |  Some(Token.PyMul( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseFactor rest2
                        left <- ASTNode.Mul(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  Some(Token.PyDiv( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseFactor rest2
                        left <- ASTNode.Div(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  Some(Token.PyFloorDiv( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseFactor rest2
                        left <- ASTNode.FloorDiv(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  Some(Token.PyMatrice( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseFactor rest2
                        left <- ASTNode.Matrice(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  Some(Token.PyModulo( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseFactor rest2
                        left <- ASTNode.Modulo(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  _ -> false
                do ()
        left, rest
        
    and ParseArith(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition(stream)
        let mutable left, rest = ParseTerm stream
        while   match TryToken rest with
                |  Some(Token.PyPlus( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseTerm rest2
                        left <- ASTNode.Plus(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  Some(Token.PyMinus( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseTerm rest2
                        left <- ASTNode.Minus(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  _ -> false
                do ()
        left, rest
        
    and ParseShift(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition(stream)
        let mutable left, rest = ParseArith stream
        while   match TryToken rest with
                |  Some(Token.PyShiftLeft( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseArith rest2
                        left <- ASTNode.ShiftLeft(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  Some(Token.PyShiftRight( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseArith rest2
                        left <- ASTNode.ShiftRight(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  _ -> false
                do ()
        left, rest
        
    and ParseAnd(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition(stream)
        let mutable left, rest = ParseShift stream
        while   match TryToken rest with
                |  Some(Token.PyBitAnd( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseShift rest2
                        left <- ASTNode.BitAnd(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  _ -> false
                do ()
        left, rest
        
    and ParseXor(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition(stream)
        let mutable left, rest = ParseAnd stream
        while   match TryToken rest with
                |  Some(Token.PyBitXor( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseAnd rest2
                        left <- ASTNode.BitXor(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  _ -> false
                do ()
        left, rest
        
    and ParseOr(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition(stream)
        let mutable left, rest = ParseXor stream
        while   match TryToken rest with
                |  Some(Token.PyBitOr( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseXor rest2
                        left <- ASTNode.BitOr(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  _ -> false
                do ()
        left, rest
        
    and ParseStarExpr(stream: TokenStream) : (ASTNode * TokenStream) =
        match TryToken stream with
        |  Some(Token.PyMul( _ , _ , _ ), rest) ->
                let op = List.head stream
                let right, rest2 = ParseOr rest
                ASTNode.StarExpr(GetStartPosition(stream), GetStartPosition(rest2), op, right), rest2
        |  _ -> raise (SyntaxError(GetStartPosition(stream), "Expecting '*' in star expression!"))
        
    and ParseComparison(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition(stream)
        let mutable left, rest = ParseOr stream
        while   match TryToken rest with
                |  Some(Token.PyLess( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseOr rest2
                        left <- ASTNode.Less(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  Some(Token.PyLessEqual( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseOr rest2
                        left <- ASTNode.LessEqual(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  Some(Token.PyEqual( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseOr rest2
                        left <- ASTNode.Equal(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  Some(Token.PyGreaterEqual( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseOr rest2
                        left <- ASTNode.GreaterEqual(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  Some(Token.PyGreater( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseOr rest2
                        left <- ASTNode.Greater(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  Some(Token.PyNotEqual( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseOr rest2
                        left <- ASTNode.NotEqual(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  Some(Token.PyIn( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseOr rest2
                        left <- ASTNode.In(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                |  Some(Token.PyIs( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        match TryToken rest2 with
                        |  Some(Token.PyNot( _ , _ , _ ), rest3) ->
                                let op2 = List.head rest2
                                let right, rest4 = ParseOr rest3
                                left <- ASTNode.IsNot(spanStart, GetStartPosition(rest4), left, op, op2, right)
                                rest <- rest4
                        |  _ ->
                               let right, rest3 = ParseOr rest2
                               left <- ASTNode.Is(spanStart, GetStartPosition(rest3), left, op, right)
                               rest <- rest3 
                        true
                |  Some(Token.PyNot( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        match TryToken rest2 with
                        |  Some(Token.PyIn( _ , _ , _ ), rest3) ->
                                let op2 = List.head rest2
                                let right, rest4 = ParseOr rest3
                                left <- ASTNode.NotIn(spanStart, GetStartPosition(rest4), left, op, op2, right)
                                rest <- rest4
                        |  _ ->
                                raise ( SyntaxError(GetStartPosition(rest2), "Missing 'in' in 'not in' expression!" ))
                        true
                |  _ -> false
                do ()
        left, rest
        
    and ParseNotTest(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |  Some(Token.PyNot( _ , _ , _ ), rest) ->
                let op = List.head stream
                let right, rest2 = ParseNotTest rest
                ASTNode.NotTest(spanStart, GetStartPosition rest2, op, right), rest2
        |  _ -> ParseComparison stream
        
    and ParseAndTest(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        let mutable left, rest = ParseNotTest stream
        while   match TryToken rest with
                |  Some(Token.PyAnd( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseNotTest rest2
                        left <- ASTNode.AndTest(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                | _ -> false
                do ()
        left, rest
        
    and ParseOrTest(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        let mutable left, rest = ParseAndTest stream
        while   match TryToken rest with
                |  Some(Token.PyOr( _ , _ , _ ), rest2) ->
                        let op = List.head rest
                        let right, rest3 = ParseAndTest rest2
                        left <- ASTNode.OrTest(spanStart, GetStartPosition(rest3), left, op, right)
                        rest <- rest3
                        true
                | _ -> false
                do ()
        left, rest
        
    and ParseLambda(stream: TokenStream, isCond: bool) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |  Some(PyLambda( _ , _ , _ ), rest) ->
             let op = List.head stream
             let left, rest2 =  match TryToken rest with
                                |  Some(PyColon( _ , _ , _), _ ) -> ASTNode.Empty, rest
                                |  _ ->  ParseVarArgsList rest
             match TryToken rest2 with
             |  Some(PyColon( _ , _ , _ ), rest4) ->
                  let op2 = List.head rest2
                  let right, rest3 =  match isCond with | true -> ParseTest rest4 | false -> ParseTestNoCond rest4
                  ASTNode.Lambda(spanStart, GetStartPosition rest3, op, left, op2, right), rest3
             |  _ ->    raise(SyntaxError(GetStartPosition rest2, "Expecting ':' in 'lambda' expression!"))
        |  _ ->  raise (SyntaxError(GetStartPosition stream, "Expecting 'lambda' expression!"))
        
    and ParseVarArgsList(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseVFPDef(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseTestNoCond(stream: TokenStream) : (ASTNode * TokenStream) =
        match TryToken stream with
        |  Some(PyLambda( _ , _ , _ ), _) ->      ParseLambda(stream, false)
        |  _ ->
              ParseOrTest stream
        
    and ParseTest(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |  Some(PyLambda( _ , _ , _ ), _ ) -> ParseLambda(stream, true)
        | _ ->
           let left, rest = ParseOrTest stream
           match TryToken rest with
           |  Some(PyIf( _ , _ , _ ), rest2) ->
                   let op1 = List.head rest
                   let right, rest3 = ParseOrTest rest2
                   match TryToken rest3 with
                   |  Some(PyElse( _ , _ , _ ), rest4 ) ->
                        let op2 = List.head rest3
                        let next, rest5 = ParseTest rest4
                        ASTNode.Test(spanStart, GetStartPosition rest5, left, op1, right, op2, next), rest5
                   | _ ->  raise(SyntaxError(GetStartPosition rest3, "Expecting 'else' in test expression!"))
           |  _ ->
                left, rest
        
    and ParseNamedExpr(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        let left, rest = ParseTest stream
        match TryToken rest with
        |  Some(PyColonAssign( _ , _ , _ ), rest2 ) ->
             let op = List.head rest
             let right, rest3 = ParseTest rest2
             ASTNode.NamedExpr(spanStart, GetStartPosition rest3, left, op, right), rest3
        | _ ->
             left, rest
             
    and ParseTestListComp(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseTrailer(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseSubscriptList(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseSubscript(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseExprList(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseTestList(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseArgList(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseArgument(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseDictorSetMaker(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseCompIter(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseSyncCompFor(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseCompFor(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseCompIf(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        
    and ParseYieldExpr(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |  Some(Token.PyYield( _ , _ , _ ), rest) ->
              let op1 = List.head stream
              match TryToken rest with
              |  Some(Token.PyFrom( _ , _ , _ ), rest2) ->
                    let op2 = List.head rest
                    let right, rest3 = ParseTest rest2
                    ASTNode.YieldFrom(spanStart, GetStartPosition rest3, op1, op2, right), rest3
              |  _ ->
                    let right, rest4 = ParseTestListStarExpr rest
                    ASTNode.YieldExpr(spanStart, GetStartPosition rest4, op1, right), rest4
        |  _ -> raise (SyntaxError(GetStartPosition stream, "Expecting 'yield' expression!"))
            
    and ParseTestListStarExpr(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        let mutable nodes : ASTNode list = List.Empty
        let mutable separators : Token list = List.Empty
        let mutable node, rest = match TryToken stream with
                                 |  Some(Token.PyMul( _ , _ , _ ), _ )  -> ParseStarExpr stream
                                 | _ -> ParseTest stream
        nodes <- node :: nodes
        while   match TryToken rest with
                |  Some(Token.PyComma( _ , _ , _ ), rest2 ) ->
                        separators <- List.head rest :: separators
                        match TryToken rest2 with   
                        |  Some(Token.PyPlusAssign( _ , _ , _ ), _ )
                        |  Some(Token.PyMinusAssign( _ , _ , _ ), _ )
                        |  Some(Token.PyMulAssign( _ , _ , _ ), _ )
                        |  Some(Token.PyPowerAssign( _ , _ , _ ), _ )
                        |  Some(Token.PyMatriceAssign( _ , _ , _ ), _ )
                        |  Some(Token.PyModuloAssign( _ , _ , _ ), _ )
                        |  Some(Token.PyBitAndAssign( _ , _ , _ ), _ )
                        |  Some(Token.PyBitOrAssign( _ , _ , _ ), _ )
                        |  Some(Token.PyBitXorAssign( _ , _ , _ ), _ )
                        |  Some(Token.PyShiftLeftAssign( _ , _ , _ ), _ )
                        |  Some(Token.PyShiftRightAssign( _ , _ , _ ), _ )
                        |  Some(Token.PyFloorDivAssign( _ , _ , _ ), _ )
                        |  Some(Token.PyDivAssign( _ , _ , _ ), _ )
                        |  Some(Token.PyAssign( _ , _ , _ ), _ )
                        |  Some(Token.PyColon( _ , _ , _ ), _ )
                        |  Some(Token.PySemiColon( _ , _ , _ ), _ )
                        |  Some(Token.Newline( _ , _ , _ , _ , _ ), _ )
                        |  Some(Token.EOF( _ ), _ ) ->
                                rest <- rest2
                        |  _  ->
                             let node2, rest3 =  match TryToken rest2 with
                                                 |  Some(Token.PyMul( _ , _ , _ ), _ )  -> ParseStarExpr rest2
                                                 | _ -> ParseTest rest2
                             rest <- rest3
                             nodes <- node2 :: nodes
                        true
                |  _ ->  false
            do ()
        
        ASTNode.TestListStarExpr(spanStart, GetStartPosition rest, List.toArray(List.rev nodes),
                                 List.toArray(List.rev separators)), rest