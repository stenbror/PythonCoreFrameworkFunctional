namespace PythonCoreFrameworkFunctional

open PythonCoreFrameworkFunctional.ParserUtilities

module PythonCoreExpressionParser =
    
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
                |   _ ->
                        let node10, rest10 =    match TryToken rest with
                                                |   Some(Token.PyYield( _ , _ , _ ), _ ) -> ParseYieldExpr rest
                                                |   _ ->  ParseTestListComp rest
                        match TryToken rest10 with
                        |   Some(Token.PyRightParen( _ , _ , _ ), rest11) ->
                                let op2 = List.head rest10
                                ASTNode.Tuple(spanStart, GetStartPosition rest11, op1, node10, op2), rest11
                        |   _ ->   raise (SyntaxError(GetStartPosition rest10, "Expecting ')' in tuple!"))
        |   Some(Token.PyLeftBracket(s, e, _ ), rest) ->
                let spanStart = GetStartPosition(stream)
                let op1 = List.head stream
                match TryToken rest with
                |   Some(Token.PyRightBracket( _ , _ , _ ), rest2) ->
                        let op2 = List.head rest
                        ASTNode.List(spanStart, GetStartPosition(rest2), op1, ASTNode.Empty, op2), rest2
                |   _ ->
                        let node20, rest12 = ParseTestListComp rest
                        match TryToken rest12 with
                        |   Some(Token.PyRightBracket( _ , _ , _ ), rest13) ->
                                let op2 = List.head rest12
                                ASTNode.List(spanStart, GetStartPosition rest13, op1, node20, op2), rest13
                        |   _ ->  raise (SyntaxError(GetStartPosition rest12, "Expecting ']' in list!"))
        |   Some(Token.PyLeftCurly(s, e, _ ), rest) ->
                let spanStart = GetStartPosition(stream)
                let op1 = List.head stream
                match TryToken rest with
                |   Some(Token.PyRightCurly( _ , _ , _ ), rest2) ->
                        let op2 = List.head rest
                        ASTNode.Dictionary(spanStart, GetStartPosition(rest2), op1, ASTNode.Empty, op2), rest2
                |   _ ->
                        let node30, rest15 = ParseDictorSetMaker rest
                        match node30 with
                        |   DictionaryContainer( _ , _ , _ , _ ) ->
                                match TryToken rest15 with
                                |   Some(Token.PyRightCurly( _, _ , _ ), rest16) ->
                                        let op2 = List.head rest15
                                        ASTNode.Dictionary(spanStart, GetStartPosition rest16, op1, node30, op2), rest16
                                |   _ ->  raise (SyntaxError(GetStartPosition rest15, "Expecting '}' in dictionary!"))
                        |   SetContainer( _ , _ , _ , _) ->
                                match TryToken rest15 with
                                |   Some(Token.PyRightCurly( _ , _ , _ ), rest17) ->
                                        let op2 = List.head rest15
                                        ASTNode.Set(spanStart, GetStartPosition rest17, op1, node30, op2), rest17
                                |   _ ->  raise (SyntaxError(GetStartPosition rest15, "Expecting '}' in set!"))
                        |   _ ->
                                raise (SyntaxError(GetStartPosition rest, "Expecting dictionary or set!"))      
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
        
        restAgain <- rest2
        while   match TryToken restAgain with
                |   Some(Token.PyLeftParen( _ , _ , _ ), _ )
                |   Some(Token.PyLeftBracket( _ , _ , _ ), _ )
                |   Some(Token.PyDot( _ , _ , _ ), _ ) ->
                        let node5, rest5 = ParseTrailer restAgain
                        restAgain <- rest5
                        trailerList <- node5 :: trailerList
                        true
                |   _ -> false
            do ()
    
        match awaitOp, trailerList with
        |   Token.Empty, [] ->
                right, rest2
        |   _  ->
                ASTNode.AtomExpr(spanStart, GetStartPosition(rest2), awaitOp, right, List.toArray(List.rev trailerList)), restAgain

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
        let spanStart = GetStartPosition stream
        let mutable mulOp = Token.Empty
        let mutable powerOp = Token.Empty
        let mutable mulNode = ASTNode.Empty
        let mutable powerNode = ASTNode.Empty
        let mutable nodes : ASTNode List = List.Empty
        let mutable separators : Token List = List.Empty
        let mutable rest = stream
        
        match TryToken rest with
        |   Some(Token.PyPower( _ , _ , _ ), rest2 ) ->
                powerOp <- List.head rest
                let node1, rest3 = ParseVFPDef rest2
                powerNode <- node1
                rest <- rest3
                match TryToken rest with
                |   Some(Token.PyComma( _ , _ , _ ), rest4) ->
                        separators <- List.head rest :: separators
                        rest <- rest4
                |   _ -> ()
        |   Some(Token.PyMul( _ , _ , _ ), rest5 ) ->
                mulOp <- List.head rest
                let node2, rest6 = ParseVFPDef rest5
                mulNode <- node2
                rest <- rest6
                while   match TryToken rest with
                        |   Some(Token.PyComma( _ , _ , _ ), rest7) ->
                                separators <- List.head rest :: separators
                                match TryToken rest7 with
                                |   Some(Token.PyPower( _ , _ , _ ), rest8) ->
                                        powerOp <- List.head rest
                                        let node1, rest3 = ParseVFPDef rest8
                                        powerNode <- node1
                                        rest <- rest3
                                        match TryToken rest with
                                        |   Some(Token.PyComma( _ , _ , _ ), rest4) ->
                                                separators <- List.head rest :: separators
                                                rest <- rest4
                                        |   _ -> ()
                                        false
                                |   _ ->
                                        let node3, rest9 = ParseVFPDef rest7
                                        match TryToken rest9 with
                                        |   Some(Token.PyAssign( _ , _ , _ ), rest10) ->
                                                let op = List.head rest9
                                                let node4, rest11 = ParseTest rest10
                                                nodes <- ASTNode.VFPDefAssign(spanStart, GetStartPosition rest11, node3, op, node4) :: nodes
                                                rest <- rest11
                                        |   _ ->
                                                nodes <- node3 :: nodes
                                                rest <- rest9
                                        true
                        |   _ ->  false
                   do ()
        |   _ ->
                let node10, rest10 = ParseVFPDef rest
                match TryToken rest10 with
                |   Some(Token.PyAssign( _ , _ , _ ), rest11) ->
                        let op = List.head rest10
                        let node11, rest12 = ParseTest rest11
                        nodes <- ASTNode.VFPDefAssign(spanStart, GetStartPosition rest12, node10, op, node11) :: nodes
                        rest <- rest12
                |   _ ->
                        nodes <- node10 :: nodes
                        rest <- rest10
                
                while   match TryToken rest with
                        |   Some(Token.PyComma( _ , _ , _ ), rest13) ->
                                separators <- List.head rest :: separators
                                match TryToken rest13 with
                                |   Some(Token.PyColon( _ , _ , _ ), _ ) -> false
                                |   Some(Token.PyMul( _ , _ , _ ), rest14) ->                  
                                        mulOp <- List.head rest13
                                        let node2, rest6 = ParseVFPDef rest14
                                        mulNode <- node2
                                        rest <- rest6
                                        while   match TryToken rest with
                                                |   Some(Token.PyComma( _ , _ , _ ), rest7) ->
                                                        separators <- List.head rest :: separators
                                                        match TryToken rest7 with
                                                        |   Some(Token.PyPower( _ , _ , _ ), rest8) ->
                                                                powerOp <- List.head rest
                                                                let node1, rest3 = ParseVFPDef rest8
                                                                powerNode <- node1
                                                                rest <- rest3
                                                                match TryToken rest with
                                                                |   Some(Token.PyComma( _ , _ , _ ), rest4) ->
                                                                        separators <- List.head rest :: separators
                                                                        rest <- rest4
                                                                |   _ -> ()
                                                                false
                                                        |   _ ->
                                                                let node3, rest9 = ParseVFPDef rest7
                                                                match TryToken rest9 with
                                                                |   Some(Token.PyAssign( _ , _ , _ ), rest10) ->
                                                                        let op = List.head rest9
                                                                        let node4, rest11 = ParseTest rest10
                                                                        nodes <- ASTNode.VFPDefAssign(spanStart, GetStartPosition rest11, node3, op, node4) :: nodes
                                                                        rest <- rest11
                                                                |   _ ->
                                                                        nodes <- node3 :: nodes
                                                                        rest <- rest9
                                                                true
                                                |   _ ->  false
                                           do ()
                                        false      
                                |   Some(Token.PyPower( _ , _ , _ ), rest14) ->
                                        powerOp <- List.head rest13
                                        let node1, rest3 = ParseVFPDef rest14
                                        powerNode <- node1
                                        rest <- rest3
                                        match TryToken rest with
                                        |   Some(Token.PyComma( _ , _ , _ ), rest4) ->
                                                separators <- List.head rest :: separators
                                                rest <- rest4
                                        |   _ -> ()
                                        false
                                |   Some(Token.PyComma( _ , _ , _ ), _ ) ->
                                        raise (SyntaxError(GetStartPosition rest13, "Unexpected ',' in list!"))
                                |   _ ->
                                        true
                        |   _ ->
                                let node10, rest10 = ParseVFPDef rest
                                match TryToken rest10 with
                                |   Some(Token.PyAssign( _ , _ , _ ), rest11) ->
                                        let op = List.head rest10
                                        let node11, rest12 = ParseTest rest11
                                        nodes <- ASTNode.VFPDefAssign(spanStart, GetStartPosition rest12, node10, op, node11) :: nodes
                                        rest <- rest12
                                |   _ ->
                                        nodes <- node10 :: nodes
                                        rest <- rest10
                                true
                    do ()
        
        ASTNode.VarArgsList(spanStart, GetStartPosition rest, mulOp, mulNode, powerOp, powerNode,
                            List.toArray(List.rev nodes), List.toArray(List.rev separators)), rest
        
    and ParseVFPDef(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |   Some(Token.Name( _ , _ , _ , _ ), rest ) ->
                let name = List.head stream
                ASTNode.Name(spanStart, GetStartPosition rest, name) , rest
        |   _ ->   raise (SyntaxError(GetStartPosition stream, "Expecting Name literal in list!"))
        
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
        let spanStart = GetStartPosition stream
        let mutable nodes : ASTNode List = List.Empty
        let mutable separators : Token List = List.Empty
        let node, rest = match TryToken stream with
                         |   Some(Token.PyMul( _ , _ , _ ), _ ) -> ParseStarExpr stream
                         |   _ ->   ParseNamedExpr stream
        nodes <- node :: nodes
        let mutable rest2 = rest
        match TryToken rest2 with
        |   Some(Token.PyFor( _ , _ , _ ), _ )
        |   Some(Token.PyAsync( _ , _ , _ ), _ ) ->
                let node2, rest3 = ParseCompFor rest2
                nodes <- node2 :: nodes
                rest2 <- rest3
        |   _ ->
                while   match TryToken rest2 with
                        |  Some(Token.PyComma( _ , _ , _ ), rest4 ) ->
                              separators <- List.head rest :: separators
                              match TryToken rest4 with
                              |  Some(Token.PyRightParen( _ , _ , _ ), _ )
                              |  Some(Token.PyRightBracket( _ , _ , _ ), _ ) ->
                                    rest2 <- rest4
                                    false
                              |  Some(Token.PyComma( _ , _ , _ ), _ ) ->
                                    raise (SyntaxError(GetStartPosition rest4, "Unexpected ',' in list!"))
                              |  _ ->
                                    let node2, rest5 =
                                           match TryToken rest4 with
                                           |   Some(Token.PyMul( _ , _ , _ ), _ ) -> ParseStarExpr stream
                                           |   _ ->   ParseNamedExpr stream
                                    rest2 <- rest5
                                    nodes <- node2 :: nodes
                                    true
                        |   _ -> false
                   do ()
        ASTNode.TestList(spanStart, GetStartPosition rest2, List.toArray(List.rev nodes),
                         List.toArray(List.rev separators)), rest2
        
    and ParseTrailer(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        let symbol1 = List.head stream
        match TryToken stream with
        |   Some(Token.PyDot( _ , _ , _ ), rest ) ->
                match TryToken rest with
                |   Some(Token.Name( _ , _ , _ , _ ), rest2 ) ->
                        let name = List.head rest
                        ASTNode.DotName(spanStart, GetStartPosition rest2, symbol1, name), rest2
                |   _ ->   raise ( SyntaxError(GetStartPosition rest, "Expecting Name literal after '.'!") )
        |   Some(Token.PyLeftParen( _ , _ , _ ), rest ) ->
                let node, rest2 =
                        match TryToken rest with
                        |   Some(Token.PyRightParen( _ , _ , _ ), _ ) -> ASTNode.Empty, rest
                        |   _ ->  ParseArgList rest
                match TryToken rest2 with
                |    Some(Token.PyRightParen( _ , _ , _ ), rest3) ->
                        let symbol2 = List.head rest2
                        ASTNode.CallExpression(spanStart, GetStartPosition rest2, symbol1, node, symbol2), rest3
                |    _ ->   raise( SyntaxError(GetStartPosition rest2, "Expecting ')' in call!") )
        |   Some(Token.PyLeftBracket( _ , _ , _ ), rest ) ->
                let node, rest2 = ParseSubscriptList rest
                match TryToken rest2 with
                |    Some(Token.PyRightBracket( _ , _ , _ ), rest3) ->
                        let symbol2 = List.head rest2
                        ASTNode.IndexExpression(spanStart, GetStartPosition rest2, symbol1, node, symbol2), rest3
                |    _ ->   raise( SyntaxError(GetStartPosition rest2, "Expecting ']' in indexer!") )
        |   _ ->   ASTNode.Empty, stream
        
    and ParseSubscriptList(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        let mutable nodes : ASTNode List = List.Empty
        let mutable separators : Token List = List.Empty
        let mutable node, rest = ParseSubscript stream
        nodes <- node :: nodes
        while   match TryToken rest with
                |  Some(Token.PyComma( _ , _ , _ ), rest2 ) ->
                     separators <- List.head rest :: separators
                     match TryToken rest2 with
                     |  Some(Token.PyRightBracket( _ , _ , _ ), _ ) ->
                           rest <- rest2
                           false
                     |  _ ->
                           let node2, rest3 = ParseSubscript rest2
                           nodes <- node2 :: nodes
                           rest <- rest3
                           true
                |  _ -> false
           do ()
        ASTNode.SubscriptList(spanStart, GetStartPosition rest,
                             List.toArray(List.rev nodes), List.toArray(List.rev separators)), rest
        
    and ParseSubscript(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        let mutable first = ASTNode.Empty
        let mutable op1 = Token.Empty
        let mutable second = ASTNode.Empty
        let mutable op2 = Token.Empty
        let mutable third = ASTNode.Empty
        let mutable rest = stream
        match TryToken rest with
        |   Some(Token.PyColon( _ , _ , _ ), _ ) ->  ()
        |   _ ->
                let node1, rest1 = ParseTest rest
                first <- node1
                rest <- rest1
        match TryToken rest with
        |  Some(Token.PyColon( _ , _ , _ ), rest2 ) ->
                op1 <- List.head rest
                rest <- rest2 
                match TryToken rest with 
                |   Some(Token.PyColon( _ , _ , _ ), _ )
                |   Some(Token.PyComma( _ , _ , _ ), _ )
                |   Some(Token.PyRightBracket( _ , _ , _ ), _ ) -> ()
                |  _ ->
                    let node2, rest3 = ParseTest rest
                    second <- node2
                    rest <- rest3
                match TryToken rest with
                |   Some(Token.PyColon( _ , _ , _ ), rest4 ) ->
                        op2 <- List.head rest
                        rest <- rest4
                        match TryToken rest with
                        |   Some(Token.PyComma( _ , _ , _ ), _ )
                        |   Some(Token.PyRightBracket( _ , _ , _ ), _ ) ->
                                ()
                        |   _ ->
                                let node3, rest5 = ParseTest rest
                                third <- node3
                                rest <- rest5
                |   _ ->  ()
        |  _ -> ()
        ASTNode.Subscript(spanStart, GetStartPosition rest, first, op1, second, op2, third ), rest
        
    and ParseExprList(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        let mutable nodes : ASTNode List = List.Empty
        let mutable separarors: Token List = List.Empty
        let mutable node, rest = match TryToken stream with
                                 | Some(Token.PyMul( _ , _ , _ ), _ ) -> ParseStarExpr stream
                                 | _ -> ParseOr stream
        nodes <- node :: nodes
        while match TryToken rest with
              |  Some(Token.PyComma( _ , _ , _ ), rest2 ) ->
                   separarors <- List.head rest :: separarors
                   match TryToken rest2 with
                   |  Some(Token.PyIn( _ , _ , _ ), _ ) ->
                        rest <- rest2
                        false
                   |  _ ->
                        let node2, rest3 = match TryToken rest2 with
                                           | Some(Token.PyMul( _ , _ , _ ), _ ) -> ParseStarExpr rest2
                                           | _ -> ParseOr rest2
                        nodes <- node2 :: nodes
                        rest <- rest3
                        true
              |  _ -> false
           do ()
        ASTNode.ExprList(spanStart, GetStartPosition rest,
                         List.toArray(List.rev nodes),
                         List.toArray(List.rev separarors)), rest
        
    and ParseTestList(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        let mutable nodes : ASTNode List = List.Empty
        let mutable separators: Token List = List.Empty
        let mutable node, rest = ParseTest stream
        nodes <- node :: nodes
        while match TryToken rest with
              |  Some(Token.PyComma( _ , _ , _ ), rest2 ) ->
                   separators <- List.head rest :: separators
                   match TryToken rest2 with
                   |  Some(Token.PySemiColon( _ , _ , _ ), _ )
                   |  Some(Token.Newline( _ , _ , _ , _ , _ ), _ )
                   |  Some(Token.EOF( _ ), _ ) ->
                        rest <- rest2
                        false
                   |  _ ->
                        let node2, rest3 = ParseTest rest2
                        rest <- rest3
                        nodes <- node2 :: nodes
                        true
              |  _ -> false
           do ()
        ASTNode.TestList(spanStart, GetStartPosition rest, List.toArray(List.rev nodes), List.toArray(List.rev separators)), rest
        
    and ParseArgList(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        let mutable nodes : ASTNode List = List.Empty
        let mutable separators : Token List = List.Empty
        let mutable node, rest = ParseArgument stream
        nodes <- node :: nodes
        while   match TryToken rest with
                |  Some(Token.PyComma( _ , _ , _ ), rest2 ) ->
                     separators <- List.head rest :: separators
                     match TryToken rest2 with
                     |  Some(Token.PyRightParen( _ , _ , _ ), _ ) ->
                           rest <- rest2
                           false
                     |  _ ->
                           let node2, rest3 = ParseArgument rest2
                           nodes <- node2 :: nodes
                           rest <- rest3
                           true
                |  _ -> false
           do ()
        ASTNode.ArgumentList(spanStart, GetStartPosition rest,
                             List.toArray(List.rev nodes), List.toArray(List.rev separators)), rest
        
    and ParseArgument(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |  Some(Token.PyMul( _ , _ , _ ), rest )
        |  Some(Token.PyPower( _ , _ , _ ), rest ) ->
              let op = List.head stream
              let right, rest2 = ParseTest rest
              ASTNode.Argument(spanStart, GetStartPosition rest2, ASTNode.Empty, op, right), rest2
        |  _ ->
             let left, rest3 = ParseTest stream
             match TryToken rest3 with
             |    Some(Token.PyAsync( _ , _ , _ ), _ )
             |    Some(Token.PyFor( _ , _ , _ ), _ ) ->
                     let right, rest4 = ParseCompFor rest3
                     ASTNode.Argument(spanStart, GetStartPosition rest4, left, Token.Empty, right), rest4
             |    Some(Token.PyColonAssign( _ , _ , _ ), rest5)
             |    Some(Token.PyAssign( _ , _ , _ ), rest5) ->
                     let op = List.head rest3
                     let right, rest6 = ParseTest rest5
                     ASTNode.Argument(spanStart, GetStartPosition rest6, left, op, right), rest6
             |  _ ->
                  left, rest3
        
    and ParseDictorSetMaker(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        let mutable key = ASTNode.Empty
        let mutable symbol = Token.Empty
        let mutable value = ASTNode.Empty
        let mutable isDictionary = true
        let mutable rest = stream
        
        match TryToken rest with
        |   Some(Token.PyMul( _ , _ , _ ), _ ) ->
                isDictionary <- false
                let node, rest2 = ParseStarExpr rest
                key <- node
                rest <- rest2
        |   Some(Token.PyPower( _ , _ , _ ), rest2 ) ->
                let spanStart2 = GetStartPosition rest
                let op1 = List.head rest
                let node2, rest4 = ParseTest rest
                rest <- rest4
                key <- PowerKey(spanStart2, GetStartPosition rest, op1, node2) 
        |   _ ->
                let node3, rest5 = ParseTest rest
                key <- node3
                rest <- rest5
                match TryToken rest with
                |   Some(Token.PyColon( _ , _ , _ ), rest6 ) ->
                        let op1 = List.head rest
                        let node4, rest7 = ParseTest rest6
                        rest <- rest7
                        value <- node4
                        key <- DictiionaryEntry(spanStart, GetStartPosition rest, key, op1, value)
                |   _ ->  isDictionary <- false
                
        let mutable nodes : ASTNode List = List.Empty
        let mutable separators : Token List = List.Empty
        nodes <- key :: nodes
        
        match TryToken rest with
        |   Some(Token.PyFor( _ , _ , _ ), _ )
        |   Some(Token.PyAsync( _ , _ , _ ), _ ) ->
                let node3, rest8 = ParseCompFor rest
                nodes <- node3 :: nodes
                rest <- rest8
        |   _ ->
                match isDictionary with
                |   true ->
                        while   match TryToken rest with
                                |   Some(Token.PyComma( _ , _ , _ ), rest20 ) ->
                                        let op2 = List.head rest
                                        separators <- op2 :: separators
                                        rest <- rest20
                                        match TryToken rest with
                                        |   Some(Token.PyRightCurly( _ , _ , _ ), _ ) -> false
                                        |   Some(Token.PyComma( _ , _ , _ ), _ ) ->
                                                raise (SyntaxError(GetStartPosition rest, "Unexpected ',' in dictionary!"))
                                        |   Some(Token.PyPower( _ , _ , _ ), rest21 ) ->
                                                let spanStart3 = GetStartPosition rest
                                                let op3 = List.head rest
                                                let node5, rest22 = ParseTest rest21
                                                nodes <- ASTNode.PowerKey(spanStart3, GetStartPosition rest21, op3, node5) :: nodes
                                                rest <- rest22
                                                true
                                        |   _ ->
                                                let node6, rest23 = ParseTest rest
                                                match TryToken rest23 with
                                                |   Some(Token.PyColon( _ , _ , _ ), rest24 ) ->
                                                        let op5 = List.head rest23
                                                        let node7, rest25 = ParseTest rest24
                                                        rest <- rest25
                                                        nodes <- DictiionaryEntry(spanStart, GetStartPosition rest, node6, op5, node7) :: nodes
                                                        true
                                                |   _ ->  raise (SyntaxError(GetStartPosition rest23, "Expecting ':' in dictionary entry!"))
                                |   _ ->        false
                            do ()
                |   _ ->
                        while   match TryToken rest with
                                |   Some(Token.PyComma( _ , _ , _ ), rest10 ) ->
                                        let op1 = List.head rest
                                        separators <- op1 :: separators
                                        rest <- rest10
                                        match TryToken rest with
                                        |   Some(Token.PyRightCurly( _ , _ , _ ), _ ) -> false
                                        |   Some(Token.PyComma( _ , _ , _ ), _ ) ->
                                                raise (SyntaxError(GetStartPosition rest, "Unexpected ',' in set!"))
                                        |   _ ->
                                                let node4, rest11 = match TryToken rest with
                                                                    |   Some(Token.PyMul( _ , _ , _ ), _ ) ->  ParseStarExpr rest
                                                                    |   _ ->  ParseTest rest
                                                nodes <- node4 :: nodes
                                                rest <- rest11
                                                true
                                |   _ -> false
                            do ()
        
        match isDictionary with
        |    true ->    DictionaryContainer(spanStart, GetStartPosition rest, List.toArray(List.rev nodes), List.toArray(List.rev separators)), rest
        |    _ ->       SetContainer(spanStart, GetStartPosition rest, List.toArray(List.rev nodes), List.toArray(List.rev separators)), rest
        
    and ParseCompIter(stream: TokenStream) : (ASTNode * TokenStream) =
        match TryToken stream with
        |  Some(Token.PyFor( _ , _ , _ ), _ )
        |  Some(Token.PyAsync( _ , _ , _ ), _ ) ->  ParseCompFor stream
        |  Some(Token.PyIf( _ , _ , _ ), _ ) -> ParseCompIf stream
        |  _ ->  ASTNode.Empty, stream
        
    and ParseSyncCompFor(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |  Some(Token.PyFor( _ , _ , _ ), rest ) ->
              let op1 = List.head stream
              let left, rest2 = ParseExprList rest
              match TryToken rest2 with
              |  Some(Token.PyIn( _ , _ , _ ), rest3 ) ->
                    let op2 = List.head rest2
                    let right, rest4 = ParseOrTest rest3
                    let next, rest5 = ParseCompIter rest4
                    ASTNode.CompFor(spanStart, GetStartPosition rest5, op1, left, op2, right, next), rest5
              |  _ ->  raise (SyntaxError(GetStartPosition rest2, "Expecting 'in' in 'for' comprehension expression!"))
        |  _ -> raise (SyntaxError(GetStartPosition stream, "Expecting 'for' in 'for' comprehension expression!"))
        
        
    and ParseCompFor(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |  Some(Token.PyAsync( _ , _ , _ ), rest ) ->
             let op = List.head stream
             let right, rest2 = ParseSyncCompFor rest
             ASTNode.CompAsyncFor(spanStart, GetStartPosition rest2, op, right), rest2
        |  _ -> ParseSyncCompFor stream
        
    and ParseCompIf(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |   Some(Token.PyIf( _ , _ , _ ), rest ) ->
                let op = List.head stream
                let right, rest2 = ParseTestNoCond rest
                let next, rest3 = ParseCompIter rest2
                ASTNode.CompIf(spanStart, GetStartPosition rest3, op, right, next), rest3
        |   _ ->   raise (SyntaxError(GetStartPosition stream, "Expecting 'if' in comprehension expression!"))
        
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