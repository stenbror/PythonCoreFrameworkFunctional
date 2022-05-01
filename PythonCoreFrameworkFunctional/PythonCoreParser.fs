namespace PythonCoreFrameworkFunctional

open System


type Trivia =
    |   Empty

type Token =
    |   Empty
    |   EOF of uint
    |   PyFalse of uint * uint * Trivia array
    |   PyNone of uint * uint * Trivia array
    |   PyTrue of uint * uint * Trivia array
    |   PyEllipsis of uint * uint * Trivia array
    |   Name of uint * uint * string * Trivia array
    |   Number of uint * uint * string * Trivia array
    |   String of uint * uint * string * Trivia array
    |   PyLeftParen of uint * uint * Trivia array
    |   PyLeftBracket of uint * uint * Trivia array
    |   PyLeftCurly of uint * uint * Trivia array
    |   PyRightParen of uint * uint * Trivia array
    |   PyRightBracket of uint * uint * Trivia array
    |   PyRightCurly of uint * uint * Trivia array
    |   PyAwait of uint * uint * Trivia array
    |   PyDot of uint * uint * Trivia array
    |   PyPower of uint * uint * Trivia array
    |   PyPlus of uint * uint * Trivia array
    |   PyMinus of uint * uint * Trivia array
    |   PyBitInvert of uint * uint * Trivia array
    |   PyMul of uint * uint * Trivia array
    |   PyDiv of uint * uint * Trivia array
    |   PyFloorDiv of uint * uint * Trivia array
    |   PyMatrice of uint * uint * Trivia array
    |   PyModulo of uint * uint * Trivia array
    |   PyShiftLeft of uint * uint * Trivia array
    |   PyShiftRight of uint * uint * Trivia array
    |   PyBitAnd of uint * uint * Trivia array
    |   PyBitXor of uint * uint * Trivia array
    |   PyBitOr of uint * uint * Trivia array
    |   PyLess of uint * uint * Trivia array
    |   PyLessEqual of uint * uint * Trivia array
    |   PyEqual of uint * uint * Trivia array
    |   PyGreaterEqual of uint * uint * Trivia array
    |   PyGreater of uint * uint * Trivia array
    |   PyNotEqual of uint * uint * Trivia array
    |   PyIn of uint * uint * Trivia array
    |   PyNot of uint * uint * Trivia array
    |   PyIs of uint * uint * Trivia array
    static member GetStartPosition(symbol: Token) : uint =
        match symbol with
        |   EOF(s)
        |   PyFalse(s, _ , _ )
        |   PyNone(s, _ , _ )
        |   PyTrue(s, _ , _ )
        |   PyEllipsis(s, _ , _)
        |   Name(s, _, _, _ )
        |   Number(s, _ , _ , _ )
        |   String(s, _ , _ , _ )
        |   PyLeftParen(s , _ , _ )
        |   PyLeftBracket(s, _ , _ )
        |   PyLeftCurly(s, _ , _ )
        |   PyLeftCurly(s, _ , _ )
        |   PyRightParen(s, _ , _ )
        |   PyRightBracket(s, _ , _ )
        |   PyRightCurly(s, _ , _ )
        |   PyAwait(s, _ , _ )
        |   PyDot(s, _ , _ )
        |   PyPower(s, _ , _ )
        |   PyPlus(s, _ , _ )
        |   PyMinus(s, _ , _)
        |   PyBitInvert(s, _ , _ )
        |   PyMul(s, _ , _ )
        |   PyDiv(s, _ , _ )
        |   PyFloorDiv(s, _ , _ )
        |   PyMatrice(s, _ , _ )
        |   PyModulo(s, _ , _ )
        |   PyShiftLeft(s, _ , _ )
        |   PyShiftRight(s, _ , _ )
        |   PyBitAnd(s, _ , _ )
        |   PyBitXor(s, _ , _ )
        |   PyBitOr(s, _ , _ )
        |   PyLess(s, _ , _ )
        |   PyLessEqual(s, _ , _ )
        |   PyEqual(s, _ , _ )
        |   PyGreaterEqual(s, _ , _ )
        |   PyGreater(s, _ , _ )
        |   PyIn(s, _ , _ )
        |   PyNot(s, _ , _ )
        |   PyIs(s, _ , _ ) -> s
        |   _   ->  0u
    
type ASTNode =
    |   Empty
    |   False of uint * uint * Token
    |   None of uint * uint * Token
    |   True of uint * uint * Token
    |   Ellipsis of uint * uint * Token
    |   Name of uint * uint * Token
    |   Number of uint * uint * Token
    |   String of uint * uint * Token array
    |   Tuple of uint * uint * Token * ASTNode * Token
    |   List of uint * uint * Token * ASTNode * Token
    |   Dictionary of uint * uint * Token * ASTNode * Token
    |   AtomExpr of uint * uint * Token * ASTNode * ASTNode array
    |   Power of uint * uint * ASTNode * Token * ASTNode
    |   UnaryPlus of uint * uint * Token * ASTNode
    |   UnaryMinus of uint * uint * Token * ASTNode
    |   UnaryBitInvert of uint * uint * Token * ASTNode
    |   Mul of uint * uint * ASTNode * Token * ASTNode
    |   Div of uint * uint * ASTNode * Token * ASTNode
    |   FloorDiv of uint * uint * ASTNode * Token * ASTNode
    |   Matrice of uint * uint * ASTNode * Token * ASTNode
    |   Modulo of uint * uint * ASTNode * Token * ASTNode
    |   Plus of uint * uint * ASTNode * Token * ASTNode
    |   Minus of uint * uint * ASTNode * Token * ASTNode
    |   ShiftLeft of uint * uint * ASTNode * Token * ASTNode
    |   ShiftRight of uint * uint * ASTNode * Token * ASTNode
    |   BitAnd of uint * uint * ASTNode * Token * ASTNode
    |   BitXor of uint * uint * ASTNode * Token * ASTNode
    |   BitOr of uint * uint * ASTNode * Token * ASTNode
    |   StarExpr of uint * uint * Token * ASTNode
    |   Less of uint * uint * ASTNode * Token * ASTNode
    |   LessEqual of uint * uint * ASTNode * Token * ASTNode
    |   Equal of uint * uint * ASTNode * Token * ASTNode
    |   GreaterEqual of uint * uint * ASTNode * Token * ASTNode
    |   Greater of uint * uint * ASTNode * Token * ASTNode
    |   NotEqual of uint * uint * ASTNode * Token * ASTNode
    |   In of uint * uint * ASTNode * Token * ASTNode
    |   NotIn of uint * uint * ASTNode * Token * Token * ASTNode
    |   Is of uint * uint * ASTNode * Token * ASTNode
    |   IsNot of uint * uint * ASTNode * Token * Token * ASTNode
    |   NotTest of uint * uint * Token * ASTNode
   
type TokenStream = Token list
exception SyntaxError of uint * string

// Python parser //////////////////////////////////////////////////////////////////////////////////////////////////////
module PythonCoreParser =
    
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