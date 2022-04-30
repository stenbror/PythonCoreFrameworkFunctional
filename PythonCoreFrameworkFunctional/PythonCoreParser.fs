namespace PythonCoreFrameworkFunctional

open System.Runtime.CompilerServices


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
        |   PyBitInvert(s, _ , _ ) -> s 
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
        
        