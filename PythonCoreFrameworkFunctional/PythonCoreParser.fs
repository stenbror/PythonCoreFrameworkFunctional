namespace PythonCoreFrameworkFunctional


type Trivia =
    |   Empty

type Token =
    |   Empty
    |   PyFalse of uint * uint * Trivia array
    |   PyNone of uint * uint * Trivia array
    |   PyTrue of uint * uint * Trivia array
    |   PyEllipsis of uint * uint * Trivia array
    static member GetStartPosition(symbol: Token) : uint =
        match symbol with
        |   PyFalse(s, _ , _ )
        |   PyNone(s, _ , _ )
        |   PyTrue(s, _ , _ )
        |   PyEllipsis(s, _ , _) -> s
        |   _   ->  0u
    
type ASTNode =
    |   Empty
    |   False of uint * uint * Token
    |   None of uint * uint * Token
    |   True of uint * uint * Token
    |   Ellipsis of uint * uint * Token
    
   
type TokenStream = Token list
exception SyntaxError of uint * string

// Python parser //////////////////////////////////////////////////////////////////////////////////////////////////////
module PythonCoreParser =
    
    let TryToken (stream: TokenStream) : (Token * TokenStream) option =
        match stream with
        |   symbol :: rest ->   Some(symbol, rest)
        |   _ ->    Option.None
        
    let GetStartPosition(stream: TokenStream) : uint = Token.GetStartPosition(stream.Head)

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
        |   _   ->  raise ( SyntaxError(GetStartPosition(stream), "Expecting an atom literal!") )
