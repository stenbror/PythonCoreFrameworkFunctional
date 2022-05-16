
namespace PythonCoreFrameworkFunctional

exception SyntaxError of uint * string

module ParserUtilities =

    let TryToken (stream: TokenStream) : (Token * TokenStream) option =
            match stream with
            |   symbol :: rest ->   Some(symbol, rest)
            |   _ ->    Option.None
            
    let GetStartPosition(stream: TokenStream) : uint =
            if stream.Length > 0 then
                Token.GetStartPosition(stream.Head)
            else 0u