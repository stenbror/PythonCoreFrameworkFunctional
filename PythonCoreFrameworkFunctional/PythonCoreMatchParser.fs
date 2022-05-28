namespace PythonCoreFrameworkFunctional

open PythonCoreFrameworkFunctional.ParserUtilities

module PythonCoreMatchParser =
    
    let ParseMatch(stream: TokenStream, flow: (uint * uint)) : (ASTNode * TokenStream * (uint * uint)) =
        ASTNode.Empty, stream, (0u, 0u)
