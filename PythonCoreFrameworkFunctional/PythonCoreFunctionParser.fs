namespace PythonCoreFrameworkFunctional

module PythonCoreFunctionParser =
    
    let ParseFuncType(stream: TokenStream) : (ASTNode * TokenStream) =
        ASTNode.Empty, stream
        

