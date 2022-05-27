namespace PythonCoreFrameworkFunctional

open System
open System.Security
open PythonCoreFrameworkFunctional.ParserUtilities

module PythonCoreFunctionParser =
    
    let rec ParseFuncType(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        match TryToken stream with
        |  Some(Token.PyLeftParen( _ , _ , _ ), rest ) ->
            let op1 = List.head stream
            let left, rest3 =   match TryToken rest with
                                |   Some(Token.PyRightParen( _ , _ , _ ), rest2 ) ->   ASTNode.Empty, rest
                                |   _ ->  ParseTypeList rest
            let op2, rest4 =    match TryToken rest3 with
                                |   Some(Token.PyRightParen( _ , _ , _ ), rest5 ) ->
                                       List.head rest3, rest5
                                |   _ ->   raise (SyntaxError(GetStartPosition rest3, "Missing ')' in function definition!"))
            let op3, rest6 =    match TryToken rest4 with
                                |   Some(Token.PyArrow( _ , _ , _ ), rest7 ) ->
                                        List.head rest4, rest7
                                |   _ ->   raise (SyntaxError(GetStartPosition rest4, "Expecting '->' in function definition!"))
            let right, rest8 =  PythonCoreExpressionParser.ParseTest rest6
            ASTNode.FuncType(spanStart, GetStartPosition rest8, op1, left, op2, op3, right), rest8
        |  _ -> raise (SyntaxError(GetStartPosition stream, "Expecting '(' in function type!"))
        
    and ParseTypeList(stream: TokenStream) : (ASTNode * TokenStream) =
        let spanStart = GetStartPosition stream
        let mutable nodes : ASTNode List = List.Empty
        let mutable separators : Token List = List.Empty
        let mutable mulOp = Token.Empty
        let mutable mulNode = ASTNode.Empty
        let mutable powerOp = Token.Empty
        let mutable powerNode = ASTNode.Empty
        let mutable restAgain = stream
        
        match TryToken restAgain with
        |   Some(Token.PyPower( _ , _ , _ ), rest ) ->
                powerOp <- List.head restAgain
                let elem, rest2 = PythonCoreExpressionParser.ParseTest rest
                powerNode <- elem
                restAgain <- rest2
        |   Some(Token.PyMul( _ , _ , _ ), rest ) ->
                mulOp <- List.head restAgain
                let elem, rest2 = PythonCoreExpressionParser.ParseTest rest
                mulNode <- elem
                restAgain <- rest2
                while   match TryToken restAgain with
                        |   Some(Token.PyComma( _ , _ , _ ), rest3 ) ->
                                separators <- List.head restAgain :: separators
                                match TryToken rest3 with
                                |   Some(Token.PyPower( _ , _ , _ ), _ ) ->
                                        powerOp <- List.head rest3
                                        let elem, rest2 = PythonCoreExpressionParser.ParseTest rest
                                        powerNode <- elem
                                        restAgain <- rest2
                                        false
                                |   _ ->
                                    let elem2, rest4 = PythonCoreExpressionParser.ParseTest rest3
                                    nodes <- elem2 :: nodes
                                    restAgain <- rest4
                                    true
                        |   _ -> false
                    do ()
        |   _ ->
                let node, rest = PythonCoreExpressionParser.ParseTest restAgain
                nodes <- node :: nodes
                restAgain <- rest
                while   match TryToken restAgain with
                        |   Some(Token.PyComma( _ , _ , _ ), rest2 ) ->
                                separators <- List.head restAgain :: separators
                                restAgain <- rest2
                                match TryToken restAgain with
                                |   Some(Token.PyRightParen( _ , _ , _ ), _ ) -> false
                                |   Some(Token.PyPower( _ , _ , _ ), rest ) ->
                                        powerOp <- List.head restAgain
                                        let elem, rest2 = PythonCoreExpressionParser.ParseTest rest
                                        powerNode <- elem
                                        restAgain <- rest2
                                        false
                                |   Some(Token.PyMul( _ , _ , _ ), rest ) ->
                                        mulOp <- List.head restAgain
                                        let elem, rest2 = PythonCoreExpressionParser.ParseTest rest
                                        mulNode <- elem
                                        restAgain <- rest2
                                        while   match TryToken restAgain with
                                                |   Some(Token.PyComma( _ , _ , _ ), rest3 ) ->
                                                        separators <- List.head restAgain :: separators
                                                        match TryToken rest3 with
                                                        |   Some(Token.PyPower( _ , _ , _ ), _ ) ->
                                                                powerOp <- List.head rest3
                                                                let elem, rest2 = PythonCoreExpressionParser.ParseTest rest
                                                                powerNode <- elem
                                                                restAgain <- rest2
                                                                false
                                                        |   _ ->
                                                            let elem2, rest4 = PythonCoreExpressionParser.ParseTest rest3
                                                            nodes <- elem2 :: nodes
                                                            restAgain <- rest4
                                                            true
                                                |   _ -> false
                                            do ()
                                        false
                                |   _ ->
                                        false      
                        |   _ -> false
                    do ()
        
        ASTNode.TypeList(spanStart, GetStartPosition restAgain, mulOp, mulNode, powerOp, powerNode,
                         List.toArray(List.rev nodes), List.toArray(List.rev separators)), restAgain
