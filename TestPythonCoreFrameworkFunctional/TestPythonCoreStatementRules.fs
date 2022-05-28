namespace PythonCoreFrameworkFunctional.Tests

open Xunit
open PythonCoreFrameworkFunctional

module TestPythonCoreStatementRules =
    
    [<Fact>]
    let ``Test pass statement``() =
         let stream = [
                         Token.PyPass(0u, 4u, [|  |])
                         Token.Newline(5u, 6u, '\r', ' ', [|  |])
                         Token.EOF(6u)
                      ]
         let node, rest, flows = PythonCoreStatementParser.ParseStmt (stream, (0u, 0u))
         
         Assert.Equal(
                ASTNode.SimpleStmt(0u, 6u, [|
                                    ASTNode.PassStmt(0u, 5u, Token.PyPass(0u, 4u, [|  |]))
                                |], [|  |], Token.Newline(5u, 6u, '\r', ' ', [|  |]))
             , node)
         
         Assert.Equal( (0u, 0u), flows)
         Assert.True(rest.Length = 1)