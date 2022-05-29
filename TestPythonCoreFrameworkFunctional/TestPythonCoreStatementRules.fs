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
         
         
         
         
    [<Fact>]
    let ```Test`integration between toklenizer and parser for statement`` () =
         let node = "pass\r\n" |> PythonCoreTokenizer.TokenizeFromString |> PythonCoreParser.ParseFileInput
         
         Assert.Equal( ASTNode.FileInput(0u, 6u, [|
                                    ASTNode.SimpleStmt(0u, 6u, [|
                                        ASTNode.PassStmt(0u, 4u, Token.PyPass(0u, 4u, [|  |]))
                                    |], [|  |], Token.Newline(4u, 6u, '\r', '\n', [|  |]))
                                |], [|  |], Token.EOF(6u)), node)