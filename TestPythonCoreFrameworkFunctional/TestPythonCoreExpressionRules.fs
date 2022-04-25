
namespace PythonCoreFrameworkFunctional.Tests

open System
open Xunit
open PythonCoreFrameworkFunctional


module ExpressionsRulesTests =
    
    [<Fact>]
    let ``Test Atom Expression rule for False token``() =
         let stream = [ Token.PyFalse(0u, 5u, [|  |]); ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(node, ASTNode.False(0u, 5u, Token.PyFalse(0u, 5u, [|  |])))
         Assert.True(rest.Length = 0)
         
    [<Fact>]
    let ``Test Atom Expression rule for None token``() =
         let stream = [ Token.PyNone(0u, 4u, [|  |]); ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(node, ASTNode.None(0u, 4u, Token.PyNone(0u, 4u, [|  |])))
         Assert.True(rest.Length = 0)
         
    [<Fact>]
    let ``Test Atom Expression rule for True token``() =
         let stream = [ Token.PyTrue(0u, 4u, [|  |]); ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(node, ASTNode.True(0u, 4u, Token.PyTrue(0u, 4u, [|  |])))
         Assert.True(rest.Length = 0)
         
    [<Fact>]
    let ``Test Atom Expression rule for ... token``() =
         let stream = [ Token.PyEllipsis(0u, 3u, [|  |]); ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(node, ASTNode.Ellipsis(0u, 3u, Token.PyEllipsis(0u, 3u, [|  |])))
         Assert.True(rest.Length = 0)
         
    [<Fact>]
    let ``Test Atom Expression rule for name token``() =
         let stream = [ Token.Name(0u, 3u, "abc", [|  |]); ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(node, ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])))
         Assert.True(rest.Length = 0)
         
    [<Fact>]
    let ``Test Atom Expression rule for number token``() =
         let stream = [ Token.Name(0u, 4u, "1.34", [|  |]); ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(node, ASTNode.Name(0u, 4u, Token.Name(0u, 4u, "1.34", [|  |])))
         Assert.True(rest.Length = 0)
         
    [<Fact>]
    let ``Test Atom Expression rule for single string token``() =
         let stream = [ Token.String(0u, 14u, "'Hello, World!'", [|  |]); Token.EOF(14u) ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(node, ASTNode.String(0u, 14u, [| Token.String(0u, 14u, "'Hello, World!'", [|  |]) |] ))
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for multiple string token``() =
         let stream = [
              Token.String(0u, 14u, "'Hello, World!'", [|  |])
              Token.String(14u, 25u, "'bye, bye!'", [|  |])
              Token.EOF(25u)
         ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(node, ASTNode.String(0u, 25u, [|
              Token.String(0u, 14u, "'Hello, World!'", [|  |])
              Token.String(14u, 25u, "'bye, bye!'", [|  |])
         |] ))
         Assert.True(rest.Length = 1)