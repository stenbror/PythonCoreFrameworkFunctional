
namespace PythonCoreFrameworkFunctional.Tests

open System
open Xunit
open PythonCoreFrameworkFunctional


module ExpressionsRulesTests =
    
    [<Fact>]
    let ``Test Atom Expression rule for False token``() =
         let stream = [ Token.PyFalse(0u, 5u, [|  |]); ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(ASTNode.False(0u, 5u, Token.PyFalse(0u, 5u, [|  |])), node)
         Assert.True(rest.Length = 0)
         
    [<Fact>]
    let ``Test Atom Expression rule for None token``() =
         let stream = [ Token.PyNone(0u, 4u, [|  |]); ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(ASTNode.None(0u, 4u, Token.PyNone(0u, 4u, [|  |])), node)
         Assert.True(rest.Length = 0)
         
    [<Fact>]
    let ``Test Atom Expression rule for True token``() =
         let stream = [ Token.PyTrue(0u, 4u, [|  |]); ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(ASTNode.True(0u, 4u, Token.PyTrue(0u, 4u, [|  |])), node)
         Assert.True(rest.Length = 0)
         
    [<Fact>]
    let ``Test Atom Expression rule for ... token``() =
         let stream = [ Token.PyEllipsis(0u, 3u, [|  |]); ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(ASTNode.Ellipsis(0u, 3u, Token.PyEllipsis(0u, 3u, [|  |])), node)
         Assert.True(rest.Length = 0)
         
    [<Fact>]
    let ``Test Atom Expression rule for name token``() =
         let stream = [ Token.Name(0u, 3u, "abc", [|  |]); ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])), node)
         Assert.True(rest.Length = 0)
         
    [<Fact>]
    let ``Test Atom Expression rule for number token``() =
         let stream = [ Token.Name(0u, 4u, "1.34", [|  |]); ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(ASTNode.Name(0u, 4u, Token.Name(0u, 4u, "1.34", [|  |])), node)
         Assert.True(rest.Length = 0)
         
    [<Fact>]
    let ``Test Atom Expression rule for single string token``() =
         let stream = [ Token.String(0u, 14u, "'Hello, World!'", [|  |]); Token.EOF(14u) ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(ASTNode.String(0u, 14u, [| Token.String(0u, 14u, "'Hello, World!'", [|  |]) |] ), node)
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for multiple string token``() =
         let stream = [
              Token.String(0u, 14u, "'Hello, World!'", [|  |])
              Token.String(14u, 25u, "'bye, bye!'", [|  |])
              Token.EOF(25u)
         ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(ASTNode.String(0u, 25u, [|
              Token.String(0u, 14u, "'Hello, World!'", [|  |])
              Token.String(14u, 25u, "'bye, bye!'", [|  |])
         |] ), node)
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for empty tuple``() =
         let stream = [ Token.PyLeftParen(0u, 1u, [|  |]); Token.PyRightParen(1u, 2u, [|  |]); Token.EOF(2u) ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(ASTNode.Tuple(0u, 2u, Token.PyLeftParen(0u, 1u, [||]), ASTNode.Empty, Token.PyRightParen(1u, 2u, [|  |]) ), node)
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for empty list``() =
         let stream = [ Token.PyLeftBracket(0u, 1u, [|  |]); Token.PyRightBracket(1u, 2u, [|  |]); Token.EOF(2u) ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(ASTNode.List(0u, 2u, Token.PyLeftBracket(0u, 1u, [||]), ASTNode.Empty, Token.PyRightBracket(1u, 2u, [|  |]) ), node)
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for empty dictionary``() =
         let stream = [ Token.PyLeftCurly(0u, 1u, [|  |]); Token.PyRightCurly(1u, 2u, [|  |]); Token.EOF(2u) ]
         let node, rest = PythonCoreParser.ParseAtom stream
         
         Assert.Equal(ASTNode.Dictionary(0u, 2u, Token.PyLeftCurly(0u, 1u, [||]), ASTNode.Empty, Token.PyRightCurly(1u, 2u, [|  |]) ), node)
         Assert.True(rest.Length = 1)