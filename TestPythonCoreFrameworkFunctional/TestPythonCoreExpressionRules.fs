
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