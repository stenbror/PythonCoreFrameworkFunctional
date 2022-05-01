
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
         
    [<Fact>]
    let ``Test Atom Expression rule for atom expr with no additional items``() =
         let stream = [ Token.Name(0u, 3u, "abc", [|  |]); Token.EOF(3u) ]
         let node, rest = PythonCoreParser.ParseAtomExpr stream
         
         Assert.Equal(ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])), node)
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for atom expr with await``() =
         let stream = [ Token.PyAwait(0u, 5u, [|  |]); Token.Name(6u, 9u, "abc", [|  |]); Token.EOF(9u) ]
         let node, rest = PythonCoreParser.ParseAtomExpr stream
         
         Assert.Equal(ASTNode.AtomExpr(0u, 9u, Token.PyAwait(0u, 5u, [||]), ASTNode.Name(6u, 9u,
                                        Token.Name(6u, 9u, "abc", [|  |])), [||])
                                        , node)
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for empty power rule``() =
         let stream = [ Token.Name(0u, 3u, "abc", [|  |]); Token.EOF(3u) ]
         let node, rest = PythonCoreParser.ParsePower stream
         
         Assert.Equal(ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])), node)
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for power rule``() =
         let stream = [
              Token.Name(0u, 3u, "abc", [|  |])
              Token.PyPower(4u, 5u, [|  |])
              Token.Name(6u, 9u, "abc", [|  |])
              Token.EOF(9u)
         ]
         let node, rest = PythonCoreParser.ParsePower stream
         
         Assert.Equal(
                   ASTNode.Power(0u, 9u, 
                   ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])),
                   Token.PyPower(4u, 5u, [|  |]),
                   ASTNode.Name(6u, 9u, Token.Name(6u, 9u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
               
    [<Fact>]
    let ``Test Atom Expression rule for empty factor rule``() =
         let stream = [ Token.Name(0u, 3u, "abc", [|  |]); Token.EOF(3u) ]
         let node, rest = PythonCoreParser.ParseFactor stream
         
         Assert.Equal(ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])), node)
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for factor plus rule``() =
         let stream = [
              Token.PyPlus(0u, 1u, [|  |])
              Token.Name(2u, 5u, "abc", [|  |])
              Token.EOF(5u)
         ]
         let node, rest = PythonCoreParser.ParseFactor stream
         
         Assert.Equal(
                   ASTNode.UnaryPlus(0u, 5u, 
                   Token.PyPlus(0u, 1u, [|  |]),
                   ASTNode.Name(2u, 5u, Token.Name(2u, 5u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for factor minus rule``() =
         let stream = [
              Token.PyMinus(0u, 1u, [|  |])
              Token.Name(2u, 5u, "abc", [|  |])
              Token.EOF(5u)
         ]
         let node, rest = PythonCoreParser.ParseFactor stream
         
         Assert.Equal(
                   ASTNode.UnaryMinus(0u, 5u, 
                   Token.PyMinus(0u, 1u, [|  |]),
                   ASTNode.Name(2u, 5u, Token.Name(2u, 5u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for factor bit invert rule``() =
         let stream = [
              Token.PyBitInvert(0u, 1u, [|  |])
              Token.Name(2u, 5u, "abc", [|  |])
              Token.EOF(5u)
         ]
         let node, rest = PythonCoreParser.ParseFactor stream
         
         Assert.Equal(
                   ASTNode.UnaryBitInvert(0u, 5u, 
                   Token.PyBitInvert(0u, 1u, [|  |]),
                   ASTNode.Name(2u, 5u, Token.Name(2u, 5u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for empty term rule``() =
         let stream = [ Token.Name(0u, 3u, "abc", [|  |]); Token.EOF(3u) ]
         let node, rest = PythonCoreParser.ParseTerm stream
         
         Assert.Equal(ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])), node)
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for term rule with mul op``() =
         let stream = [
              Token.Name(0u, 3u, "abc", [|  |])
              Token.PyMul(4u, 5u, [|  |])
              Token.Name(6u, 9u, "abc", [|  |])
              Token.EOF(9u)
         ]
         let node, rest = PythonCoreParser.ParseTerm stream
         
         Assert.Equal(
                   ASTNode.Mul(0u, 9u, 
                   ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])),
                   Token.PyMul(4u, 5u, [|  |]),
                   ASTNode.Name(6u, 9u, Token.Name(6u, 9u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for term rule with div op``() =
         let stream = [
              Token.Name(0u, 3u, "abc", [|  |])
              Token.PyDiv(4u, 5u, [|  |])
              Token.Name(6u, 9u, "abc", [|  |])
              Token.EOF(9u)
         ]
         let node, rest = PythonCoreParser.ParseTerm stream
         
         Assert.Equal(
                   ASTNode.Div(0u, 9u, 
                   ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])),
                   Token.PyDiv(4u, 5u, [|  |]),
                   ASTNode.Name(6u, 9u, Token.Name(6u, 9u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for term rule with matrice op``() =
         let stream = [
              Token.Name(0u, 3u, "abc", [|  |])
              Token.PyMatrice(4u, 5u, [|  |])
              Token.Name(6u, 9u, "abc", [|  |])
              Token.EOF(9u)
         ]
         let node, rest = PythonCoreParser.ParseTerm stream
         
         Assert.Equal(
                   ASTNode.Matrice(0u, 9u, 
                   ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])),
                   Token.PyMatrice(4u, 5u, [|  |]),
                   ASTNode.Name(6u, 9u, Token.Name(6u, 9u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for term rule with modulo op``() =
         let stream = [
              Token.Name(0u, 3u, "abc", [|  |])
              Token.PyModulo(4u, 5u, [|  |])
              Token.Name(6u, 9u, "abc", [|  |])
              Token.EOF(9u)
         ]
         let node, rest = PythonCoreParser.ParseTerm stream
         
         Assert.Equal(
                   ASTNode.Modulo(0u, 9u, 
                   ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])),
                   Token.PyModulo(4u, 5u, [|  |]),
                   ASTNode.Name(6u, 9u, Token.Name(6u, 9u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for term rule with floor div op``() =
         let stream = [
              Token.Name(0u, 3u, "abc", [|  |])
              Token.PyFloorDiv(4u, 6u, [|  |])
              Token.Name(7u, 10u, "abc", [|  |])
              Token.EOF(10u)
         ]
         let node, rest = PythonCoreParser.ParseTerm stream
         
         Assert.Equal(
                   ASTNode.FloorDiv(0u, 10u, 
                   ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])),
                   Token.PyFloorDiv(4u, 6u, [|  |]),
                   ASTNode.Name(7u, 10u, Token.Name(7u, 10u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for term rule with double modulo op``() =
         let stream = [
              Token.Name(0u, 3u, "abc", [|  |])
              Token.PyModulo(4u, 5u, [|  |])
              Token.Name(6u, 9u, "abc", [|  |])
              Token.PyModulo(10u, 11u, [|  |])
              Token.Name(12u, 15u, "abc", [|  |])
              Token.EOF(15u)
         ]
         let node, rest = PythonCoreParser.ParseTerm stream
         
         Assert.Equal(
                   ASTNode.Modulo(0u, 15u, 
                        ASTNode.Modulo(0u, 10u, 
                             ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])),
                             Token.PyModulo(4u, 5u, [|  |]),
                             ASTNode.Name(6u, 9u, Token.Name(6u, 9u, "abc", [|  |]) )),
                        Token.PyModulo(10u, 11u, [|  |]),
                        ASTNode.Name(12u, 15u, Token.Name(12u, 15u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for empty arith rule``() =
         let stream = [ Token.Name(0u, 3u, "abc", [|  |]); Token.EOF(3u) ]
         let node, rest = PythonCoreParser.ParseArith stream
         
         Assert.Equal(ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])), node)
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for arith rule with plus op``() =
         let stream = [
              Token.Name(0u, 3u, "abc", [|  |])
              Token.PyPlus(4u, 5u, [|  |])
              Token.Name(6u, 9u, "abc", [|  |])
              Token.EOF(9u)
         ]
         let node, rest = PythonCoreParser.ParseArith stream
         
         Assert.Equal(
                   ASTNode.Plus(0u, 9u, 
                   ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])),
                   Token.PyPlus(4u, 5u, [|  |]),
                   ASTNode.Name(6u, 9u, Token.Name(6u, 9u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for arith rule with minus op``() =
         let stream = [
              Token.Name(0u, 3u, "abc", [|  |])
              Token.PyMinus(4u, 5u, [|  |])
              Token.Name(6u, 9u, "abc", [|  |])
              Token.EOF(9u)
         ]
         let node, rest = PythonCoreParser.ParseArith stream
         
         Assert.Equal(
                   ASTNode.Minus(0u, 9u, 
                   ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])),
                   Token.PyMinus(4u, 5u, [|  |]),
                   ASTNode.Name(6u, 9u, Token.Name(6u, 9u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for arith rule with double plus/minus op``() =
         let stream = [
              Token.Name(0u, 3u, "abc", [|  |])
              Token.PyPlus(4u, 5u, [|  |])
              Token.Name(6u, 9u, "abc", [|  |])
              Token.PyMinus(10u, 11u, [|  |])
              Token.Name(12u, 15u, "abc", [|  |])
              Token.EOF(15u)
         ]
         let node, rest = PythonCoreParser.ParseArith stream
         
         Assert.Equal(
                   ASTNode.Minus(0u, 15u, 
                        ASTNode.Plus(0u, 10u, 
                             ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])),
                             Token.PyPlus(4u, 5u, [|  |]),
                             ASTNode.Name(6u, 9u, Token.Name(6u, 9u, "abc", [|  |]) )),
                        Token.PyMinus(10u, 11u, [|  |]),
                        ASTNode.Name(12u, 15u, Token.Name(12u, 15u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
              
    [<Fact>]
    let ``Test Atom Expression rule for empty shift rule``() =
         let stream = [ Token.Name(0u, 3u, "abc", [|  |]); Token.EOF(3u) ]
         let node, rest = PythonCoreParser.ParseShift stream
         
         Assert.Equal(ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])), node)
         Assert.True(rest.Length = 1)    
         
    [<Fact>]
    let ``Test Atom Expression rule for shift rule with shift left op``() =
         let stream = [
              Token.Name(0u, 3u, "abc", [|  |])
              Token.PyShiftLeft(4u, 5u, [|  |])
              Token.Name(6u, 9u, "abc", [|  |])
              Token.EOF(9u)
         ]
         let node, rest = PythonCoreParser.ParseShift stream
         
         Assert.Equal(
                   ASTNode.ShiftLeft(0u, 9u, 
                   ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])),
                   Token.PyShiftLeft(4u, 5u, [|  |]),
                   ASTNode.Name(6u, 9u, Token.Name(6u, 9u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for shift rule with shift right op``() =
         let stream = [
              Token.Name(0u, 3u, "abc", [|  |])
              Token.PyShiftRight(4u, 5u, [|  |])
              Token.Name(6u, 9u, "abc", [|  |])
              Token.EOF(9u)
         ]
         let node, rest = PythonCoreParser.ParseShift stream
         
         Assert.Equal(
                   ASTNode.ShiftRight(0u, 9u, 
                   ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])),
                   Token.PyShiftRight(4u, 5u, [|  |]),
                   ASTNode.Name(6u, 9u, Token.Name(6u, 9u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for shift rule with double shiftleft/shiftright op``() =
         let stream = [
              Token.Name(0u, 3u, "abc", [|  |])
              Token.PyShiftLeft(4u, 5u, [|  |])
              Token.Name(6u, 9u, "abc", [|  |])
              Token.PyShiftRight(10u, 11u, [|  |])
              Token.Name(12u, 15u, "abc", [|  |])
              Token.EOF(15u)
         ]
         let node, rest = PythonCoreParser.ParseShift stream
         
         Assert.Equal(
                   ASTNode.ShiftRight(0u, 15u, 
                        ASTNode.ShiftLeft(0u, 10u, 
                             ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])),
                             Token.PyShiftLeft(4u, 5u, [|  |]),
                             ASTNode.Name(6u, 9u, Token.Name(6u, 9u, "abc", [|  |]) )),
                        Token.PyShiftRight(10u, 11u, [|  |]),
                        ASTNode.Name(12u, 15u, Token.Name(12u, 15u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for empty bit and rule``() =
         let stream = [ Token.Name(0u, 3u, "abc", [|  |]); Token.EOF(3u) ]
         let node, rest = PythonCoreParser.ParseAnd stream
         
         Assert.Equal(ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])), node)
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for shift rule with bit and right op``() =
         let stream = [
              Token.Name(0u, 3u, "abc", [|  |])
              Token.PyBitAnd(4u, 7u, [|  |])
              Token.Name(8u, 11u, "abc", [|  |])
              Token.EOF(11u)
         ]
         let node, rest = PythonCoreParser.ParseAnd stream
         
         Assert.Equal(
                   ASTNode.BitAnd(0u, 11u, 
                   ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])),
                   Token.PyBitAnd(4u, 7u, [|  |]),
                   ASTNode.Name(8u, 11u, Token.Name(8u, 11u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for shift rule with double bit and op``() =
         let stream = [
              Token.Name(0u, 3u, "abc", [|  |])
              Token.PyBitAnd(4u, 7u, [|  |])
              Token.Name(8u, 11u, "abc", [|  |])
              Token.PyBitAnd(12u, 15u, [|  |])
              Token.Name(16u, 19u, "abc", [|  |])
              Token.EOF(19u)
         ]
         let node, rest = PythonCoreParser.ParseAnd stream
         
         Assert.Equal(
                   ASTNode.BitAnd(0u, 19u, 
                        ASTNode.BitAnd(0u, 12u, 
                             ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])),
                             Token.PyBitAnd(4u, 7u, [|  |]),
                             ASTNode.Name(8u, 11u, Token.Name(8u, 11u, "abc", [|  |]) )),
                        Token.PyBitAnd(12u, 15u, [|  |]),
                        ASTNode.Name(16u, 19u, Token.Name(16u, 19u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for empty bit xor rule``() =
         let stream = [ Token.Name(0u, 3u, "abc", [|  |]); Token.EOF(3u) ]
         let node, rest = PythonCoreParser.ParseXor stream
         
         Assert.Equal(ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])), node)
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for shift rule with bit xor right op``() =
         let stream = [
              Token.Name(0u, 3u, "abc", [|  |])
              Token.PyBitXor(4u, 7u, [|  |])
              Token.Name(8u, 11u, "abc", [|  |])
              Token.EOF(11u)
         ]
         let node, rest = PythonCoreParser.ParseXor stream
         
         Assert.Equal(
                   ASTNode.BitXor(0u, 11u, 
                   ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])),
                   Token.PyBitXor(4u, 7u, [|  |]),
                   ASTNode.Name(8u, 11u, Token.Name(8u, 11u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)
         
    [<Fact>]
    let ``Test Atom Expression rule for shift rule with double xor and op``() =
         let stream = [
              Token.Name(0u, 3u, "abc", [|  |])
              Token.PyBitXor(4u, 7u, [|  |])
              Token.Name(8u, 11u, "abc", [|  |])
              Token.PyBitXor(12u, 15u, [|  |])
              Token.Name(16u, 19u, "abc", [|  |])
              Token.EOF(19u)
         ]
         let node, rest = PythonCoreParser.ParseXor stream
         
         Assert.Equal(
                   ASTNode.BitXor(0u, 19u, 
                        ASTNode.BitXor(0u, 12u, 
                             ASTNode.Name(0u, 3u, Token.Name(0u, 3u, "abc", [|  |])),
                             Token.PyBitXor(4u, 7u, [|  |]),
                             ASTNode.Name(8u, 11u, Token.Name(8u, 11u, "abc", [|  |]) )),
                        Token.PyBitXor(12u, 15u, [|  |]),
                        ASTNode.Name(16u, 19u, Token.Name(16u, 19u, "abc", [|  |]))
              ), node)
         
         Assert.True(rest.Length = 1)