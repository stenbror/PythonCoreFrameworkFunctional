
namespace PythonCoreFrameworkFunctional

type Trivia =
    |   Empty

type Token =
    |   Empty
    |   EOF of uint
    |   PyFalse of uint * uint * Trivia array
    |   PyNone of uint * uint * Trivia array
    |   PyTrue of uint * uint * Trivia array
    |   PyEllipsis of uint * uint * Trivia array
    |   Name of uint * uint * string * Trivia array
    |   Number of uint * uint * string * Trivia array
    |   String of uint * uint * string * Trivia array
    |   PyLeftParen of uint * uint * Trivia array
    |   PyLeftBracket of uint * uint * Trivia array
    |   PyLeftCurly of uint * uint * Trivia array
    |   PyRightParen of uint * uint * Trivia array
    |   PyRightBracket of uint * uint * Trivia array
    |   PyRightCurly of uint * uint * Trivia array
    |   PyAwait of uint * uint * Trivia array
    |   PyDot of uint * uint * Trivia array
    |   PyPower of uint * uint * Trivia array
    |   PyPlus of uint * uint * Trivia array
    |   PyMinus of uint * uint * Trivia array
    |   PyBitInvert of uint * uint * Trivia array
    |   PyMul of uint * uint * Trivia array
    |   PyDiv of uint * uint * Trivia array
    |   PyFloorDiv of uint * uint * Trivia array
    |   PyMatrice of uint * uint * Trivia array
    |   PyModulo of uint * uint * Trivia array
    |   PyShiftLeft of uint * uint * Trivia array
    |   PyShiftRight of uint * uint * Trivia array
    |   PyBitAnd of uint * uint * Trivia array
    |   PyBitXor of uint * uint * Trivia array
    |   PyBitOr of uint * uint * Trivia array
    |   PyLess of uint * uint * Trivia array
    |   PyLessEqual of uint * uint * Trivia array
    |   PyEqual of uint * uint * Trivia array
    |   PyGreaterEqual of uint * uint * Trivia array
    |   PyGreater of uint * uint * Trivia array
    |   PyNotEqual of uint * uint * Trivia array
    |   PyIn of uint * uint * Trivia array
    |   PyNot of uint * uint * Trivia array
    |   PyIs of uint * uint * Trivia array
    |   PyAnd of uint * uint * Trivia array
    |   PyOr of uint * uint * Trivia array
    static member GetStartPosition(symbol: Token) : uint =
        match symbol with
        |   EOF(s)
        |   PyFalse(s, _ , _ )
        |   PyNone(s, _ , _ )
        |   PyTrue(s, _ , _ )
        |   PyEllipsis(s, _ , _)
        |   Name(s, _, _, _ )
        |   Number(s, _ , _ , _ )
        |   String(s, _ , _ , _ )
        |   PyLeftParen(s , _ , _ )
        |   PyLeftBracket(s, _ , _ )
        |   PyLeftCurly(s, _ , _ )
        |   PyLeftCurly(s, _ , _ )
        |   PyRightParen(s, _ , _ )
        |   PyRightBracket(s, _ , _ )
        |   PyRightCurly(s, _ , _ )
        |   PyAwait(s, _ , _ )
        |   PyDot(s, _ , _ )
        |   PyPower(s, _ , _ )
        |   PyPlus(s, _ , _ )
        |   PyMinus(s, _ , _)
        |   PyBitInvert(s, _ , _ )
        |   PyMul(s, _ , _ )
        |   PyDiv(s, _ , _ )
        |   PyFloorDiv(s, _ , _ )
        |   PyMatrice(s, _ , _ )
        |   PyModulo(s, _ , _ )
        |   PyShiftLeft(s, _ , _ )
        |   PyShiftRight(s, _ , _ )
        |   PyBitAnd(s, _ , _ )
        |   PyBitXor(s, _ , _ )
        |   PyBitOr(s, _ , _ )
        |   PyLess(s, _ , _ )
        |   PyLessEqual(s, _ , _ )
        |   PyEqual(s, _ , _ )
        |   PyGreaterEqual(s, _ , _ )
        |   PyGreater(s, _ , _ )
        |   PyIn(s, _ , _ )
        |   PyNot(s, _ , _ )
        |   PyIs(s, _ , _ )
        |   PyAnd(s, _ , _ )
        |   PyOr(s, _ , _ ) -> s
        |   _   ->  0u
        
type TokenStream = Token list

module PythonCoreTokenizer = ()


