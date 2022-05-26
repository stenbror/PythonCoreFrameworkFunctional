
namespace PythonCoreFrameworkFunctional


type Trivia =
    |   Empty

type Token =
    |   Empty
    |   EOF of uint
    |   Newline of uint * uint * char * char * Trivia array
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
    |   PyLambda of uint * uint * Trivia array
    |   PyIf of uint * uint * Trivia array
    |   PyElse of uint * uint * Trivia array
    |   PyColonAssign of uint * uint * Trivia array
    |   PyColon of uint * uint * Trivia array
    |   PyYield of uint * uint * Trivia array
    |   PyFrom of uint * uint * Trivia array
    |   PyComma of uint * uint * Trivia array
    |   PyPlusAssign of uint * uint * Trivia array
    |   PyMinusAssign of uint * uint * Trivia array
    |   PyMulAssign of uint * uint * Trivia array
    |   PyDivAssign of uint * uint * Trivia array
    |   PyMatriceAssign of uint * uint * Trivia array
    |   PyModuloAssign of uint * uint * Trivia array
    |   PyBitAndAssign of uint * uint * Trivia array
    |   PyBitOrAssign of uint * uint * Trivia array
    |   PyBitXorAssign of uint * uint * Trivia array
    |   PyShiftLeftAssign of uint * uint * Trivia array
    |   PyShiftRightAssign of uint * uint * Trivia array
    |   PyPowerAssign of uint * uint * Trivia array
    |   PyFloorDivAssign of uint * uint * Trivia array
    |   PyAssign of uint * uint * Trivia array
    |   PySemiColon of uint * uint * Trivia array
    |   PyFor of uint * uint * Trivia array
    |   PyAsync of uint * uint * Trivia array
    |   PyAs of uint * uint * Trivia array
    |   PyAssert of uint * uint * Trivia array
    |   PyBreak of uint * uint * Trivia array
    |   PyClass of uint * uint * Trivia array
    |   PyContinue of uint * uint * Trivia array
    |   PyDef of uint * uint * Trivia array
    |   PyDel of uint * uint * Trivia array
    |   PyElif of uint * uint * Trivia array
    |   PyExcept of uint * uint * Trivia array
    |   PyFinally of uint * uint * Trivia array
    |   PyGlobal of uint * uint * Trivia array
    |   PyImport of uint * uint * Trivia array
    |   PyNonLocal of uint * uint * Trivia array
    |   PyPass of uint * uint * Trivia array
    |   PyRaise of uint * uint * Trivia array
    |   PyReturn of uint * uint * Trivia array
    |   PyTry of uint * uint * Trivia array
    |   PyWhile of uint * uint * Trivia array
    |   PyWith of uint * uint * Trivia array
    
    static member GetStartPosition(symbol: Token) : uint =
        match symbol with
        |   EOF(s)
        |   Newline(s, _ , _ , _ , _ )
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
        |   PyOr(s, _ , _ )
        |   PyLambda(s, _ , _ )
        |   PyIf(s, _ , _ )
        |   PyElse(s, _ , _ )
        |   PyColonAssign(s, _ , _ )
        |   PyColon(s, _ , _ )
        |   PyYield(s, _ , _ )
        |   PyFrom(s, _ , _ )
        |   PyComma(s, _ , _ )
        |   PyPlusAssign(s, _ , _ )
        |   PyMinusAssign(s, _ , _ )
        |   PyMulAssign(s, _ , _ )
        |   PyDivAssign(s, _ , _ )
        |   PyModuloAssign(s, _ , _ )
        |   PyFloorDivAssign(s, _ , _ )
        |   PyBitAndAssign(s, _ , _ )
        |   PyBitOrAssign(s, _ , _ )
        |   PyBitXorAssign(s, _ , _ )
        |   PyShiftLeftAssign(s, _ , _ )
        |   PyShiftRightAssign(s, _ , _ )
        |   PyPowerAssign(s, _ , _ )
        |   PyAssign(s, _ , _ )
        |   PyAsync(s, _ , _ )
        |   PyFor(s, _ , _ )
        |   PyAs(s, _ , _ )
        |   PyAssert(s, _ , _ )
        |   PyBreak(s, _ , _ )
        |   PyClass(s, _ , _ )
        |   PyContinue(s, _ , _ )
        |   PyDef(s, _ , _ )
        |   PyDel(s, _ , _ ) 
        |   PyElif(s, _ , _ )
        |   PyExcept(s, _ , _ )
        |   PyFinally(s, _ , _ )
        |   PyGlobal(s, _ , _ )
        |   PyImport(s, _ , _ )
        |   PyNonLocal(s, _ , _ )
        |   PyPass(s, _ , _ )
        |   PyRaise(s, _ , _ )
        |   PyReturn(s, _ , _ )
        |   PyTry(s, _ , _ )
        |   PyWhile(s, _ , _ )
        |   PyWith(s, _ , _ ) -> s
        |   _   ->  0u
        
type TokenStream = Token list

module PythonCoreTokenizer =
    
    let Keywords =
        [
            ( "and", Token.PyAnd )
            ( "as", Token.PyAs )
            ( "assert", Token.PyAssert )
            ( "async", Token.PyAsync )
            ( "await", Token.PyAwait )
            ( "break", Token.PyBreak )
            ( "class", Token.PyClass )
            ( "continue", Token.PyContinue )
            ( "def", Token.PyDef )
            ( "del", Token.PyDel )
            ( "elif", Token.PyElif )
            ( "else", Token.PyElse )
            ( "except", Token.PyExcept )
            ( "finally", Token.PyFinally )
            ( "for", Token.PyFor )
            ( "from", Token.PyFrom )
            ( "global", Token.PyGlobal )
            ( "if", Token.PyIf )
            ( "import", Token.PyImport )
            ( "in", Token.PyIn )
            ( "is", Token.PyIs )
            ( "lambda", Token.PyLambda )
            ( "nonlocal", Token.PyNonLocal )
            ( "not", Token.PyNot )
            ( "or", Token.PyOr )
            ( "pass", Token.PyPass )
            ( "raise", Token.PyRaise )
            ( "return", Token.PyReturn )
            ( "try", Token.PyTry )
            ( "while", Token.PyWhile )
            ( "with", Token.PyWith )
            ( "yield", Token.PyYield )
            ( "False", Token.PyFalse )
            ( "None", Token.PyNone )
            ( "True", Token.PyTrue )
        ] |> Map.ofList
        
    
