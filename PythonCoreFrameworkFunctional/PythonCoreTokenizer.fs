
namespace PythonCoreFrameworkFunctional

open System

type Trivia =
    |   Empty
    |   WhiteSpace of uint32 * uint32
    |   Tabulator of uint32 * uint32
    |   Newline of uint32 * uint32 * char * char
    |   LineContinuation of uint32 * uint32 * char * char * char
    |   Comment of uint32 * uint32 * string

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
    |   PyArrow of uint * uint * Trivia array
    |   PyIndent
    |   PyDedent
    |   TypeComment of uint * uint * string * Trivia array
    
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
        |   PyWith(s, _ , _ )
        |   PyArrow(s, _ , _ )
        |   TypeComment(s, _ , _ , _ ) -> s
        |   _   ->  0u
        
type TokenStream = Token list

exception LexicalError of string * uint32
exception InteractiveNeedMoreInput

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
        
    type TokenizerStack<'T> =
        |  Empty
        |  Stack of 'T * TokenizerStack<'T>
        
        member s.Push x = Stack(x, s)
        
        member s.Pop =
            match s with
            |  Empty -> failwith "Underflow"
            |  Stack( t, _ ) -> t
            
        member s.Rest =
            match s with
            |  Empty -> Empty
            |  Stack( _ , r ) -> r
            
        member s.IEmpty =
            match s with
            |  Empty -> true
            |  _ -> false

    [<Struct>]
    type TokenizerState =
        {
            mutable ParenthesisLevelStack : TokenizerStack<char>
            mutable IndentStack : uint32 array
            mutable IndentLevel : uint32
            mutable Pending : int32
            mutable Index : int32
            mutable TokenStartPos : uint32
            mutable AtBOL : bool
            mutable TabSize : uint32
            mutable IsInteractiveMode : bool
            mutable SourceCode : char array
            mutable TriviaList : Trivia list
            mutable IsBlankLine : bool
            mutable IsDone : bool
        }
        with
            static member Init source =
                {
                    ParenthesisLevelStack = TokenizerStack.Empty
                    SourceCode = source
                    TabSize = 8ul
                    IsInteractiveMode = false
                    Pending = 0l
                    Index = 0l
                    TokenStartPos = 0ul
                    AtBOL = true
                    IndentStack = Array.zeroCreate 100
                    IndentLevel = 0ul
                    TriviaList = List.Empty
                    IsBlankLine = false
                    IsDone = false
                }
    
    let GetChar (state : byref<TokenizerState>) : Option<char> =
        if state.Index >= state.SourceCode.Length then
            Option.None
        else
            let current = state.Index
            state.Index <- state.Index + 1l
            Some(state.SourceCode.[current])
            
    let PeekChar (state : byref<TokenizerState>, step : int32) : Option<char> =
        if (state.Index + step) >= state.SourceCode.Length then
            Option.None
        else
            Some(state.SourceCode.[state.Index + step])
    
    let IsHexDigit (ch : char) : bool =
        match ch with
        | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
        | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
        | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' ->    true
        | _ -> false
        
    let IsOctetDigit (ch : char) : bool =
        match ch with
        | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' ->    true
        | _ ->    false
        
    let IsBoolDigit (ch : char) : bool =
        match ch with
        | '0' | '1' ->    true
        | _ ->    false
                        
    let IsStartLetter (ch: char) : bool =
         Char.IsLetter (ch) || ch = '_'
         
    let IsLetterOrDigit (ch: char) : bool =
         Char.IsLetterOrDigit (ch) || ch = '_'

    let TriviaKeeping(state : byref<TokenizerState>, steps : int32) : Trivia array =
            state.Index <- state.Index + steps
            let trivia = List.toArray(List.rev state.TriviaList)
            state.TriviaList <- List.Empty
            state.TokenStartPos <- (uint32)state.Index
            trivia
            
    let rec OperatorOrDelimiters (state : byref<TokenizerState>) : Token option =
        let ch1 = match GetChar(&state) with | Some(a) -> a | Option.None -> (char)0x00 
        let ch2 = match PeekChar(&state, 0) with | Some(a) -> a | Option.None -> (char)0x00
        let ch3 = match PeekChar(&state, 1) with | Some(a) -> a | Option.None -> (char)0x00
        let _TokenStartPos = state.TokenStartPos
        
        match ch1, ch2, ch3 with
        |    '*', '*', '=' ->
                let trivia = TriviaKeeping(&state, 2)
                Some(Token.PyPowerAssign(_TokenStartPos, (uint32)state.Index, trivia))
        |    '*', '*', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyPower(_TokenStartPos, (uint32)state.Index, trivia))
        |    '*', '=', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyMulAssign(_TokenStartPos, (uint32)state.Index, trivia))
        |    '*', _ , _ ->
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyMul(_TokenStartPos, (uint32)state.Index, trivia))
        |    '/', '/', '=' ->
                let trivia = TriviaKeeping(&state, 2)
                Some(Token.PyFloorDivAssign(_TokenStartPos, (uint32)state.Index, trivia))
        |    '/', '/', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyFloorDiv(_TokenStartPos, (uint32)state.Index, trivia))
        |    '/', '=', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyDivAssign(_TokenStartPos, (uint32)state.Index, trivia))
        |    '/', _ , _ ->
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyDiv(_TokenStartPos, (uint32)state.Index, trivia))
        |    '<', '<', '=' ->
                let trivia = TriviaKeeping(&state, 2)
                Some(Token.PyShiftLeftAssign(_TokenStartPos, (uint32)state.Index, trivia))
        |    '<', '<', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyShiftLeft(_TokenStartPos, (uint32)state.Index, trivia))
        |    '<', '>', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyNotEqual(_TokenStartPos, (uint32)state.Index, trivia))
        |    '<', '=', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyLessEqual(_TokenStartPos, (uint32)state.Index, trivia))
        |    '<', _ , _ ->
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyLess(_TokenStartPos, (uint32)state.Index, trivia))
        |    '>', '>', '=' ->
                let trivia = TriviaKeeping(&state, 2)
                Some(Token.PyShiftRightAssign(_TokenStartPos, (uint32)state.Index, trivia))
        |    '>', '>', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyShiftRight(_TokenStartPos, (uint32)state.Index, trivia))
        |    '>', '=', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyGreaterEqual(_TokenStartPos, (uint32)state.Index, trivia))
        |    '>', _ , _ ->
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyGreater(_TokenStartPos, (uint32)state.Index, trivia))
        |    '.', '.', '.'  ->
                let trivia = TriviaKeeping(&state, 2)
                Some(Token.PyEllipsis(_TokenStartPos, (uint32)state.Index, trivia))
        |    '.', _ , _ ->
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyDot(_TokenStartPos, (uint32)state.Index, trivia))
        |    '+', '=', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyPlusAssign(_TokenStartPos, (uint32)state.Index, trivia))
        |    '+', _ , _ ->
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyPlus(_TokenStartPos, (uint32)state.Index, trivia))
        |    '-', '=', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyMinusAssign(_TokenStartPos, (uint32)state.Index, trivia))
        |    '-', '>', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyArrow(_TokenStartPos, (uint32)state.Index, trivia))
        |    '-', _ , _ ->
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyMinus(_TokenStartPos, (uint32)state.Index, trivia))
        |    '%', '=', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyModuloAssign(_TokenStartPos, (uint32)state.Index, trivia))
        |    '%', _ , _ ->
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyModulo(_TokenStartPos, (uint32)state.Index, trivia))
        |    '@', '=', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyMatriceAssign(_TokenStartPos, (uint32)state.Index, trivia))
        |    '@', _ , _ ->
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyMatrice(_TokenStartPos, (uint32)state.Index, trivia))
        |    '=', '=', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyEqual(_TokenStartPos, (uint32)state.Index, trivia))
        |    '=', _ , _ ->
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyAssign(_TokenStartPos, (uint32)state.Index, trivia))
        |    '&', '=', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyBitAndAssign(_TokenStartPos, (uint32)state.Index, trivia))
        |    '&', _ , _ ->
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyBitAnd(_TokenStartPos, (uint32)state.Index, trivia))
        |    '|', '=', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyBitOrAssign(_TokenStartPos, (uint32)state.Index, trivia))
        |    '|', _ , _ ->
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyBitOr(_TokenStartPos, (uint32)state.Index, trivia))
        |    '^', '=', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyBitXorAssign(_TokenStartPos, (uint32)state.Index, trivia))
        |    '^', _ , _ ->
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyBitXor(_TokenStartPos, (uint32)state.Index, trivia))
        |    ':', '=', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyColonAssign(_TokenStartPos, (uint32)state.Index, trivia))
        |    ':', _ , _ ->
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyColon(_TokenStartPos, (uint32)state.Index, trivia))
        |    '!', '=', _ ->
                let trivia = TriviaKeeping(&state, 1)
                Some(Token.PyNotEqual(_TokenStartPos, (uint32)state.Index, trivia))
        |    '~', _ , _ ->
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyBitInvert(_TokenStartPos, (uint32)state.Index, trivia))
        |    ',', _ , _ ->
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyComma(_TokenStartPos, (uint32)state.Index, trivia))
        |    ';', _ , _ ->
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PySemiColon(_TokenStartPos, (uint32)state.Index, trivia))
        |    '(', _ , _ ->
                state.ParenthesisLevelStack <- state.ParenthesisLevelStack.Push (ch1)
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyLeftParen(_TokenStartPos, (uint32)state.Index, trivia))
        |    '[', _ , _ ->
                state.ParenthesisLevelStack <- state.ParenthesisLevelStack.Push (ch1)
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyLeftBracket(_TokenStartPos, (uint32)state.Index, trivia))
        |    '{', _ , _ ->
                state.ParenthesisLevelStack <- state.ParenthesisLevelStack.Push (ch1)
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.PyLeftCurly(_TokenStartPos, (uint32)state.Index, trivia))
        |    ')', _ , _ ->
                if state.ParenthesisLevelStack.IEmpty = false && state.ParenthesisLevelStack.Pop = '(' then
                    let trivia = TriviaKeeping(&state, 0)
                    state.ParenthesisLevelStack <- state.ParenthesisLevelStack.Rest
                    Some(Token.PyRightParen(_TokenStartPos, (uint32)state.Index, trivia))
                else
                    Option.None
        |    ']', _ , _ ->
                if state.ParenthesisLevelStack.IEmpty = false && state.ParenthesisLevelStack.Pop = '[' then
                    let trivia = TriviaKeeping(&state, 0)
                    state.ParenthesisLevelStack <- state.ParenthesisLevelStack.Rest
                    Some(Token.PyRightBracket(_TokenStartPos, (uint32)state.Index, trivia))
                else
                    Option.None
        |    '}', _ , _ ->
                if state.ParenthesisLevelStack.IEmpty = false && state.ParenthesisLevelStack.Pop = '{' then
                    let trivia = TriviaKeeping(&state, 0)
                    state.ParenthesisLevelStack <- state.ParenthesisLevelStack.Rest
                    Some(Token.PyRightCurly(_TokenStartPos, (uint32)state.Index, trivia))
                else
                    Option.None
        |    _ , _ , _ ->
                state.Index <- state.Index - 1
                Option.None

    and NumberLiteral (state : byref<TokenizerState>) : Token option =
        let _TokenStartPos = state.TokenStartPos
        match PeekChar(&state, 0), PeekChar(&state, 1) with
        |  Some(x), Some(y) when x = '.' && Char.IsDigit(y) = false ->
                Option.None
        |  Some(x), _ when x = '0' ->        // Number starting with zero
                state.Index <- state.Index + 1
                
                match PeekChar(&state, 0) with
                | Some(x) when x = 'x' || x = 'X' ->
                        state.Index <- state.Index + 1
                        
                        while match PeekChar(&state, 0) with
                              | Some(x) when IsHexDigit(x) ->
                                   state.Index <- state.Index + 1
                                   match PeekChar(&state, 0) with
                                   |  Some(x) when x = '_' ->
                                           state.Index <- state.Index + 1
                                           match PeekChar(&state, 0) with | Some(x) when IsHexDigit(x) = false -> raise (LexicalError("Expecting digit after '_' in Number!", (uint32)state.Index)) | _ -> ()
                                   |  _ -> ()
                                   true
                              | Some(x) when IsBoolDigit(x) = false -> raise (LexicalError("Expecting hex digit in hex decimal Number!", (uint32)state.Index))
                              | Some(_) -> false
                              | _ -> false
                            do ()    
                | Some(x) when x = 'o' || x = 'O' ->
                        state.Index <- state.Index + 1
                        
                        while match PeekChar(&state, 0) with
                              | Some(x) when IsOctetDigit(x) ->
                                   state.Index <- state.Index + 1
                                   match PeekChar(&state, 0) with
                                   |  Some(x) when x = '_' ->
                                           state.Index <- state.Index + 1
                                           match PeekChar(&state, 0) with | Some(x) when IsOctetDigit(x) = false -> raise (LexicalError("Expecting digit after '_' in Number!", (uint32)state.Index)) | _ -> ()
                                   |  _ -> ()
                                   true
                              | Some(x) when IsBoolDigit(x) = false -> raise (LexicalError("Expecting between '0' and '7' digit in octet Number!", (uint32)state.Index))
                              | Some(_) -> false
                              | _ -> false
                            do ()    
                | Some(x) when x = 'b' || x = 'B' ->
                        state.Index <- state.Index + 1
                        
                        while match PeekChar(&state, 0) with
                              | Some(x) when IsBoolDigit(x) ->
                                   state.Index <- state.Index + 1
                                   match PeekChar(&state, 0) with
                                   |  Some(x) when x = '_' ->
                                           state.Index <- state.Index + 1
                                           match PeekChar(&state, 0) with | Some(x) when IsBoolDigit(x) = false -> raise (LexicalError("Expecting digit after '_' in Number!", (uint32)state.Index)) | _ -> ()
                                   |  _ -> ()
                                   true
                              | Some(x) when IsBoolDigit(x) = false -> raise (LexicalError("Expecting '0' or '1' digit in binary Number!", (uint32)state.Index))
                              | Some(_) -> false
                              | _ -> false
                            do ()       
                | Some(_) ->        // Decimal number
                        let mutable nonZero = false
                        
                        match PeekChar(&state, 0) with
                        |  Some(x) when x <> '.' ->
                                while   match PeekChar(&state, 0) with
                                        | Some(x) when x = '0' ->
                                                state.Index <- state.Index + 1
                                                true
                                        | Some(x) when Char.IsDigit(x) ->
                                                nonZero <- true
                                                state.Index <- state.Index + 1
                                                true
                                        | _ ->  false
                                     do ()
                        |  _ -> ()

                        match PeekChar(&state, 0) with
                        | Some(x) when x = '.' ->
                              state.Index <- state.Index + 1
                              while match PeekChar(&state, 0) with
                                    | Some(x) when Char.IsDigit(x) ->
                                           state.Index <- state.Index + 1
                                           match PeekChar(&state, 0) with
                                           |  Some(x) when x = '_' ->
                                                   state.Index <- state.Index + 1
                                                   match PeekChar(&state, 0) with | Some(x) when Char.IsDigit(x) = false -> raise (LexicalError("Expecting digit after '_' in Number!", (uint32)state.Index)) | _ -> ()
                                           |  _ -> ()
                                           true
                                    | Some(_) -> false
                                    | _ -> false
                                do ()
                        | _ -> ()
                        
                        match PeekChar(&state, 0) with
                        | Some(x) when x = 'e' || x = 'E' ->
                                state.Index <- state.Index + 1
                                
                                match PeekChar(&state, 0) with
                                | Some(x) when x = '+' || x = '-' ->
                                       state.Index <- state.Index + 1
                                       match PeekChar(&state, 0) with | Some(x) when Char.IsDigit(x) = false -> raise (LexicalError("Expecting digit after '+' or '-' in exponent part of Number!", (uint32)state.Index)) | _ -> ()
                                | _ -> ()
                                
                                while match PeekChar(&state, 0) with
                                      | Some(x) when Char.IsDigit(x) ->
                                           state.Index <- state.Index + 1
                                           match PeekChar(&state, 0) with
                                           |  Some(x) when x = '_' ->
                                                   state.Index <- state.Index + 1
                                                   match PeekChar(&state, 0) with | Some(x) when Char.IsDigit(x) = false -> raise (LexicalError("Expecting digit after '_' in Number!", (uint32)state.Index)) | _ -> ()
                                           |  _ -> ()
                                           true
                                      | Some(_) -> false
                                      | _ -> false
                                    do ()
                        | _ -> ()
                        
                        match PeekChar(&state, 0) with
                        | Some(x) when x = 'j' || x = 'J' ->
                                state.Index <- state.Index + 1
                        | _ -> ()
                        
                | Option.None -> ()
                
                let text = System.String(state.SourceCode.[(int32)_TokenStartPos..state.Index])
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.Number(_TokenStartPos, (uint32)state.Index, text, trivia))
        |  Some(x), _  when (x >= '1' && x <= '9') || x = '.' ->                     // Number starting with digit other than zero. Possibly '.'
                if x <> '.' then
                     while match PeekChar(&state, 0) with
                           | Some(x) when Char.IsDigit(x) ->
                                   state.Index <- state.Index + 1
                                   match PeekChar(&state, 0) with
                                   |  Some(x) when x = '_' ->
                                           state.Index <- state.Index + 1
                                           match PeekChar(&state, 0) with | Some(x) when Char.IsDigit(x) = false -> raise (LexicalError("Expecting digit after '_' in Number!", (uint32)state.Index)) | _ -> ()
                                   |  _ -> ()
                                   true
                           | Some(_) -> false
                           | _ -> false
                        do ()
                        
                match PeekChar(&state, 0) with
                | Some(x) when x = '.' ->
                      state.Index <- state.Index + 1
                      while match PeekChar(&state, 0) with
                            | Some(x) when Char.IsDigit(x) ->
                                   state.Index <- state.Index + 1
                                   match PeekChar(&state, 0) with
                                   |  Some(x) when x = '_' ->
                                           state.Index <- state.Index + 1
                                           match PeekChar(&state, 0) with | Some(x) when Char.IsDigit(x) = false -> raise (LexicalError("Expecting digit after '_' in Number!", (uint32)state.Index)) | _ -> ()
                                   |  _ -> ()
                                   true
                            | Some(_) -> false
                            | _ -> false
                        do ()
                | _ -> ()
                
                match PeekChar(&state, 0) with
                | Some(x) when x = 'e' || x = 'E' ->
                        state.Index <- state.Index + 1
                        
                        match PeekChar(&state, 0) with
                        | Some(x) when x = '+' || x = '-' ->
                               state.Index <- state.Index + 1
                               match PeekChar(&state, 0) with | Some(x) when Char.IsDigit(x) = false -> raise (LexicalError("Expecting digit after '+' or '-' in exponent part of Number!", (uint32)state.Index)) | _ -> ()
                        | _ -> ()
                        
                        while match PeekChar(&state, 0) with
                              | Some(x) when Char.IsDigit(x) ->
                                   state.Index <- state.Index + 1
                                   match PeekChar(&state, 0) with
                                   |  Some(x) when x = '_' ->
                                           state.Index <- state.Index + 1
                                           match PeekChar(&state, 0) with | Some(x) when Char.IsDigit(x) = false -> raise (LexicalError("Expecting digit after '_' in Number!", (uint32)state.Index)) | _ -> ()
                                   |  _ -> ()
                                   true
                              | Some(_) -> false
                              | _ -> false
                            do ()
                | _ -> ()
                
                match PeekChar(&state, 0) with
                | Some(x) when x = 'j' || x = 'J' ->
                        state.Index <- state.Index + 1
                | _ -> ()
                
                let text = System.String(state.SourceCode.[(int32)_TokenStartPos..state.Index - 1])
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.Number(_TokenStartPos, (uint32)state.Index, text, trivia))
        |  _ ->
                Option.None

    and StringLiteral (state : byref<TokenizerState>, tokenStart : uint32) : Token option =
        let _TokenStartPos = tokenStart
        match PeekChar(&state, 0) with
        | Some(x1) when x1 = '\'' || x1 = '"' ->
                let x2 = match PeekChar(&state, 1) with | Some(x2) -> x2 | _ -> (char)0x00
                let x3 = match PeekChar(&state, 2) with | Some(x2) -> x2 | _ -> (char)0x00
                let isTriple = (x1 = x2) && (x2 = x3)
                let isEmpty = (x1 = x2) && not isTriple
                
                if isEmpty then
                     state.Index <- state.Index + 2
                else
                     if isTriple then
                          state.Index <- state.Index + 3
                     else
                          state.Index <- state.Index + 1
                     
                     while  match PeekChar(&state, 0) with
                            | Some(x4) when x4 <> x1 ->
                                    state.Index <- state.Index + 1
                                    true
                            | Some(x4) ->
                                    if isTriple then
                                        let x5 = match PeekChar(&state, 1) with | Some(x2) -> x2 | _ -> (char)0x00
                                        let x6 = match PeekChar(&state, 2) with | Some(x2) -> x2 | _ -> (char)0x00
                                        if (x4 = x5) && (x5 = x6) then
                                             state.Index <- state.Index + 3
                                             false
                                        else
                                             state.Index <- state.Index + 1
                                             true
                                    else
                                        state.Index <- state.Index + 1
                                        false 
                            | Option.None ->  false
                        do ()
                     
                let text = System.String(state.SourceCode.[(int32)_TokenStartPos..state.Index - 1])
                let trivia = TriviaKeeping(&state, 0)
                Some(Token.String(_TokenStartPos, (uint32)state.Index, text, trivia))
        | _ ->
                Option.None

    and KeywordOrNameLiteral (state : byref<TokenizerState>) : Token option =
        let _TokenStartPos = state.TokenStartPos
        
        match   IsStartLetter (match PeekChar(&state, 0) with | Some(x) -> x | _ -> ' ') with
        | true ->
        
                while IsLetterOrDigit (match PeekChar(&state, 0) with | Some(x) -> x | _ -> ' ') do
                        state.Index <- state.Index + 1
                        
                let text = System.String(state.SourceCode.[(int32)_TokenStartPos..state.Index - 1])
                        
                if Keywords.ContainsKey text then
                        
                        let trivia = TriviaKeeping(&state, 0)
                        Some(Keywords.[text](_TokenStartPos, (uint32)state.Index, trivia))
                else
                        match PeekChar(&state, 0) with
                        | Some(x) when x = '\'' || x = '"' ->
                                match text with
                                | "r"
                                | "u"
                                | "R"
                                | "U"
                                | "f"
                                | "F"
                                | "fr"
                                | "Fr"
                                | "fR"
                                | "FR"
                                | "rf"
                                | "rF"
                                | "Rf"
                                | "RF" ->
                                        StringLiteral(&state, _TokenStartPos)
                                | _ ->
                                      let trivia = TriviaKeeping(&state, 0)
                                      Some(Token.Name(_TokenStartPos, (uint32)state.Index, text, trivia))
                        | _ ->    
                                let trivia = TriviaKeeping(&state, 0)
                                Some(Token.Name(_TokenStartPos, (uint32)state.Index, text, trivia))
        | _ ->
                Option.None

    and TypeCommentOrComment (state : byref<TokenizerState>) : Token option =
        let _TokenStartPos = state.TokenStartPos
        match PeekChar(&state, 0) with
        |    Some(x) ->
                match x with
                | '#' ->
                        state.Index <- state.Index + 1
                        while   match PeekChar(&state, 0) with
                                | Some( x ) when x <> '\r' && x <> '\n'  ->
                                        state.Index <- state.Index + 1
                                        true
                                | Some(_) -> false
                                | Option.None -> false
                                do ()
                        let text = System.String(state.SourceCode.[(int32)_TokenStartPos..state.Index - 1])
                        if text.StartsWith("# type: ") then
                             let trivia = TriviaKeeping(&state, 0)
                             Some(Token.TypeComment(_TokenStartPos, (uint32)state.Index, text, trivia))
                        else
                             state.TriviaList <- Trivia.Comment(_TokenStartPos, (uint32)state.Index, text) :: state.TriviaList
                             Some(InnerLoop(&state)) // Recursive call innerloop again!
                |   _ -> Option.None
        |   _ ->
                Option.None

    and NewlineHandling (state : byref<TokenizerState>) : Token option =
         let _TokenStartPos = state.TokenStartPos
         match PeekChar(&state, 0) with
         | Some(x) when x = '\r' || x = '\n' ->
                 match PeekChar(&state, 0), PeekChar(&state, 1) with
                 |  Some(x), Some(y) ->
                        match x, y with
                        | '\r', '\n' ->     state.Index <- state.Index + 2l
                        | '\r', _ ->        state.Index <- state.Index + 1l
                        | '\n', _ ->        state.Index <- state.Index + 1l
                        | _ -> ()
                        state.AtBOL <- true
                        if state.IsBlankLine || state.ParenthesisLevelStack.IEmpty = false then
                              state.TriviaList <- Trivia.Newline(_TokenStartPos, (uint32)state.Index, x, y) :: state.TriviaList
                              Some(OuterLoop(&state)) // Recursive call outerloop again!
                        else
                              let trivia = TriviaKeeping(&state, 0)
                              Some(Token.Newline(_TokenStartPos, (uint32)state.Index, x, y, trivia))
                 |  Some(x), _ ->
                         match x with
                         | '\r' | '\n' ->
                                 state.Index <- state.Index + 1l
                                 if state.IsBlankLine || not state.ParenthesisLevelStack.IEmpty then
                                      state.TriviaList <- Trivia.Newline(_TokenStartPos, (uint32)state.Index, x, ' ') :: state.TriviaList
                                      Some(OuterLoop(&state)) // Recursive call outerloop again!
                                 else
                                      let trivia = TriviaKeeping(&state, 0)
                                      Some(Token.Newline(_TokenStartPos, (uint32)state.Index, x, ' ', trivia))
                         | _ ->  Option.None
                 | _ ->
                        Option.None
         | _ ->
                 Option.None
                
    and EndOfFileHandling (state : byref<TokenizerState>) : Token option =
         match PeekChar(&state, 0) with
         | Option.None ->
                 if state.IsInteractiveMode && not state.IsDone then raise (InteractiveNeedMoreInput)
                 let trivia = TriviaKeeping(&state, 0)
                 Some(Token.EOF((uint32)state.Index))
         | _ -> Option.None
         
    and WhiteSpaceHandling (state : byref<TokenizerState>) : Token option =
         let _TokenStartPos = state.TokenStartPos
         match PeekChar(&state, 0) with
         | Some(x) when x = ' ' || x = '\t' ->
                 while match PeekChar(&state, 0) with
                       | Some(x) when x = ' ' ->
                               state.Index <- state.Index + 1
                               state.TriviaList <- Trivia.WhiteSpace(_TokenStartPos, (uint32)state.Index) :: state.TriviaList
                               true
                        | Some(x) when x = '\t' ->
                                state.Index <- state.Index + 1
                                state.TriviaList <- Trivia.Tabulator(_TokenStartPos, (uint32)state.Index) :: state.TriviaList
                                true
                       | Some(_) -> false
                       | _ -> false
                       do ()
                 
                 state.TokenStartPos <- (uint32)state.Index
                 Some(InnerLoop(&state)) // Recursive call innerloop again!
         | _ ->
                 Option.None
         
    and LineContinuationHandling (state : byref<TokenizerState>) : Token Option =
         let _TokenStartPos = state.TokenStartPos
         match PeekChar(&state, 0) with
         | Some(x) ->
                 match x with
                 | '\\' ->
                      state.Index <- state.Index + 1
                      match PeekChar(&state, 0), PeekChar(&state, 1) with
                      | Some(y), Some(z) ->
                            match y, z with
                            | '\r', '\n'
                            | '\r', _
                            | '\n', _ ->
                                  if z = '\n' then state.Index <- state.Index + 1
                                  state.Index <- state.Index + 1
                                  state.TriviaList <- Trivia.LineContinuation(_TokenStartPos, (uint32)state.Index, '\\', y, z) :: state.TriviaList
                                  state.TokenStartPos <- (uint32)state.Index
                                  Some(InnerLoop(&state)) // Recursive call innerloop again!
                            | _ ->
                                  raise (LexicalError("Expecting newline after line continuation character '\\'!", (uint32)state.Index ))
                      | Some(x), Option.None ->
                            match x with
                            | '\r' | '\n' ->
                                  state.Index <- state.Index + 1
                                  state.TriviaList <- Trivia.LineContinuation(_TokenStartPos, (uint32)state.Index, '\\', x, ' ') :: state.TriviaList  
                                  state.TokenStartPos <- (uint32)state.Index
                                  Some(InnerLoop(&state)) // Recursive call innerloop again!
                            | _ ->
                                  raise (LexicalError("Expecting newline after line continuation character '\\'!", (uint32)state.Index ))
                      | _ ->
                            Option.None
                 | _ ->
                      Option.None
         | _ ->
                Option.None
               
    and PendingIndentationLevel (state : byref<TokenizerState>) : Token option =
         state.TokenStartPos <- (uint32)state.Index
         match state.Pending with
         |  _ when state.Pending < 0 ->
                state.Pending <- state.Pending + 1
                Some(Token.PyDedent)
         |  _ when state.Pending > 0 ->
                state.Pending <- state.Pending - 1
                Some(Token.PyIndent)
         |  _ ->
                Option.None
                
    and AnalyzeIndentationLevel (state : byref<TokenizerState>) =
          match state.AtBOL with
          | true ->
                 state.AtBOL <- false
                 let mutable col = 0ul
                 
                 while  match PeekChar(&state, 0) with
                        | Some(' ') ->
                                col <- col + 1ul
                                true
                        | Some('\t') ->
                                col <- (col / state.TabSize + 1ul) *state.TabSize
                                true
                        | Some('\f') ->
                                col <- 0ul
                                true
                        | _ -> false
                        do
                                GetChar(&state) |> ignore
                 
                 match PeekChar(&state, 0) with
                 | Some('#')
                 | Some('\r')
                 | Some('\n')
                 | Some('\\') ->
                         if col = 0ul && state.IsInteractiveMode then
                              match PeekChar(&state, 0) with
                              | Some('\r')
                              | Some('\n') ->
                                   state.IsBlankLine <- false
                              | _ ->
                                   ()
                         elif state.IsInteractiveMode then
                              state.IsBlankLine <- false
                              col <- 0ul
                         else
                              state.IsBlankLine <- true
                 | _ ->  ()
                 
                 if state.IsBlankLine = false && state.ParenthesisLevelStack.IEmpty then
                      if col = state.IndentStack.[(int32)state.IndentLevel] then
                          () // No change in indentation level
                      elif col > state.IndentStack.[(int32)state.IndentLevel] then
                          if state.IndentLevel >= 99ul then
                               raise (LexicalError("Maximum 100 level of indentation allowed!", (uint32)state.Index))
                          state.IndentLevel <- state.IndentLevel + 1ul
                          state.IndentStack.[(int32)state.IndentLevel] <- col
                          state.Pending <- state.Pending + 1l
                      else
                           while state.IndentLevel > 0ul && col < state.IndentStack.[(int32)state.IndentLevel] do
                                state.IndentLevel <- state.IndentLevel - 1ul
                                state.Pending <- state.Pending - 1l
                           if col <> state.IndentStack.[(int32)state.IndentLevel] then
                                raise (LexicalError("Inconsistant indenation level!", (uint32)state.Index))
          | _ ->
                ()
                        
    and InnerLoop (state : byref<TokenizerState>) =
        match WhiteSpaceHandling(&state) with
        | Some(x) -> x
        | _ ->
             match TypeCommentOrComment(&state) with
             | Some(x) -> x
             | _ ->
                 match EndOfFileHandling(&state) with
                 | Some(x) -> x
                 | _ ->
                        match KeywordOrNameLiteral(&state) with
                        | Some(x) -> x
                        | _ ->
                                match NewlineHandling(&state) with
                                | Some(x) -> x
                                | _ ->
                                        match NumberLiteral(&state) with
                                        | Some(x) -> x
                                        | _ ->
                                                match StringLiteral(&state, (uint32)state.Index) with
                                                | Some(x) -> x
                                                | _ ->
                                                        match LineContinuationHandling(&state) with
                                                        | Some(x) -> x
                                                        | _ ->
                                                                match OperatorOrDelimiters(&state) with
                                                                | Some(x) -> x
                                                                | _ ->
                                                                        raise (LexicalError("Expected a valid Token!", (uint32)state.Index))
    
    and OuterLoop (state : byref<TokenizerState>) =
        AnalyzeIndentationLevel(&state)
        match PendingIndentationLevel(&state) with
        | Some(x) -> x
        | _ ->
                InnerLoop(&state)
                
    and TokenizeFromCharArray (text : char array) : TokenStream =
       let mutable state = TokenizerState.Init text
       let mutable tokenStream : Token list = []
       
       while    match OuterLoop(&state) with
                | Token.EOF(_) as x ->
                        tokenStream <- x :: tokenStream
                        false
                | x ->
                        tokenStream <- x :: tokenStream
                        true
           do ()
       
       List.rev tokenStream
       
    and TokenizeFromString (text: String) : TokenStream =
         text |> Seq.toArray |> TokenizeFromCharArray
