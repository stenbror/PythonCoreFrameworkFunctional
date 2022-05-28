
namespace PythonCoreFrameworkFunctional

open PythonCoreFrameworkFunctional

type ASTNode =
    |   Empty
    |   False of uint * uint * Token
    |   None of uint * uint * Token
    |   True of uint * uint * Token
    |   Ellipsis of uint * uint * Token
    |   Name of uint * uint * Token
    |   Number of uint * uint * Token
    |   String of uint * uint * Token array
    |   Tuple of uint * uint * Token * ASTNode * Token
    |   List of uint * uint * Token * ASTNode * Token
    |   Dictionary of uint * uint * Token * ASTNode * Token
    |   Set of uint * uint * Token * ASTNode * Token
    |   AtomExpr of uint * uint * Token * ASTNode * ASTNode array
    |   Power of uint * uint * ASTNode * Token * ASTNode
    |   UnaryPlus of uint * uint * Token * ASTNode
    |   UnaryMinus of uint * uint * Token * ASTNode
    |   UnaryBitInvert of uint * uint * Token * ASTNode
    |   Mul of uint * uint * ASTNode * Token * ASTNode
    |   Div of uint * uint * ASTNode * Token * ASTNode
    |   FloorDiv of uint * uint * ASTNode * Token * ASTNode
    |   Matrice of uint * uint * ASTNode * Token * ASTNode
    |   Modulo of uint * uint * ASTNode * Token * ASTNode
    |   Plus of uint * uint * ASTNode * Token * ASTNode
    |   Minus of uint * uint * ASTNode * Token * ASTNode
    |   ShiftLeft of uint * uint * ASTNode * Token * ASTNode
    |   ShiftRight of uint * uint * ASTNode * Token * ASTNode
    |   BitAnd of uint * uint * ASTNode * Token * ASTNode
    |   BitXor of uint * uint * ASTNode * Token * ASTNode
    |   BitOr of uint * uint * ASTNode * Token * ASTNode
    |   StarExpr of uint * uint * Token * ASTNode
    |   Less of uint * uint * ASTNode * Token * ASTNode
    |   LessEqual of uint * uint * ASTNode * Token * ASTNode
    |   Equal of uint * uint * ASTNode * Token * ASTNode
    |   GreaterEqual of uint * uint * ASTNode * Token * ASTNode
    |   Greater of uint * uint * ASTNode * Token * ASTNode
    |   NotEqual of uint * uint * ASTNode * Token * ASTNode
    |   In of uint * uint * ASTNode * Token * ASTNode
    |   NotIn of uint * uint * ASTNode * Token * Token * ASTNode
    |   Is of uint * uint * ASTNode * Token * ASTNode
    |   IsNot of uint * uint * ASTNode * Token * Token * ASTNode
    |   NotTest of uint * uint * Token * ASTNode
    |   AndTest of uint * uint * ASTNode * Token * ASTNode
    |   OrTest of uint * uint * ASTNode * Token * ASTNode
    |   Lambda of uint * uint * Token * ASTNode * Token * ASTNode
    |   Test of uint * uint * ASTNode * Token * ASTNode * Token * ASTNode
    |   NamedExpr of uint * uint * ASTNode * Token * ASTNode
    |   YieldFrom of uint * uint * Token * Token * ASTNode
    |   YieldExpr of uint * uint * Token * ASTNode
    |   TestListStarExpr of uint * uint * ASTNode array * Token array
    |   CompIf of uint * uint * Token * ASTNode * ASTNode
    |   CompAsyncFor of uint * uint * Token * ASTNode
    |   CompFor of uint * uint * Token * ASTNode * Token * ASTNode * ASTNode
    |   ExprList of uint * uint * ASTNode array * Token array
    |   TestList of uint * uint * ASTNode array * Token array
    |   ArgumentList of uint * uint * ASTNode array * Token array
    |   Argument of uint * uint * ASTNode * Token * ASTNode
    |   SubscriptList of uint * uint * ASTNode array * Token array
    |   Subscript of uint * uint * ASTNode * Token * ASTNode * Token * ASTNode
    |   DotName of uint * uint * Token * Token
    |   CallExpression of uint * uint * Token * ASTNode * Token
    |   IndexExpression of uint * uint * Token * ASTNode * Token
    |   PowerKey of uint * uint * Token * ASTNode
    |   DictionaryContainer of uint * uint * ASTNode array * Token array
    |   DictiionaryEntry of uint * uint * ASTNode * Token * ASTNode
    |   SetContainer of uint * uint * ASTNode array * Token array
    |   VarArgsList of uint * uint * Token * ASTNode * Token * ASTNode * ASTNode array * Token array
    |   VFPDefAssign of uint * uint * ASTNode * Token * ASTNode
    |   EvalInput of uint * uint * ASTNode * Token array * Token
    |   FuncTypeInput of uint * uint * ASTNode * Token array * Token
    |   FuncType of uint * uint * Token * ASTNode * Token * Token * ASTNode
    |   TypeList of uint * uint * Token * ASTNode * Token * ASTNode * ASTNode array * Token array
    |   SimpleStmt of uint * uint * ASTNode array * Token array * Token
    |   DelStmt of uint * uint * Token * ASTNode
    |   PassStmt of uint * uint * Token
    |   BreakStmt of uint * uint * Token
    |   ContinueStmt of uint * uint * Token
    |   ReturnStmt of uint * uint * Token * ASTNode
    |   RaiseStmt of uint * uint * Token * ASTNode * Token * ASTNode
    |   PlusAssign of uint * uint * ASTNode * Token * ASTNode
    |   MinusAssign of uint * uint * ASTNode * Token * ASTNode
    |   MulAssign of uint * uint * ASTNode * Token * ASTNode
    |   MatriceAssign of uint * uint * ASTNode * Token * ASTNode
    |   DivAssign of uint * uint * ASTNode * Token * ASTNode
    |   ModuloAssign of uint * uint * ASTNode * Token * ASTNode
    |   BitAndAssign of uint * uint * ASTNode * Token * ASTNode
    |   BitOrAssign of uint * uint * ASTNode * Token * ASTNode
    |   BitXorAssign of uint * uint * ASTNode * Token * ASTNode
    |   ShiftLeftAssign of uint * uint * ASTNode * Token * ASTNode
    |   ShiftRightAssign of uint * uint * ASTNode * Token * ASTNode
    |   PowerAssign of uint * uint * ASTNode * Token * ASTNode
    |   FloorDivAssign of uint * uint * ASTNode * Token * ASTNode
    |   Assign of uint * uint * ASTNode * Token * ASTNode * Token
    |   Annotation of uint * uint * ASTNode * Token * ASTNode
    