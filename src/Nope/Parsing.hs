module Nope.Parsing where

import qualified Language.Python.Common.AST as Python
import Language.Python.Version3.Parser
import Language.Python.Common.ParseError (ParseError(UnexpectedToken, UnexpectedChar, StrError))
import Language.Python.Common.Token (Token, token_span, tokenString)
import Language.Python.Common.SrcLocation (SrcSpan, startRow, startCol, getSpan)

import Nope.Results
import Nope.Sources
import qualified Nope.Nodes as Nodes

parse :: Source -> Result Nodes.Module
parse (Source description input) = do
    (moduleSpan, _) <- toResult description $ parseModule input (show description)
    return $ transformModule moduleSpan


toResult :: SourceDescription -> Either ParseError a -> Result a

toResult _ (Right x) = return x

toResult sourceDescription (Left (UnexpectedToken token)) =
    let location = extractLocation sourceDescription token
        message = "Unexpected token '" ++ (tokenString token) ++ "'"
    in Left (SyntaxError location message)

toResult sourceDescription (Left (UnexpectedChar char location)) =
    let location' = spanToLocation sourceDescription (getSpan location)
        message = "Unexpected character '" ++ [char] ++ "'"
    in Left (SyntaxError location' message)

toResult sourceDescription (Left (StrError message)) =
    Left (SyntaxError (SourceFile sourceDescription) message)

extractLocation :: SourceDescription -> Token -> SourceLocation
extractLocation sourceDescription token =
    spanToLocation sourceDescription (token_span token)


spanToLocation :: SourceDescription -> SrcSpan -> SourceLocation
spanToLocation sourceDescription srcSpan =
    let rowIndex = (startRow srcSpan) - 1
        colIndex = (startCol srcSpan) - 1
    in SourcePoint sourceDescription rowIndex colIndex


transformModule :: Python.ModuleSpan -> Nodes.Module
transformModule (Python.Module statements) =
    Nodes.Module $ map transformStatement statements


transformStatement :: Python.StatementSpan -> Nodes.Statement
transformStatement (Python.StmtExpr expression _) =
    Nodes.ExpressionStatement $ transformExpression expression
-- TODO: error
transformStatement _ = undefined


transformExpression :: Python.ExprSpan -> Nodes.Expression
transformExpression (Python.Int value _ _) = Nodes.Literal value
transformExpression (Python.Call func args _) =
    Nodes.Call (transformExpression func) (map transformArgument args)
transformExpression (Python.Var (Python.Ident name _) _) = Nodes.Builtin name
-- TODO: error
transformExpression _ = undefined


transformArgument :: Python.ArgumentSpan -> Nodes.Expression
transformArgument (Python.ArgExpr expression _) = transformExpression expression
-- TODO: error
transformArgument _ = undefined
