module Nope.Parsing where

import qualified Language.Python.Common.AST as Python
import qualified Language.Python.Version3.Parser as PythonParser
import Language.Python.Common.ParseError (ParseError(UnexpectedToken, UnexpectedChar, StrError))
import Language.Python.Common.Token (Token, token_span, tokenString)
import Language.Python.Common.SrcLocation (SrcSpan, startRow, startCol, getSpan)

import Nope.Results
import Nope.Sources
import qualified Nope.Nodes as Nodes

parseModule :: Source -> Result Nodes.Module
parseModule (Source description input) = do
    (moduleSpan, _) <- toResult description $ PythonParser.parseModule input (show description)
    transformModule moduleSpan


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


transformModule :: Python.ModuleSpan -> Result Nodes.Module
transformModule (Python.Module statements) = do
    nopeStatements <- mapM transformStatement statements
    return $ Nodes.Module nopeStatements


transformStatement :: Python.StatementSpan -> Result Nodes.Statement
transformStatement (Python.StmtExpr expression _) =
    fmap Nodes.ExpressionStatement (transformExpression expression)

transformStatement _ = undefined
--transformStatement statement =
    --Left $ SyntaxError ()


transformExpression :: Python.ExprSpan -> Result Nodes.Expression

transformExpression (Python.Int value _ _) =
    return $ Nodes.Literal value

transformExpression (Python.Call func args _) = do
    nopeFunc <- transformExpression func
    nopeArgs <- mapM transformArgument args
    return $ Nodes.Call nopeFunc nopeArgs
    
transformExpression (Python.Var (Python.Ident name _) _) =
    return $ Nodes.Builtin name
-- TODO: error
transformExpression _ = undefined


transformArgument :: Python.ArgumentSpan -> Result Nodes.Expression
transformArgument (Python.ArgExpr expression _) = transformExpression expression
-- TODO: error
transformArgument _ = undefined
