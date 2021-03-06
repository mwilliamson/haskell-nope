module Nope.Parsing where

import qualified Language.Python.Common.AST as Python
import qualified Language.Python.Version3.Parser as PythonParser
import Language.Python.Common.ParseError (ParseError(UnexpectedToken, UnexpectedChar, StrError))
import Language.Python.Common.Token (Token, token_span, tokenString)
import Language.Python.Common.SrcLocation (SrcSpan, startRow, startCol, getSpan)
import Data.Maybe (fromMaybe)

import Nope.Results
import Nope.Sources
import qualified Nope.Nodes as Nodes


type ParsedModule = Nodes.Module () String
type ParsedStatement = Nodes.Statement () String
type ParsedExpression = Nodes.Expression String
type ParsedFunction = Nodes.Function () String


parsedModule :: [ParsedStatement] -> ParsedModule
parsedModule statements = Nodes.Module () statements


parseModule :: Source -> Result ParsedModule
parseModule (Source sourceDescription input) = do
    (moduleSpan, _) <- toResult $ PythonParser.parseModule input (show sourceDescription)
    transformModule moduleSpan where

        toResult :: Either ParseError a -> Result a

        toResult (Right x) = return x

        toResult (Left (UnexpectedToken token)) =
            let location = extractLocation token
                message = "Unexpected token '" ++ (tokenString token) ++ "'"
            in Left (SyntaxError location message)

        toResult (Left (UnexpectedChar char location)) =
            let location' = spanToLocation (getSpan location)
                message = "Unexpected character '" ++ [char] ++ "'"
            in Left (SyntaxError location' message)

        toResult (Left (StrError message)) =
            Left (SyntaxError (SourceFile sourceDescription) message)

        extractLocation :: Token -> SourceLocation
        extractLocation token = spanToLocation (token_span token)


        spanToLocation :: SrcSpan -> SourceLocation
        spanToLocation srcSpan =
            let rowIndex = (startRow srcSpan) - 1
                colIndex = (startCol srcSpan) - 1
            in SourcePoint sourceDescription rowIndex colIndex


        transformModule :: Python.ModuleSpan -> Result ParsedModule
        transformModule (Python.Module statements) = do
            nopeStatements <- mapM transformStatement statements
            return $ parsedModule nopeStatements


        transformStatement :: Python.StatementSpan -> Result ParsedStatement
        
        transformStatement (Python.StmtExpr expression _) =
            fmap Nodes.ExpressionStatement (transformExpression expression)
        
        transformStatement (Python.Assign targets value _) = do
            nopeTargets <- mapM transformExpression targets
            nopeValue <- transformExpression value
            return $ Nodes.Assign nopeTargets nopeValue
        
        transformStatement function@Python.Fun{} = do
            let name = Python.ident_string (Python.fun_name function)
                arguments = map (Python.ident_string . Python.param_name) (Python.fun_args function)
            body <- mapM transformStatement (Python.fun_body function)
            return $ Nodes.FunctionStatement $ Nodes.Function {
                Nodes.functionTarget = name,
                Nodes.functionArguments = arguments,
                Nodes.functionScope = (),
                Nodes.functionBody = body
            }
        
        transformStatement (Python.Return value _) = do
            value' <- mapM transformExpression value
            return $ Nodes.Return (fromMaybe Nodes.none value')
        
        transformStatement statement =
            unsupportedNode (Python.stmt_annot statement) (describeStatement statement)


        unsupportedNode :: SrcSpan -> String -> Result a
        unsupportedNode srcSpan nodeDescription =
            Left $ SyntaxError (spanToLocation srcSpan) ("Unsupported node: " ++ nodeDescription)


        transformExpression :: Python.ExprSpan -> Result ParsedExpression
        
        transformExpression (Python.None _) = return Nodes.none
        
        transformExpression (Python.Int value _ _) =
            return $ Nodes.integer value

        transformExpression (Python.Call func args _) = do
            nopeFunc <- transformExpression func
            nopeArgs <- mapM transformArgument args
            return $ Nodes.Call nopeFunc nopeArgs
            
        transformExpression (Python.Var (Python.Ident name _) _) =
            return $ Nodes.VariableReference name
        
        -- TODO: error
        transformExpression _ = undefined


        transformArgument :: Python.ArgumentSpan -> Result ParsedExpression
        transformArgument (Python.ArgExpr expression _) = transformExpression expression
        -- TODO: error
        transformArgument _ = undefined


describeStatement :: Python.StatementSpan -> String
describeStatement (Python.Import _ _) = "import statement"
describeStatement (Python.FromImport _ _ _) = "import from statement"
describeStatement (Python.While _ _ _ _) = "while loop"
describeStatement (Python.For _ _ _ _ _) = "for loop"
describeStatement (Python.Fun _ _ _ _ _) = "function definition"
describeStatement (Python.Class _ _ _ _) = "class definition"
describeStatement (Python.Conditional _ _ _) = "if statement"
describeStatement (Python.Assign _ _ _) = "assignment"
describeStatement (Python.AugmentedAssign _ _ _ _) = "augmented assignment"
describeStatement (Python.Decorated _ _ _) = "decorated definition"
describeStatement (Python.Return _ _) = "return statement"
describeStatement (Python.Try _ _ _ _ _) = "try statement"
describeStatement (Python.Raise _ _) = "raise statement"
describeStatement (Python.With _ _ _) = "with statement"
describeStatement (Python.Pass _) = "pass statement"
describeStatement (Python.Break _) = "break statement"
describeStatement (Python.Continue _) = "continue statement"
describeStatement (Python.Delete _ _) = "delete statement"
describeStatement (Python.StmtExpr _ _) = "expression statement"
describeStatement (Python.Global _ _) = "global statement"
describeStatement (Python.NonLocal _ _) = "nonlocal statement"
describeStatement (Python.Assert _ _) = "assert statement"
describeStatement (Python.Print _ _ _ _) = "print statement"
describeStatement (Python.Exec _ _ _) = "exec statement"
