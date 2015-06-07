module Nope.NameResolution (ResolvedModule, ResolvedStatement, ResolvedExpression, resolveReferences, VariableDeclaration(..)) where

import Control.Monad.State (State, get, put, evalState)
import qualified Data.Map.Strict as Map

import Nope.Parsing (ParsedModule, ParsedStatement, ParsedExpression)
import Nope.NameDeclaration
import qualified Nope.Nodes as Nodes


data VariableDeclaration =
    VariableDeclaration String Int |
    Builtin String
    deriving (Show, Eq)

type Environment = Map.Map String VariableDeclaration

type ResolvedModule = Nodes.Module [VariableDeclaration] VariableDeclaration
type ResolvedStatement = Nodes.Statement [VariableDeclaration] VariableDeclaration
type ResolvedExpression = Nodes.Expression VariableDeclaration

type Counter = State Int

resolveReferences :: ParsedModule -> ResolvedModule
resolveReferences moduleNode =
    evalState (resolveReferencesInModule moduleNode) 1

resolveReferencesInModule :: ParsedModule -> Counter ResolvedModule
resolveReferencesInModule moduleNode = do
    environment <- generateDeclarations moduleNode
    statements <- mapM (resolveReferencesInStatement environment) (Nodes.statements moduleNode)
    return $ Nodes.Module (Map.elems environment) statements


resolveReferencesInStatement :: Environment -> ParsedStatement -> Counter ResolvedStatement

resolveReferencesInStatement environment (Nodes.ExpressionStatement value) =
    return $ Nodes.ExpressionStatement (resolveReferencesInExpression environment value)

resolveReferencesInStatement environment (Nodes.Assign targets value) = do
    let targets' = map (resolveReferencesInExpression environment) targets
        value' = resolveReferencesInExpression environment value
    return $ Nodes.Assign targets' value'

resolveReferencesInStatement outerEnvironment (Nodes.FunctionStatement function) = do
    -- An absent name is a programming error: they should be added by name declaration
    let (Just declaration) = Map.lookup (Nodes.functionTarget function) outerEnvironment
    bodyDeclarations <- generateDeclarations function
    let bodyEnvironment = Map.union bodyDeclarations outerEnvironment
    body <- mapM (resolveReferencesInStatement bodyEnvironment) (Nodes.functionBody function)
    return $ Nodes.FunctionStatement $ Nodes.Function {
        Nodes.functionTarget = declaration,
        Nodes.functionScope = Map.elems bodyDeclarations,
        Nodes.functionBody = body
    }

resolveReferencesInStatement environment (Nodes.Return value) =
    return $ Nodes.Return (resolveReferencesInExpression environment value)


resolveReferencesInExpression :: Environment -> ParsedExpression -> ResolvedExpression
resolveReferencesInExpression env (Nodes.VariableReference name) =
    case Map.lookup name env of
        Just variableDeclaration -> Nodes.VariableReference variableDeclaration
        Nothing -> Nodes.VariableReference (Builtin name)
resolveReferencesInExpression scope (Nodes.Call func args) =
    let func' = resolveReferencesInExpression scope func
        args' = map (resolveReferencesInExpression scope) args
    in Nodes.Call func' args'
resolveReferencesInExpression _ (Nodes.Literal literal) = Nodes.Literal literal

generateDeclarations :: CreatesScope a => a -> Counter Environment
generateDeclarations a = do
    declarations <- namesToDeclarations (declaredNames a)
    return (Map.fromList declarations)

namesToDeclarations :: [String] -> Counter [(String, VariableDeclaration)]
namesToDeclarations names = mapM nameToDeclaration names

nameToDeclaration :: String -> Counter (String, VariableDeclaration)
nameToDeclaration name = do
    count <- get
    put (count + 1)
    return $ (name, VariableDeclaration name count)
