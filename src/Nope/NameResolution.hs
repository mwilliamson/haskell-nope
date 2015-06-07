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
    scope <- scopeForModule moduleNode
    statements <- mapM (resolveReferencesInStatement scope) (Nodes.statements moduleNode)
    return $ Nodes.Module (Map.elems scope) statements


resolveReferencesInStatement :: Environment -> ParsedStatement -> Counter ResolvedStatement

resolveReferencesInStatement environment (Nodes.ExpressionStatement value) =
    return $ Nodes.ExpressionStatement (resolveReferencesInExpression environment value)

resolveReferencesInStatement environment (Nodes.Assign targets value) = do
    let targets' = map (resolveReferencesInExpression environment) targets
        value' = resolveReferencesInExpression environment value
    return $ Nodes.Assign targets' value'

resolveReferencesInStatement outerEnvironment function@Nodes.Function{} = do
    -- An absent name is a programming error: they should be added by name declaration
    let (Just declaration) = Map.lookup (Nodes.functionTarget function) outerEnvironment
    bodyEnvironment <- scopeForFunction (Nodes.functionBody function)
    body <- mapM (resolveReferencesInStatement bodyEnvironment) (Nodes.functionBody function)
    return Nodes.Function {
        Nodes.functionTarget = declaration,
        Nodes.functionScope = Map.elems bodyEnvironment,
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


scopeForModule :: ParsedModule -> Counter Environment
scopeForModule moduleNode = do
    let names = namesDeclaredInModule moduleNode
    declarations <- namesToDeclarations names
    return $ Map.fromList declarations

-- TODO: fix up the types
scopeForFunction :: [ParsedStatement] -> Counter Environment
scopeForFunction statements = do
    let names = concat $ map namesDeclaredInStatement statements
    declarations <- namesToDeclarations names
    return $ Map.fromList declarations


namesToDeclarations :: [String] -> Counter [(String, VariableDeclaration)]
namesToDeclarations names = mapM nameToDeclaration names

nameToDeclaration :: String -> Counter (String, VariableDeclaration)
nameToDeclaration name = do
    count <- get
    put (count + 1)
    return $ (name, VariableDeclaration name count)
