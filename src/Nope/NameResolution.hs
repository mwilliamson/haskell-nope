module Nope.NameResolution where

import Control.Monad.State (State, get, put, evalState)
import qualified Data.Map.Strict as Map

import Nope.Parsing (ParsedModule)
import Nope.NameDeclaration
import qualified Nope.Nodes as Nodes


data VariableDeclaration =
    VariableDeclaration String Int |
    Builtin String
    deriving (Show, Eq)

type Environment = Map.Map String VariableDeclaration

type ResolvedModule = Nodes.Module VariableDeclaration
type ResolvedStatement = Nodes.Statement VariableDeclaration
type ResolvedExpression = Nodes.Expression VariableDeclaration

resolveReferences :: ParsedModule -> ResolvedModule
resolveReferences moduleNode =
    evalState (resolveModuleReferences moduleNode) 1

resolveModuleReferences :: Nodes.Module String -> Counter ResolvedModule
resolveModuleReferences moduleNode = do
    scope <- scopeForModule moduleNode
    return $ mapModuleExpressions (resolveExpressionReference scope) moduleNode


resolveExpressionReference :: Environment -> Nodes.Expression String -> ResolvedExpression
resolveExpressionReference env (Nodes.VariableReference name) =
    case Map.lookup name env of
        Just variableDeclaration -> Nodes.VariableReference variableDeclaration
        Nothing -> Nodes.VariableReference (Builtin name)
resolveExpressionReference scope (Nodes.Call func args) =
    let func' = resolveExpressionReference scope func
        args' = map (resolveExpressionReference scope) args
    in Nodes.Call func' args'
resolveExpressionReference _ (Nodes.Literal literal) = Nodes.Literal literal


type ExpressionMap ref ref' = Nodes.Expression ref -> Nodes.Expression ref'


mapModuleExpressions :: ExpressionMap ref ref' -> Nodes.Module ref -> Nodes.Module ref'
mapModuleExpressions f moduleNode = 
    let statements = Nodes.statements moduleNode
        statements' = map (mapStatementExpressions f) statements
    in moduleNode {Nodes.statements = statements'}

mapStatementExpressions :: ExpressionMap ref ref'-> Nodes.Statement ref -> Nodes.Statement ref'
mapStatementExpressions f (Nodes.ExpressionStatement value) =
    Nodes.ExpressionStatement (f value)
mapStatementExpressions f (Nodes.Assign targets value) =
    Nodes.Assign (map f targets) (f value)
type Counter = State Int

scopeForModule :: ParsedModule -> Counter (Map.Map String VariableDeclaration)
scopeForModule moduleNode = do
    let names = namesDeclaredInModule moduleNode
    declarations <- namesToDeclarations names
    return $ Map.fromList declarations

namesToDeclarations :: [String] -> Counter [(String, VariableDeclaration)]
namesToDeclarations names = mapM nameToDeclaration names

nameToDeclaration :: String -> Counter (String, VariableDeclaration)
nameToDeclaration name = do
    count <- get
    put (count + 1)
    return $ (name, VariableDeclaration name count)
