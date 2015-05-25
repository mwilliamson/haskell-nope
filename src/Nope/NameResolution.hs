module Nope.NameResolution where

import Nope.Parsing (ParsedModule)
import Nope.NameDeclaration
import qualified Nope.Nodes as Nodes


resolveReferences :: ParsedModule -> Nodes.Module Scope VariableDeclaration
resolveReferences moduleNode =
    resolveModuleReferences $ declareNames moduleNode

resolveModuleReferences :: Nodes.Module Scope String -> Nodes.Module Scope VariableDeclaration
resolveModuleReferences moduleNode =
    let scope = Nodes.scope moduleNode
    in mapModuleExpressions (resolveExpressionReference scope) moduleNode


resolveExpressionReference :: Scope -> Nodes.Expression String -> Nodes.Expression VariableDeclaration
resolveExpressionReference scope (Nodes.VariableReference name) =
    -- TODO: errors
    let (Just variableDeclaration) = declaration name scope
    in Nodes.VariableReference variableDeclaration
resolveExpressionReference scope (Nodes.Call func args) =
    let func' = resolveExpressionReference scope func
        args' = map (resolveExpressionReference scope) args
    in Nodes.Call func' args'
resolveExpressionReference _ (Nodes.Literal literal) = Nodes.Literal literal


type ExpressionMap ref ref' = Nodes.Expression ref -> Nodes.Expression ref'


mapModuleExpressions :: ExpressionMap ref ref' -> Nodes.Module scope ref -> Nodes.Module scope ref'
mapModuleExpressions f moduleNode = 
    let statements = Nodes.statements moduleNode
        statements' = map (mapStatementExpressions f) statements
    in moduleNode {Nodes.statements = statements'}

mapStatementExpressions :: ExpressionMap ref ref'-> Nodes.Statement ref -> Nodes.Statement ref'
mapStatementExpressions f (Nodes.ExpressionStatement value) =
    Nodes.ExpressionStatement (f value)
mapStatementExpressions f (Nodes.Assign targets value) =
    Nodes.Assign (map f targets) (f value)
