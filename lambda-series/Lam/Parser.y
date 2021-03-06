{
{-# LANGUAGE FlexibleContexts #-}

module Lam.Parser where

import Numeric
import System.Exit
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)

import Lam.Lexer
import Lam.Syntax
import Lam.Options

}

%name program Program
%name expr Expr
%tokentype { Token }
%error { parseError }
%monad { ReaderT String (Either String) }

%token
    nl      { TokenNL _ }
    let     { TokenLet _ }
    case    { TokenCase _ }
    natcase { TokenNatCase _ }
    of      { TokenOf _ }
    '|'     { TokenSep _ }
    fix     { TokenFix _ }
    fst     { TokenFst _ }
    snd     { TokenSnd _ }
    inl     { TokenInl _ }
    inr     { TokenInr _ }
    in      { TokenIn  _  }
    zero    { TokenZero _ }
    succ    { TokenSucc _ }
    VAR     { TokenSym _ _ }
    LANG    { TokenLang _ _ }
    CONSTR  { TokenConstr _ _ }
    '\\'    { TokenLambda _ }
    '->'    { TokenArrow _ }
    '='     { TokenEq _ }
    '('     { TokenLParen _ }
    ')'     { TokenRParen _ }
    ':'     { TokenSig _ }
    '?'     { TokenHole _ }
    '*'     { TokenProd _ }
    '+'     { TokenSum _ }
    '<'     { TokenLPair _ }
    '>'     { TokenRPair _ }
    ', '    { TokenMPair _ }

%right in
%right '->'
%left ':'
%left '+' '-'
%left '*'
%%

Program :: { (Expr PCF, [Option]) }
  : LangOpts Defs  { ($2 $1, $1) }

LangOpts :: { [Option] }
  : LANG nl LangOpts    {% (readOption $1) >>= (\opt -> addOption opt $3) }
  | {- empty -}         { [] }

Defs :: { [Option] -> Expr PCF }
  : Def NL Defs           { \opts -> ($1 opts) ($3 opts) }
  | Expr                  { \opts -> $1 opts }

NL :: { () }
  : nl NL                     { }
  | nl                        { }

Def :: { [Option] -> Expr PCF -> Expr PCF }
  : VAR '=' Expr { \opts -> \program -> App (Abs (symString $1) program) ($3 opts) }
  | zero '=' Expr { \opts ->
        if isPCF opts
          then error "Cannot use 'zero' as a variable name"
          else \program -> App (Abs "zero" program) ($3 opts) }
  | succ '=' Expr { \opts ->
        if isPCF opts
          then error "Cannot use 'succ' as a variable name"
          else  \program -> App (Abs "succ" program) ($3 opts) }

Expr :: { [Option] -> Expr PCF }
  : let VAR '=' Expr in Expr
    { \opts -> App (Abs (symString $2) ($6 opts)) ($4 opts) }

  | '\\' VAR '->' Expr
    { \opts -> Abs (symString $2) ($4 opts) }

  | Expr ':' Type  { \opts -> Sig ($1 opts) $3 }

  | Juxt
    { $1 }

  | fix '(' Expr ')'
     { \opts ->
      if isPCF opts
        then Ext (Fix ($3 opts))
        else error "`fix` doesn't exists in the lambda calculus" }

  | natcase Expr of zero '->' Expr '|' succ VAR '->' Expr
     { \opts ->
          if isPCF opts
            then Ext (NatCase ($2 opts) ($6 opts) (symString $9, ($11 opts)))
            else error "`natcase` doesn't exist in the lambda calculus" }

  | '<' Expr ', ' Expr '>'
     { \opts ->
          if isPCF opts
            then Ext (Pair ($2 opts) ($4 opts))
            else error "pairs don't exists in the lambda calculus"}

  | fst '(' Expr ')'
     { \opts ->
      if isPCF opts
        then Ext (Fst ($3 opts))
        else error "`fst` doesn't exists in the lambda calculus" }

  | snd '(' Expr ')'
     { \opts ->
      if isPCF opts
        then Ext (Snd ($3 opts))
        else error "`snd` doesn't exists in the lambda calculus" }

  | inl '(' Expr ')'
     { \opts ->
      if isPCF opts
        then Ext (Inl ($3 opts))
        else error "`inl` doesn't exists in the lambda calculus" }

  | inr '(' Expr ')'
     { \opts ->
      if isPCF opts
        then Ext (Inr ($3 opts))
        else error "`inr` doesn't exists in the lambda calculus" }

 | case Expr of inl VAR '->' Expr '|' inr VAR '->' Expr
     { \opts ->
          if isPCF opts
            then Ext (Case ($2 opts) (symString $5, $7 opts) (symString $10, ($12 opts)))
            else error "`case` doesn't exist in the lambda calculus" }


Type :: { Type }
Type
  : CONSTR           { if constrString $1 == "Nat" then NatTy else error $ "Unknown type constructor " ++ constrString $1 }
  | Type '->' Type   { FunTy $1 $3 }
  | Type '*' Type    { ProdTy $1 $3}
  | Type '+' Type    { SumTy $1 $3 }
  | '(' Type ')'     { $2 }

Juxt :: { [Option] -> Expr PCF }
  : Juxt Atom                 { \opts -> App ($1 opts) ($2 opts) }
  | Atom                      { $1 }

Atom :: { [Option] -> Expr PCF }
  : '(' Expr ')'              { $2 }
  | VAR                       { \opts -> Var $ symString $1 }
  | zero
    { \opts ->
        if isPCF opts
          then Ext Zero
          else Var "zero" }
  | succ
    { \opts ->
        if isPCF opts
          then Ext Succ
          else Var "succ" }

  -- For later
  -- | '?' { Hole }

{

readOption :: Token -> ReaderT String (Either String) Option
readOption (TokenLang _ x) | x == "lang.pcf"   = return PCF
readOption (TokenLang _ x) | x == "lang.typed" = return Typed
readOption (TokenLang _ x) | x == "lang.cbv"   = return CBV
readOption (TokenLang _ x) | x == "lang.cbn"   = return CBN
readOption (TokenLang _ x) = lift . Left $ "Unknown language option: " <> x
readOption _ = lift . Left $ "Wrong token for language"

parseError :: [Token] -> ReaderT String (Either String) a
parseError [] = lift . Left $ "Premature end of file"
parseError t  =  do
    file <- ask
    lift . Left $ file <> ":" <> show l <> ":" <> show c
                        <> ": parse error"
  where (l, c) = getPos (head t)

parseProgram :: FilePath -> String -> Either String (Expr PCF, [Option])
parseProgram file input = runReaderT (program $ scanTokens input) file

}
