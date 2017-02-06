{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.DeepSeq
--import Control.DeepSeq.TH
import Data.Serialize
import GHC.Generics

import Language.MiniZinc


type Replacement = (Constraint, [Argument])

-- newtype ConstraintNumber = ConstraintNumber Int
--   deriving (Show, Generic, Eq, NFData)

-- type Context = [ConstraintNumber]

data ArgBaseType = ArgInt
                 | ArgSetInt
  deriving (Eq, Show, Read, Generic, Ord)

data ArgType = ArgType ArgBaseType Integer
  deriving (Eq, Show, Read, Generic, Ord)

data Constraint = Constraint {
      name :: String
    , argtypes :: [ArgType]
    }
  deriving (Eq, Show, Generic, Read)


data Argument =
    -- e.g. "int : n"
    OrdinaryParameter (Expression')
    -- e.g. "array [int] of var int : x", which becomes a par array
    -- after the solutions are found
  | ErstwhileVariable VarId
    -- e.g. "x[p,_]" -- the "x" is a var array, but the arguments are
    -- par.
  | ArgumentArrayAccess Argument [Expression']
    -- blank argument to be filled in
  | Blank            
  deriving (Eq, Show, Generic, Read)

type ChannelMap = [(VarId, VarId, [Int])]




instance Serialize Argument
instance Serialize ArgBaseType
instance Serialize ArgType
instance Serialize Constraint
-- instance Serialize ConstraintNumber
