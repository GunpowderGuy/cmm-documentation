{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson (FromJSON(..), Value)
import GHC.Generics (Generic)

import GHC.Cmm.Dataflow.Label

-- Core Cmm surface (decls, graph, section, synonyms like CmmGraph/CmmStatics)
import GHC.Cmm

-- Not re-exported by GHC.Cmm in GHC 9.10.x:
import GHC.Cmm.CLabel (CLabel)         -- label type used by Section and CmmProc
import GHC.Cmm.Reg    (GlobalReg)      -- register type used by CmmProc

-- Allow Aeson Generic-based instance at the top level
deriving instance Generic (GenCmmDecl CmmStatics CmmTopInfo CmmGraph)
instance FromJSON (GenCmmDecl CmmStatics CmmTopInfo CmmGraph)
-- ^ Do NOT give a body here; you want the automatic (Generic) instance.

-- ---- Dummy leaf instances to satisfy the Generic traversal ----

-- d: CmmStatics is a type synonym = GenCmmStatics 'False
-- Provide only the polymorphic instance to avoid overlap with the synonym.
instance FromJSON (GenCmmStatics rawOnly) where
  parseJSON _ = fail "dummy FromJSON for GenCmmStatics"

deriving instance Generic CmmTopInfo
-- h
instance FromJSON CmmTopInfo --where
--parseJSON _ = fail "dummy FromJSON for CmmTopInfo"

-- g
instance FromJSON (GenCmmGraph CmmNode) where
  parseJSON _ = fail "dummy FromJSON for GenCmmGraph"

-- CmmNode is higher-kinded (Extensibility -> Extensibility -> *)
instance FromJSON (CmmNode e x) where
  parseJSON _ = fail "dummy FromJSON for CmmNode"

-- Fields in CmmProc/CmmData:
instance FromJSON CLabel where
  parseJSON _ = fail "dummy FromJSON for CLabel"

instance FromJSON GlobalReg where
  parseJSON _ = fail "dummy FromJSON for GlobalReg"

instance FromJSON Section where
  parseJSON _ = fail "dummy FromJSON for Section"

-- ------------------------------------------------------------
-- These instances are needed for CmmTopInfos
instance FromJSON (GHC.Cmm.Dataflow.Label.LabelMap CmmInfoTable) where
  parseJSON _= fail "dummy"
  
instance FromJSON CmmStackInfo where
  parseJSON _= fail "dummy"

main :: IO ()
main = putStrLn "Hello, World!"

