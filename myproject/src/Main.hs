{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

--for manual generic implementation
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}


module Main where

import Data.Aeson (FromJSON(..), Value, genericParseJSON, defaultOptions)
-- last one for json instance based on manual Generic

import GHC.Generics (Generic)


import GHC.Generics
  ( Generic(..), (:*:)(..), (:+:)(..)
  , M1(..), K1(..), C1, D1, S1, Rec0
  , Meta(..)                 -- 'MetaData, 'MetaCons, 'MetaSel
  , FixityI(..)              -- PrefixI, InfixI ...
  , SourceUnpackedness(..)   -- NoSourceUnpackedness
  , SourceStrictness(..)     -- NoSourceStrictness
  , DecidedStrictness(..)    -- DecidedLazy
  )

import GHC.Cmm.Dataflow.Label

-- Core Cmm surface (decls, graph, section, synonyms like CmmGraph/CmmStatics)
import GHC.Cmm

-- Not re-exported by GHC.Cmm in GHC 9.10.x:
import GHC.Cmm.CLabel (CLabel)         -- label type used by Section and CmmProc
import GHC.Cmm.Reg    (GlobalReg)      -- register type used by CmmProc


import GHC.Cmm.Dataflow.Graph -- curent import
import GHC.Cmm.Dataflow.Block

--for manual generic implementation
import GHC.Types.CostCentre (CostCentreStack)


-- NECESARIO para usar mapEmpty / mapInsertList
--import GHC.Cmm.Dataflow.Collections (IsMap(..))
import Data.Aeson.Types (Parser)
--import qualified Data.IntMap as IM
import Data.Word (Word64)



-- Allow Aeson Generic-based instance at the top level
deriving instance Generic (GenCmmDecl CmmStatics CmmTopInfo CmmGraph)
instance FromJSON (GenCmmDecl CmmStatics CmmTopInfo CmmGraph)
--instance FromJSON (GenCmmDecl CmmStatics CmmTopInfo CmmGraph) where
--  parseJSON _ = fail "dummy FromJSON for CmmNode"
-- ^ Do NOT give a body here; you want the automatic (Generic) instance.

-- ---- Dummy leaf instances to satisfy the Generic traversal ----

-- d: CmmStatics is a type synonym = GenCmmStatics 'False
-- Provide only the polymorphic instance to avoid overlap with the synonym.
--instance FromJSON (GenCmmStatics rawOnly) where
--  parseJSON _ = fail "dummy FromJSON for GenCmmStatics"

deriving instance Generic CmmTopInfo
-- h
instance FromJSON CmmTopInfo --where
--  parseJSON _ = fail "dummy FromJSON for CmmTopInfo"

deriving instance Generic (GenCmmGraph CmmNode)
-- g
instance FromJSON (GenCmmGraph CmmNode) --where
--  parseJSON _ = fail "dummy FromJSON for GenCmmGraph"

-- CmmNode is higher-kinded (Extensibility -> Extensibility -> *) 
-- Seems iffy whether i can derive generic
--deriving instance Generic (CmmNode e x) 
instance FromJSON (CmmNode e x) where
  parseJSON _ = fail "dummy FromJSON for CmmNode"

-- Fields in CmmProc/CmmData:
--deriving instance Generic CLabel 
--Can't make a derived instance of ‘Generic CLabel’:
--        The data constructors of ‘CLabel’ are not all in scope
--          so you cannot derive an instance for it
--    • In the stand-alone deriving instance for ‘Generic CLabel’
instance FromJSON CLabel where
  parseJSON _ = fail "dummy FromJSON for CLabel"

deriving instance Generic GlobalReg 
instance FromJSON GlobalReg --where
--parseJSON _ = fail "dummy FromJSON for GlobalReg"

deriving instance Generic Section
instance FromJSON Section --where
--parseJSON _ = fail "dummy FromJSON for Section"


-- These instances are needed for CmmTopInfos
--instance FromJSON (GHC.Cmm.Dataflow.Label.LabelMap CmmInfoTable) where
--  parseJSON _= fail "dummy"


-- JSON como lista de pares [(Word64, a)] -> LabelMap a
instance FromJSON a => FromJSON (LabelMap a) where
  parseJSON v = do
    ps <- (parseJSON v :: Parser [(Word64, Value)])
    pairs <- mapM
      (\(w, val) -> do
          x <- parseJSON val   -- x :: a (se infiere por el 'a' del instance)
          pure (mkHooplLabel w, x))
      ps
    pure (mapFromList pairs)


deriving instance Generic CmmStackInfo
instance FromJSON CmmStackInfo 
--instance FromJSON CmmStackInfo where
--  parseJSON _= fail "dummy"

-- needed because of instance FromJSON CmmTopInfo

instance FromJSON (GHC.Cmm.Dataflow.Graph.Graph'
                            GHC.Cmm.Dataflow.Block.Block
                            CmmNode
                            GHC.Cmm.Dataflow.Block.C
                            GHC.Cmm.Dataflow.Block.C) where
  parseJSON _= fail "dummy"
--import GHC.Cmm.Dataflow.Graph are imported because of 
--import GHC.Cmm.Dataflow.Block  the above
---
---
---Needed for GenCmmGraph CmmNode

--instance FromJSON Label where
--  parseJSON _ = fail "dummy"
--deriving instance Generic Label
--instance FromJSON Label
instance FromJSON Label where
  parseJSON v = mkHooplLabel <$> (parseJSON v :: Parser Word64)



deriving instance Generic SectionType
instance FromJSON SectionType --where
--instance FromJSON SectionType where
--  parseJSON _= fail "dummy"



-- No-selector meta for S1 (use the type-level 'MetaSel constructor)
type NoSel =
  'MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy


type Rep_GenCmmStaticsTrue =
  D1 ('MetaData "GenCmmStatics 'True" "GHC.Cmm" "ghc" 'False)
    ( C1 ('MetaCons "CmmStaticsRaw" 'PrefixI 'False)
        ( S1 NoSel (Rec0 CLabel)
     :*: S1 NoSel (Rec0 [CmmStatic]) ) )


instance Generic (GenCmmStatics 'True) where
  type Rep (GenCmmStatics 'True) = Rep_GenCmmStaticsTrue
  from (CmmStaticsRaw lbl statics) =
    M1 (M1 ( M1 (K1 lbl) :*: M1 (K1 statics) ))
  to (M1 (M1 (a :*: b))) =
    CmmStaticsRaw (unK1 (unM1 a)) (unK1 (unM1 b))


--------------------------------------------------------------------------------
-- GenCmmStatics 'False  ==> CmmStatics  :+:  CmmStaticsRaw

type Rep_GenCmmStaticsFalse =
  D1 ('MetaData "GenCmmStatics 'False" "GHC.Cmm" "ghc" 'False)
    ( -- CmmStatics :: CLabel -> CmmInfoTable -> CostCentreStack
      --           -> [CmmLit] -> [CmmLit] -> GenCmmStatics 'False
      C1 ('MetaCons "CmmStatics" 'PrefixI 'False)
        ( S1 NoSel (Rec0 CLabel)
     :*: S1 NoSel (Rec0 CmmInfoTable)
     :*: S1 NoSel (Rec0 CostCentreStack)
     :*: S1 NoSel (Rec0 [CmmLit])
     :*: S1 NoSel (Rec0 [CmmLit]) )
      :+:
      -- CmmStaticsRaw :: CLabel -> [CmmStatic] -> GenCmmStatics a
      C1 ('MetaCons "CmmStaticsRaw" 'PrefixI 'False)
        ( S1 NoSel (Rec0 CLabel)
     :*: S1 NoSel (Rec0 [CmmStatic]) )
    )    

instance Generic (GenCmmStatics 'False) where
  type Rep (GenCmmStatics 'False) = Rep_GenCmmStaticsFalse
  from (CmmStatics lbl info ccs lits refs) =
    M1 (L1 (M1 ( M1 (K1 lbl)
               :*: M1 (K1 info)
               :*: M1 (K1 ccs)
               :*: M1 (K1 lits)
               :*: M1 (K1 refs) )))
  from (CmmStaticsRaw lbl statics) =
    M1 (R1 (M1 ( M1 (K1 lbl) :*: M1 (K1 statics) )))
  to (M1 (L1 (M1 (a :*: b :*: c :*: d :*: e)))) =
    CmmStatics (unK1 (unM1 a))
               (unK1 (unM1 b))
               (unK1 (unM1 c))
               (unK1 (unM1 d))
               (unK1 (unM1 e))
  to (M1 (R1 (M1 (a :*: b)))) =
    CmmStaticsRaw (unK1 (unM1 a)) (unK1 (unM1 b))

instance FromJSON (GenCmmStatics 'True) where
  parseJSON = genericParseJSON defaultOptions

-- CmmStatic aparece en CmmStaticsRaw :: CLabel -> [CmmStatic]
-- Necesaria para que genericParseJSON de GenCmmStatics 'True compile.
instance FromJSON CmmStatic where
  parseJSON _ = fail "dummy"


instance FromJSON (GenCmmStatics 'False) where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON CmmLit where
  parseJSON _ = fail "dummy"

instance FromJSON CostCentreStack where
  parseJSON _ = fail "dummy"

instance FromJSON CmmInfoTable where
  parseJSON _ = fail "dummy"


main :: IO ()
main = putStrLn "Hello, World!"
