{-

{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric      #-}

{-# LANGUAGE DeriveDataTypeable #-}  -- <- required for deriving Data

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

import Codec.Serialise
import qualified Data.ByteString.Lazy as BSL

-- Save as: src/CmmGenericInstances.hs
-- Make sure your .cabal or package.yaml depends on the 'ghc' library.


--module CmmGenericInstances () where

import GHC.Generics (Generic)
import GHC.Cmm

import Data.Data     (Data)

import Util

import Data.Aeson


-- defined in line 195 Cmm.hs
--type DCmmTopInfo = GenCmmTopInfo DWrap
--type CmmTopInfo  = GenCmmTopInfo LabelMap

-- define same file , line 150
--type CmmGraph = GenCmmGraph CmmNode
--type DCmmGraph = GenGenCmmGraph DWrap CmmNode


-- Orphan instance for the concrete DCmm specialization of GenCmmDecl
--deriving instance Generic (GenCmmDecl CmmStatics DCmmTopInfo DCmmGraph)


--deriving instance Generic (GenCmmDecl CmmStatics CmmTopInfo CmmGraph)
--deriving instance Generic (GenCmmGraph CmmNode)
--deriving instance Generic (GenCmmStatics False)
--deriving instance Data    (GenCmmDecl CmmStatics CmmTopInfo CmmGraph)

-- Per-index Generic instances:
--deriving instance Generic (GenCmmStatics 'False)
--deriving instance Generic (GenCmmStatics 'True)


--deriving instance Generic (GenCmmStatics a) 

instance FromJSON (GenCmmDecl CmmStatics CmmTopInfo CmmGraph) where
   parseJSON _ = fail "fromJSON for GlobalReg is not implemented (dummy instance)"

{-
instance FromJSON (GenCmmGraph CmmNode)
instance FromJSON (GenCmmStatics False)
--instance FromJSON GlobalReg
--instance FromJSON GHC.Cmm.CLabel.CLabel
instance FromJSON GlobalReg where
  parseJSON _ = fail "fromJSON for GlobalReg is not implemented (dummy instance)"
  -}
--instance FromJSON GHC.Cmm.CLabel.CLabel where
--  parseJSON _ = fail "fromJSON for GlobalReg is not implemented (dummy instance)"
  



--instance Serialise (GenCmmDecl CmmStatics CmmTopInfo CmmGraph)
--instance Serialise (GenCmmGraph CmmNode)
--instance Serialise (GenCmmStatics False)

-}



{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

--module CmmJSON where

import GHC.Cmm --(GenCmmStatics(..))
import Data.Aeson --(FromJSON(..))

-- Always-failing dummy parser
instance FromJSON (GenCmmStatics rawOnly) where
  parseJSON _ = fail "dummy FromJSON for GenCmmStatics"



main :: IO ()
main = putStrLn "Hello, World!"

--that compiles, now to try to derive aeson or cborg (de)serialization , whicever you find easiest


