{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Save as: src/CmmGenericInstances.hs
-- Make sure your .cabal or package.yaml depends on the 'ghc' library.

--module CmmGenericInstances () where

import GHC.Generics (Generic)
import GHC.Cmm


-- defined in line 195 Cmm.hs
--type DCmmTopInfo = GenCmmTopInfo DWrap
--type CmmTopInfo  = GenCmmTopInfo LabelMap

-- define same file , line 150
--type CmmGraph = GenCmmGraph CmmNode
--type DCmmGraph = GenGenCmmGraph DWrap CmmNode


-- Orphan instance for the concrete DCmm specialization of GenCmmDecl
--deriving instance Generic (GenCmmDecl CmmStatics DCmmTopInfo DCmmGraph)


deriving instance Generic (GenCmmDecl CmmStatics CmmTopInfo CmmGraph)
