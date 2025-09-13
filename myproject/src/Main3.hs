{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Aeson (FromJSON(..), Value)
import qualified Data.Aeson as Aeson

import GHC.Generics
  ( Generic(..), (:*:)(..), (:+:)(..)
  , M1(..), K1(..), C1, D1, S1, Rec0
  , Meta(..)                 -- 'MetaData, 'MetaCons, 'MetaSel
  , FixityI(..)              -- PrefixI, InfixI ...
  , SourceUnpackedness(..)   -- NoSourceUnpackedness
  , SourceStrictness(..)     -- NoSourceStrictness
  , DecidedStrictness(..)    -- DecidedLazy
  )

-- GHC API (ghc-9.10.2): https://hackage-content.haskell.org/package/ghc-9.10.2/docs/GHC-Cmm.html
import GHC.Cmm
  ( GenCmmStatics(..)   -- CmmStatics, CmmStaticsRaw
  , CmmStatic
  , CmmInfoTable
  )
import GHC.Cmm.CLabel (CLabel)
import GHC.Cmm.Expr   (CmmLit)
import GHC.Types.CostCentre (CostCentreStack)

-- No-selector meta for S1 (use the type-level 'MetaSel constructor)
type NoSel =
  'MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy

--------------------------------------------------------------------------------
-- GenCmmStatics 'True  ==> only CmmStaticsRaw

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
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions




