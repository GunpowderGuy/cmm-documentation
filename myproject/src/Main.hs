{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- for manual generic implementation
{-# LANGUAGE TypeOperators #-}

module Main where

-- Standard library imports
import Data.Aeson (FromJSON (..), Value, defaultOptions, genericParseJSON, withObject, withText)
import Data.Aeson.Types (Parser, (.:), (.:?))
import qualified Data.ByteString as BS
import Data.Text
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)

-- GHC Generics (basic + detailed)
import GHC.Generics (
    C1,
    D1,
    -- 'MetaData, 'MetaCons, 'MetaSel
    -- PrefixI, InfixI ...
    -- NoSourceUnpackedness
    -- NoSourceStrictness
    DecidedStrictness (..), -- DecidedLazy
    FixityI (..),
    Generic (..),
    Generic1,
    K1 (..),
    M1 (..),
    Meta (..),
    Rec0,
    S1,
    SourceStrictness (..),
    SourceUnpackedness (..),
    (:*:) (..),
    (:+:) (..),
 )

-- GHC internals: Cmm & related
import GHC.Cmm
import GHC.Cmm.CLabel (CLabel, ForeignLabelSource (..), mkForeignLabel) -- label type used by Section and CmmProc
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Reg (GlobalReg) -- register type used by CmmProc
-- Other GHC internals

import GHC.Data.FastString (fsLit)
import GHC.Types.Basic (FunctionOrData (..))
import GHC.Unit.Types (stringToUnitId)

import GHC.Types.CostCentre (CostCentreStack)

-- Allow Aeson Generic-based instance at the top level
deriving instance Generic (GenCmmDecl CmmStatics CmmTopInfo CmmGraph)
instance FromJSON (GenCmmDecl CmmStatics CmmTopInfo CmmGraph)

deriving instance Generic CmmTopInfo

-- h
instance FromJSON CmmTopInfo

deriving instance Generic (GenCmmGraph CmmNode)

-- g
instance FromJSON (GenCmmGraph CmmNode)

-- CmmNode is higher-kinded (Extensibility -> Extensibility -> *)
-- Seems iffy whether i can derive generic
-- deriving instance Generic (CmmNode e x)
-- instance FromJSON (CmmNode e x) where
--    parseJSON _ = fail "dummy FromJSON for CmmNode"

-- Fields in CmmProc/CmmData:
-- deriving instance Generic CLabel
-- Can't make a derived instance of ‘Generic CLabel’:The data constructors of
-- ‘CLabel’ are not all in scope so you cannot derive an instance for it
-- instance FromJSON CLabel where
--    parseJSON _ = fail "dummy FromJSON for CLabel"

-- parseFOD :: Text -> Parser FunctionOrData
-- parseFOD "Function" = pure IsFunction
-- parseFOD "Data"     = pure IsData
-- parseFOD t          = fail $ "Unknown kind: " <> BS.unpack t

-- main CLabel parser
instance FromJSON CLabel where
    parseJSON = withObject "CLabel" $ \o -> do
        tag <- o .: "tag" :: Parser Text
        case tag of
            "ForeignLabel" -> do
                name <- o .: "name"
                bytes <- o .:? "stdcallBytes"
                srcVal <- o .: "source"
                src <- parseFLSrc srcVal
                fod <- o .: "kind" >>= parseFOD
                pure (mkForeignLabel (fsLit name) bytes src fod)

            -- You can add other supported smart constructors here (mkCmmInfoLabel, mkCmmEntryLabel, etc.)
            other -> fail $ "Unsupported CLabel tag: " <> unpack other

-- parse a ForeignLabelSource
parseFLSrc :: Value -> Parser ForeignLabelSource
parseFLSrc = withObject "ForeignLabelSource" $ \o -> do
    tag <- o .: "tag"
    case (tag :: Text) of
        "InThisPackage" -> pure ForeignLabelInThisPackage
        "InExternalPackage" -> pure ForeignLabelInExternalPackage
        "InPackage" -> ForeignLabelInPackage . stringToUnitId <$> o .: "unitId"
        other -> fail $ "Unknown ForeignLabelSource: " <> unpack other

parseFOD :: Text -> Parser FunctionOrData
parseFOD t
    | t == pack "Function" = pure IsFunction
    | t == pack "Data" = pure IsData
    | otherwise = fail $ "Unknown kind: " <> unpack t

deriving instance Generic GlobalReg
instance FromJSON GlobalReg

deriving instance Generic Section
instance FromJSON Section

-- These instances are needed for CmmTopInfos
-- instance FromJSON (GHC.Cmm.Dataflow.Label.LabelMap CmmInfoTable) where
--  parseJSON _= fail "dummy"

-- JSON como lista de pares [(Word64, a)] -> LabelMap a
instance (FromJSON a) => FromJSON (LabelMap a) where
    parseJSON v = do
        ps <- (parseJSON v :: Parser [(Word64, Value)])
        pairs <-
            mapM
                ( \(w, val) -> do
                    x <- parseJSON val -- x :: a (se infiere por el 'a' del instance)
                    pure (mkHooplLabel w, x)
                )
                ps
        pure (mapFromList pairs)

deriving instance Generic CmmStackInfo
instance FromJSON CmmStackInfo

-- needed because of instance FromJSON CmmTopInfo

parseCmmNode :: String -> CmmNode C O
parseCmmNode _ = undefined

{- | Given the exact string "CmmEntry", produce the simplest possible
CmmNode C O value. Anything else is rejected.
-}
parseNodeC_O :: String -> CmmNode C O
parseNodeC_O "CmmEntry" = CmmEntry (mkHooplLabel 0) GlobalScope
parseNodeC_O _ = error "Unsupported CmmNode C O; expected \"CmmEntry\""

-- | O→C: now handles the two additional simplest cases: CmmSwitch and CmmForeignCall
parseNodeO_C :: String -> CmmNode O C
parseNodeO_C "CmmBranch" =
    CmmBranch (mkHooplLabel 0)
parseNodeO_C "CmmCondBranch" =
    CmmCondBranch
        { cml_pred = error "stub: CmmExpr predicate"
        , cml_true = mkHooplLabel 0
        , cml_false = mkHooplLabel 1
        , cml_likely = Nothing
        }
parseNodeO_C "CmmCall" =
    CmmCall
        { cml_target = error "stub: CmmExpr target"
        , cml_cont = Nothing
        , cml_args_regs = []
        , cml_args = 0
        , cml_ret_args = 0
        , cml_ret_off = 0
        }
-- added #1
parseNodeO_C "CmmSwitch" =
    CmmSwitch (error "stub: CmmExpr scrutinee") (error "stub: SwitchTargets")
-- added #2
parseNodeO_C "CmmForeignCall" =
    CmmForeignCall
        (error "stub: target")
        (error "stub: results")
        (error "stub: args")
        (mkHooplLabel 0)
        0
        0
        False
parseNodeO_C _ =
    error "Unsupported CmmNode O C; expected one of {CmmBranch,CmmCondBranch,CmmCall,CmmSwitch,CmmForeignCall}"

-- | Open→Open nodes: return the simplest possible value for each tag.
parseNodeO_O :: String -> CmmNode O O
parseNodeO_O "CmmComment" = CmmComment (error "stub: FastString")
parseNodeO_O "CmmTick" = CmmTick (error "stub: CmmTickish")
parseNodeO_O "CmmUnwind" = CmmUnwind []
parseNodeO_O "CmmAssign" = CmmAssign (error "stub: CmmReg") (error "stub: CmmExpr")
parseNodeO_O "CmmStore" = CmmStore (error "stub: addr") (error "stub: rhs") (error "stub: AlignmentSpec")
parseNodeO_O "CmmUnsafeForeignCall" = CmmUnsafeForeignCall (error "stub: ForeignTarget") [] []
parseNodeO_O _ = error "Unsupported CmmNode O O"

-- import Data.Aeson (withText)

-- | C → O nodes
instance FromJSON (CmmNode C O) where
    parseJSON = withText "CmmNode C O" $ \t ->
        pure (parseNodeC_O (toString t))
      where
        toString = Data.Text.unpack

-- | O → C nodes
instance FromJSON (CmmNode O C) where
    parseJSON = withText "CmmNode O C" $ \t ->
        pure (parseNodeO_C (toString t))
      where
        toString = Data.Text.unpack

-- | O → O nodes
instance FromJSON (CmmNode O O) where
    parseJSON = withText "CmmNode O O" $ \t ->
        pure (parseNodeO_O (toString t))
      where
        toString = Data.Text.unpack

-- https://hackage-content.haskell.org/package/ghc-9.10.2/docs/GHC-Cmm-Node.html#t:CmmNode
instance
    FromJSON
        ( GHC.Cmm.Dataflow.Graph.Graph'
            GHC.Cmm.Dataflow.Block.Block
            CmmNode
            GHC.Cmm.Dataflow.Block.C
            GHC.Cmm.Dataflow.Block.C
        )
    where
    parseJSON _ = fail "dummy"

-- import GHC.Cmm.Dataflow.Graph are imported because of
-- import GHC.Cmm.Dataflow.Block  the above
---
---
---Needed for GenCmmGraph CmmNode

instance FromJSON Label where
    parseJSON v = mkHooplLabel <$> (parseJSON v :: Parser Word64)

deriving instance Generic SectionType
instance FromJSON SectionType

-- No-selector meta for S1 (use the type-level 'MetaSel constructor)
type NoSel =
    'MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy

type Rep_GenCmmStaticsTrue =
    D1
        ('MetaData "GenCmmStatics 'True" "GHC.Cmm" "ghc" 'False)
        ( C1
            ('MetaCons "CmmStaticsRaw" 'PrefixI 'False)
            ( S1 NoSel (Rec0 CLabel)
                :*: S1 NoSel (Rec0 [CmmStatic])
            )
        )

instance Generic (GenCmmStatics 'True) where
    type Rep (GenCmmStatics 'True) = Rep_GenCmmStaticsTrue
    from (CmmStaticsRaw lbl statics) =
        M1 (M1 (M1 (K1 lbl) :*: M1 (K1 statics)))
    to (M1 (M1 (a :*: b))) =
        CmmStaticsRaw (unK1 (unM1 a)) (unK1 (unM1 b))

--------------------------------------------------------------------------------
-- GenCmmStatics 'False  ==> CmmStatics  :+:  CmmStaticsRaw

type Rep_GenCmmStaticsFalse =
    D1
        ('MetaData "GenCmmStatics 'False" "GHC.Cmm" "ghc" 'False)
        ( -- CmmStatics :: CLabel -> CmmInfoTable -> CostCentreStack
          --           -> [CmmLit] -> [CmmLit] -> GenCmmStatics 'False
          C1
            ('MetaCons "CmmStatics" 'PrefixI 'False)
            ( S1 NoSel (Rec0 CLabel)
                :*: S1 NoSel (Rec0 CmmInfoTable)
                :*: S1 NoSel (Rec0 CostCentreStack)
                :*: S1 NoSel (Rec0 [CmmLit])
                :*: S1 NoSel (Rec0 [CmmLit])
            )
            :+:
            -- CmmStaticsRaw :: CLabel -> [CmmStatic] -> GenCmmStatics a
            C1
                ('MetaCons "CmmStaticsRaw" 'PrefixI 'False)
                ( S1 NoSel (Rec0 CLabel)
                    :*: S1 NoSel (Rec0 [CmmStatic])
                )
        )

instance Generic (GenCmmStatics 'False) where
    type Rep (GenCmmStatics 'False) = Rep_GenCmmStaticsFalse
    from (CmmStatics lbl info ccs lits refs) =
        M1
            ( L1
                ( M1
                    ( M1 (K1 lbl)
                        :*: M1 (K1 info)
                        :*: M1 (K1 ccs)
                        :*: M1 (K1 lits)
                        :*: M1 (K1 refs)
                    )
                )
            )
    from (CmmStaticsRaw lbl statics) =
        M1 (R1 (M1 (M1 (K1 lbl) :*: M1 (K1 statics))))
    to (M1 (L1 (M1 (a :*: b :*: c :*: d :*: e)))) =
        CmmStatics
            (unK1 (unM1 a))
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
deriving instance Generic CmmStatic
instance FromJSON CmmStatic

-- Necessary for CmmStatic instance
instance FromJSON BS.ByteString where
    parseJSON = withText "ByteString" (pure . TE.encodeUtf8)

instance FromJSON (GenCmmStatics 'False) where
    parseJSON = genericParseJSON defaultOptions

deriving instance Generic CmmLit
instance FromJSON CmmLit where
    parseJSON _ = fail "dummy"

-- data structures are not all in scope
instance FromJSON CostCentreStack where
    parseJSON _ = fail "dummy"

deriving instance Generic CmmInfoTable
instance FromJSON CmmInfoTable where
    parseJSON _ = fail "dummy"

main :: IO ()
main = putStrLn "Hello, World!"