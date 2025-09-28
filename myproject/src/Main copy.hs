{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
-- For testing
{-# LANGUAGE TypeApplications #-}
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

import Data.Aeson (eitherDecode) -- For testing
import qualified Data.ByteString.Lazy.Char8 as LBS

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

-- import GHC.Cmm.Reg (GlobalReg) -- register type used by CmmProc
-- Other GHC internals

import GHC.Data.FastString (fsLit)
import GHC.Types.Basic (FunctionOrData (..))
import GHC.Unit.Types (stringToUnitId)

import GHC.Types.CostCentre (CostCentreStack)

import GHC.Types.Unique (Unique, mkUnique, mkUniqueGrimily, mkUniqueIntGrimily)

-- import GHC.Runtime.Heap.Layout (SMRep)

import qualified Data.Semigroup as GHC.Runtime.Heap.Layout
import GHC.Runtime.Heap.Layout (ArgDescr (..), SMRep (..))

-- import GHC.Types.Var (Var(..))
import GHC.Types.Var (Var (), mkCoVar, mkExportedLocalVar, mkGlobalVar, mkLocalVar)

import GHC.Core.TyCo.Rep (Mult, Type)
import GHC.Types.Id.Info (IdDetails, IdInfo, vanillaIdInfo)
import GHC.Types.Name (Name)

import qualified GHC.Plugins as GHC.Types.FM -- debo deprecar esto
import GHC.Types.Id.Info (IdDetails (..))
import qualified GHC.Types.Unique.DFM as GHC.Types.FM

import GHC.Tc.Utils.TcType (ConcreteTvOrigin (..))

import Data.ByteString.Short (ShortByteString (..))

import GHC.Types.Name (
    Name,
    mkExternalName,
    mkInternalName,
    mkSystemName,
    mkSystemNameAt,
    nameOccName,
    nameUnique,
    setNameLoc,
 )

-- Piezas requeridas por esos ctors
import GHC.Types.Name.Occurrence (OccName, mkOccName, mkTcOcc, mkVarOcc)
import GHC.Types.SrcLoc (SrcSpan, noSrcSpan)
import GHC.Types.Unique (Unique)
import GHC.Unit.Module (Module)

import GHC.Types.ForeignCall (CCallSpec (..), CCallTarget (..), ForeignCall (..))
import qualified GHC.Types.ForeignCall as GHC.Types -- sus

-- import Data.Array.Byte (ByteArray(..))

-- Deja este para el TIPO ByteArray (sigue siendo el mismo tipo)
import Data.Array.Byte (ByteArray)

-- QUITA las funciones que antes intentabas traer de Data.Array.Byte
--   , MutableByteArray
--   , newByteArray
--   , writeByteArray
--   , unsafeFreezeByteArray

import GHC.Core.Class (Class (..))
import GHC.Core.ConLike (ConLike (..))
import GHC.Core.PatSyn (PatSyn (..))

import GHC.Builtin.PrimOps (PrimOp (..))

--Generic GHC.Core.TyCo.Rep.Type
import GHC.Core.TyCo.Rep (Type (..),TyLit(..))
--import GHC.Types (Tycon(..))

import GHC.Core.TyCon (TyCon(..))

import GHC.Core.TyCo.Rep (Coercion(..))
import GHC.Core.Coercion.Axiom (CoAxiom)

import GHC.Core.Coercion.Axiom ( CoAxiom(..), Branched(..),Branches(..),BranchIndex(..),CoAxBranch(..))


import GHC.Arr (Array(..))

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

-- C → O: parse a real JSON object
-- Accepted shape (minimum):
--   { "tag": "CmmEntry", "label": <Word64|Label>, "scope": "GlobalScope" }
-- - "scope" is optional; for now only "GlobalScope" is supported.
parseNodeC_O_json :: Value -> Parser (CmmNode C O)
parseNodeC_O_json = withObject "CmmNode C O" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
        "CmmEntry" -> do
            lbl <- o .: "label" :: Parser Label
            mscope <- o .:? "scope" :: Parser (Maybe Text)
            scope <- case mscope of
                Nothing -> pure GlobalScope
                Just "GlobalScope" -> pure GlobalScope
                Just other -> fail ("Unsupported CmmTickScope: " <> unpack other)
            pure (CmmEntry lbl scope)
        other ->
            fail ("Unsupported CmmNode C O tag: " <> unpack other)

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
    parseJSON = parseNodeC_O_json

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
    parseJSON :: Value -> Parser CostCentreStack
    parseJSON _ = fail "dummy"

deriving instance Generic CmmInfoTable

-- instance FromJSON CmmInfoTable where
-- parseJSON _ = fail "dummy"

instance FromJSON CmmInfoTable

--instance FromJSON GHC.Types.Var.Var where
--    parseJSON _ = fail "dummy"


instance FromJSON GHC.Types.Var.Var where
  parseJSON = withObject "Var" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      -- LocalVar: (IdDetails, Name, Mult, Type, Maybe IdInfo)
      "LocalVar" -> do
        (details, name, mult, ty, minfo)
          <- o .: "contents"
             :: Parser ( GHC.Types.Id.Info.IdDetails
                       , GHC.Types.Name.Name
                       , GHC.Core.TyCo.Rep.Mult
                       , GHC.Core.TyCo.Rep.Type
                       , Maybe GHC.Types.Id.Info.IdInfo )
        let info = maybe GHC.Types.Id.Info.vanillaIdInfo id minfo
        pure (GHC.Types.Var.mkLocalVar details name mult ty info)

      -- GlobalVar: (IdDetails, Name, Type, Maybe IdInfo)
      "GlobalVar" -> do
        (details, name, ty, minfo)
          <- o .: "contents"
             :: Parser ( GHC.Types.Id.Info.IdDetails
                       , GHC.Types.Name.Name
                       , GHC.Core.TyCo.Rep.Type
                       , Maybe GHC.Types.Id.Info.IdInfo )
        let info = maybe GHC.Types.Id.Info.vanillaIdInfo id minfo
        pure (GHC.Types.Var.mkGlobalVar details name ty info)

      -- ExportedLocalVar: (IdDetails, Name, Type, Maybe IdInfo)
      "ExportedLocalVar" -> do
        (details, name, ty, minfo)
          <- o .: "contents"
             :: Parser ( GHC.Types.Id.Info.IdDetails
                       , GHC.Types.Name.Name
                       , GHC.Core.TyCo.Rep.Type
                       , Maybe GHC.Types.Id.Info.IdInfo )
        let info = maybe GHC.Types.Id.Info.vanillaIdInfo id minfo
        pure (GHC.Types.Var.mkExportedLocalVar details name ty info)

      -- CoVar: (Name, Type)
      "CoVar" -> do
        (name, ty)
          <- o .: "contents"
             :: Parser ( GHC.Types.Name.Name
                       , GHC.Core.TyCo.Rep.Type )
        pure (GHC.Types.Var.mkCoVar name ty)

      other ->
        fail ("FromJSON Var: unknown tag " <> unpack other)


deriving instance Generic GHC.Core.TyCo.Rep.Type
instance FromJSON GHC.Core.TyCo.Rep.Type
--instance FromJSON GHC.Core.TyCo.Rep.Type where
--   parseJSON =
--    error "falla"




deriving instance Generic GHC.Core.TyCo.Rep.TyLit
instance FromJSON GHC.Core.TyCo.Rep.TyLit

--https://hackage-content.haskell.org/package/ghc-9.10.2/docs/GHC-Core-TyCon.html#t:TyCon
--deriving instance Generic GHC.Types.FM.TyCon
instance FromJSON GHC.Types.FM.TyCon
 where 
    parseJSON = 
        error "fala"

deriving instance Generic GHC.Types.FM.FunTyFlag
instance FromJSON GHC.Types.FM.FunTyFlag


--deriving instance Generic GHC.Plugins.Coercion
deriving instance Generic GHC.Core.TyCo.Rep.Coercion
instance FromJSON GHC.Types.FM.Coercion 
 where 
   parseJSON = 
       error "fala"

deriving instance Generic (GHC.Core.Coercion.Axiom.CoAxiom GHC.Core.Coercion.Axiom.Branched)    
instance FromJSON (GHC.Core.Coercion.Axiom.CoAxiom GHC.Core.Coercion.Axiom.Branched) 
  where 
    parseJSON =
        error "falla"


deriving instance Generic (GHC.Core.Coercion.Axiom.Branches Branched)
instance FromJSON ( GHC.Core.Coercion.Axiom.Branches Branched )
 where
    parseJSON =
        error "falla"


--deriving instance Generic (ghc-internal-9.1002.0:GHC.Internal.Arr.Array
--                       GHC.Core.Coercion.Axiom.BranchIndex
--                        GHC.Core.Coercion.Axiom.CoAxBranch)


--deriving instance Generic (GHC.Arr.Array
 --                      GHC.Core.Coercion.Axiom.BranchIndex
 --                       GHC.Core.Coercion.Axiom.CoAxBranch)
 --                 --      GHC.Arr.Array

-- JSON esperado: lista de pares [(i, e)]
-- Ej.: [[0, "a"], [1, "b"], [2, "c"]]
{-instance (Ix i, Ord i, FromJSON i, FromJSON e) => FromJSON (Array i e) where
  parseJSON v = do
    ps <- parseJSON v :: Parser [(i, e)]
    case ps of
      [] -> fail "Array FromJSON: lista vacía; no puedo deducir bounds"
      _  -> 
        let is = map fst ps
            lo = minimum is
            hi = maximum is
        in pure (array (lo, hi) ps)
-}

deriving instance Generic ( GHC.Types.FM.VarBndr GHC.Types.FM.TyCoVar GHC.Types.FM.ForAllTyFlag)
instance FromJSON ( GHC.Types.FM.VarBndr GHC.Types.FM.TyCoVar GHC.Types.FM.ForAllTyFlag)

deriving instance Generic GHC.Types.FM.ForAllTyFlag
instance FromJSON GHC.Types.FM.ForAllTyFlag


deriving instance Generic GHC.Types.FM.Specificity
instance FromJSON GHC.Types.FM.Specificity

instance FromJSON IdInfo where
    parseJSON =
        error "falla"

deriving instance Generic IdDetails
instance FromJSON IdDetails

instance FromJSON GHC.Types.FM.CbvMark where
    parseJSON =
        error "falla"

instance FromJSON GHC.Types.FM.TickBoxOp where
    parseJSON =
        error "falla"

instance FromJSON GHC.Builtin.PrimOps.PrimOp where
    parseJSON =
        error "falla"

-- check if i can import data constructor
instance FromJSON GHC.Core.Class.Class where
    parseJSON =
        error "falla"

deriving instance Generic GHC.Core.ConLike.ConLike

-- instance FromJSON GHC.Core.ConLike.ConLike
instance FromJSON GHC.Core.ConLike.ConLike where
    parseJSON =
        error "falla"

-- seems this type does not export data constructor, must check, but did import type itself
-- instance FromJSON PatSyn wnere
--  parseJSON =
--    error "falla"

instance FromJSON GHC.Types.FM.FieldLabel where
    parseJSON :: Value -> Parser GHC.Types.FM.FieldLabel
    parseJSON =
        error "falla pues"

instance FromJSON GHC.Types.FM.RecSelParent where
    parseJSON =
        error "falla pues"

-- deriving instance Generic GHC.Types.FM.DataCon

instance FromJSON GHC.Types.FM.DataCon where
    parseJSON :: Value -> Parser GHC.Types.FM.DataCon
    parseJSON =
        error "FromJSON Name (dummy): Name es abstracto; haremos una decodificación manual usando mk*Name."

deriving instance Generic GHC.Types.ForeignCall.ForeignCall
instance FromJSON GHC.Types.ForeignCall.ForeignCall

deriving instance Generic GHC.Types.ForeignCall.CCallSpec

-- instance FromJSON GHC.Types.ForeignCall.CCallSpec

instance FromJSON GHC.Types.ForeignCall.CCallSpec where
    parseJSON =
        error "Falla pues"

deriving instance Generic GHC.Types.ForeignCall.CCallTarget

-- instance FromJSON GHC.Types.CCallTarget

instance FromJSON GHC.Types.CCallTarget where
    parseJSON =
        error "Falla pues"

deriving instance Generic (GHC.Types.FM.GenUnit GHC.Types.FM.UnitId)
instance FromJSON (GHC.Types.FM.GenUnit GHC.Types.FM.UnitId)

instance FromJSON (GHC.Types.FM.GenInstantiatedUnit GHC.Types.FM.UnitId) where
    parseJSON =
        error "falla pues"

deriving instance Generic (GHC.Types.FM.Definite GHC.Types.FM.UnitId)
instance FromJSON (GHC.Types.FM.Definite GHC.Types.FM.UnitId)

deriving instance Generic GHC.Types.FM.UnitId
instance FromJSON GHC.Types.FM.UnitId

deriving instance Generic GHC.Types.FM.FastString
instance FromJSON GHC.Types.FM.FastString

-- instance FromJSON GHC.Types.FM.RecSelParent where
--  parseJSON =
--    fail "falla pues"

instance FromJSON GHC.Types.FM.FastZString where
    parseJSON =
        error "falla pues"

-- deriving instance Generic Data.ByteString.Short.ShortByteString
instance FromJSON Data.ByteString.Short.ShortByteString

-- deriving instance Generic Data.Array.Byte.ByteArray no se puede generar por ser tipo primigeneo

-- Instancia dummy: JSON → [Word8] → ShortByteString → ByteArray interno
-- instance FromJSON ByteArray where
-- parseJSON v = do
--  ws <- parseJSON v :: Parser BS.ByteString  -- Aeson ya sabe convertir de String→ByteString
-- convertimos a ShortByteString y sacamos el ByteArray interno
--  let sbs = toShort ws
-- Ojo: esto usa el campo interno de ShortByteString (que es ByteArray)
--     arr = case sbs of SBS ba -> ba
-- pure arr

instance FromJSON ByteArray where
    parseJSON =
        error "FromJSON Name (dummy): Name es abstracto; haremos una decodificación manual usando mk*Name."

-- deriving instance Generic (GHC.Types.FM.UniqFM Name GHC.Tc.Utils.TcType.ConcreteTvOrigin )
-- UniqFM does not export constructor
-- https://hackage-content.haskell.org/package/ghc-9.12.2/docs/GHC-Types-Unique-FM.html

instance FromJSON (GHC.Types.FM.UniqFM Name GHC.Tc.Utils.TcType.ConcreteTvOrigin) where
    parseJSON :: Value -> Parser (GHC.Types.FM.UniqFM Name ConcreteTvOrigin)
    parseJSON =
        error "FromJSON Name (dummy): Name es abstracto; haremos una decodificación manual usando mk*Name."

instance FromJSON GHC.Types.Name.Name where
    parseJSON =
        error "FromJSON Name (dummy): Name es abstracto; haremos una decodificación manual usando mk*Name."

deriving instance Generic ProfilingInfo
instance FromJSON ProfilingInfo

deriving instance Generic GHC.Runtime.Heap.Layout.SMRep
instance FromJSON GHC.Runtime.Heap.Layout.SMRep

deriving instance Generic ClosureTypeInfo
instance FromJSON ClosureTypeInfo

deriving instance Generic GHC.Runtime.Heap.Layout.ArgDescr
instance FromJSON GHC.Runtime.Heap.Layout.ArgDescr

deriving instance Generic CmmExpr
instance FromJSON CmmExpr

deriving instance Generic MachOp
instance FromJSON MachOp

deriving instance Generic FMASign
instance FromJSON FMASign

deriving instance Generic AlignmentSpec
instance FromJSON AlignmentSpec

deriving instance Generic Area
instance FromJSON Area

deriving instance Generic CmmReg
instance FromJSON CmmReg

deriving instance Generic GlobalRegUse
instance FromJSON GlobalRegUse

deriving instance Generic LocalReg
instance FromJSON LocalReg

-- Emulación del derivado genérico para: newtype Unique = MkUnique Word64
-- Se decodifica exactamente como Word64 y luego se construye el Unique.
instance FromJSON GHC.Types.Unique.Unique where
    parseJSON v =
        (GHC.Types.Unique.mkUniqueGrimily <$> (parseJSON v :: Parser Word64))

-- reemplaza tu main por este
main :: IO ()
main = do
    putStrLn "== CmmNode C O JSON tests =="

    -- 1) OK: objeto válido
    let ok = "{\"tag\":\"CmmEntry\",\"label\":0,\"scope\":\"GlobalScope\"}"
    test "OK (object with tag/label/scope)" ok

    -- 2) OK sin scope (usa GlobalScope por defecto)
    let okNoScope = "{\"tag\":\"CmmEntry\",\"label\":1}"
    test "OK (object without scope → defaults to GlobalScope)" okNoScope

    -- 3) Falla: scope no soportado
    let badScope = "{\"tag\":\"CmmEntry\",\"label\":0,\"scope\":\"LocalScope\"}"
    test "FAIL (unsupported scope)" badScope

    -- 4) Falla: tag no soportado
    let badTag = "{\"tag\":\"NotEntry\",\"label\":0}"
    test "FAIL (unsupported tag)" badTag

    -- 5) Falla: falta label
    let missingLabel = "{\"tag\":\"CmmEntry\"}"
    test "FAIL (missing label)" missingLabel

    -- 6) Falla: string “legacy” (ya no aceptamos Text puro)
    let legacy = "\"CmmEntry\""
    test "FAIL (string form no longer accepted)" legacy

    putStrLn "== Done =="
  where
    test msg js = do
        putStrLn $ "→ " <> msg
        case eitherDecode @(CmmNode C O) (LBS.pack js) of
            Right _ -> putStrLn "   decoded: OK"
            Left e -> putStrLn $ "   decoded: ERROR → " <> e

-- ===== FromJSON for CmmType (emulating genericParseJSON defaultOptions) =====
-- JSON shape expected (TaggedObject):
--   { "tag":"CmmType", "contents":[ <CmmCat>, <Width> ] }
--   CmmCat:
--     { "tag":"BitsCat" }
--     { "tag":"FloatCat" }
--     { "tag":"GcPtrCat" }
--     { "tag":"VecCat", "contents":[ <Length :: Int>, <CmmCat> ] }
--   Width:
--     { "tag":"W8" | "W16" | "W32" | "W64" | "W128" | "W256" | "W512" }

-- Open mirror of the internal category so we can rebuild using public ctors
data CmmCatOpen
    = OGcPtr
    | OBits
    | OFloat
    | OVec Int CmmCatOpen
    deriving (Show, Eq)

instance FromJSON CmmCatOpen where
    parseJSON = withObject "CmmCat" $ \o -> do
        tag <- o .: "tag" :: Parser Text
        case tag of
            "GcPtrCat" -> pure OGcPtr
            "BitsCat" -> pure OBits
            "FloatCat" -> pure OFloat
            "VecCat" -> do
                cs <- o .: "contents"
                (n, sub) <- Data.Aeson.parseJSON cs :: Parser (Int, CmmCatOpen)
                pure (OVec n sub)
            other -> fail ("Unknown CmmCat tag: " <> unpack other)

instance FromJSON Width where
    parseJSON = withObject "Width" $ \o -> do
        tag <- o .: "tag" :: Parser Text
        case tag of
            "W8" -> pure W8
            "W16" -> pure W16
            "W32" -> pure W32
            "W64" -> pure W64
            "W128" -> pure W128
            "W256" -> pure W256
            "W512" -> pure W512
            other -> fail ("Unknown Width tag: " <> unpack other)

instance FromJSON CmmType where
    parseJSON = withObject "CmmType" $ \o -> do
        tag <- o .: "tag" :: Parser Text
        case tag of
            "CmmType" -> do
                cs <- o .: "contents"
                (cat, w) <- parseJSON cs :: Parser (CmmCatOpen, Width)
                build cat w
            other -> fail ("Expected tag CmmType, got: " <> unpack other)
      where
        build :: CmmCatOpen -> Width -> Parser CmmType
        build OBits w = pure (cmmBits w)
        build OFloat w = pure (cmmFloat w)
        build (OVec n c) w = vec n <$> build c w
        build OGcPtr _ =
            -- Requires a Platform to construct (gcWord). Provide a Platform-aware parser if needed.
            fail "GcPtrCat requires a Platform (use a Platform-aware parser to call gcWord)."