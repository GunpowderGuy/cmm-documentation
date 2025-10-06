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


import GHC.Arr (Array(..), array)

import GHC.Types.Tickish ( GenTickish (..)) 


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


-- | O → O nodes: parse JSON into real constructors with arguments.
parseNodeO_O_json :: Value -> Parser (CmmNode O O)
parseNodeO_O_json = withObject "CmmNode O O" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
        "CmmComment" -> do
            txt <- o .: "contents" :: Parser Text
            pure (CmmComment (fsLit (unpack txt)))

        "CmmTick" -> do
            tick <- o .: "contents" :: Parser CmmTickish
            pure (CmmTick tick)

        "CmmUnwind" -> do
            regs <- o .: "contents" :: Parser [(GlobalReg, Maybe CmmExpr)]
            pure (CmmUnwind regs)

        "CmmAssign" -> do
            (reg, expr) <- o .: "contents" :: Parser (CmmReg, CmmExpr)
            pure (CmmAssign reg expr)

        "CmmStore" -> do
            (addr, rhs, align) <- o .: "contents" :: Parser (CmmExpr, CmmExpr, AlignmentSpec)
            pure (CmmStore addr rhs align)

        "CmmUnsafeForeignCall" -> do
            (tgt, results, args) <- o .: "contents" :: Parser (ForeignTarget, [CmmFormal], [CmmActual])
            pure (CmmUnsafeForeignCall tgt results args)

        other ->
            fail ("Unsupported CmmNode O O tag: " <> unpack other)




--deriving instance Generic (GenTickish 'TickishPassCmmC)

instance FromJSON CmmTickish where
    parseJSON _ = fail "FromJSON CmmTickish: dummy instance"


instance FromJSON ForeignTarget where
    parseJSON _ = fail "FromJSON ForeignTarget: dummy instance"

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


-- | C → O nodes
instance FromJSON (CmmNode C O) where
    parseJSON = parseNodeC_O_json


-- | O → O nodes
instance FromJSON (CmmNode O O) where
    parseJSON = parseNodeO_O_json


-- | O → C nodes: parse JSON into real constructors with arguments.
parseNodeO_C_json :: Value -> Parser (CmmNode O C)
parseNodeO_C_json = withObject "CmmNode O C" $ \o -> do
  tag <- o .: "tag" :: Parser Text
  case tag of
    -- { "tag":"CmmBranch", "label": <Label> }
    "CmmBranch" -> do
      lbl <- o .: "label" :: Parser Label
      pure (CmmBranch lbl)

    -- { "tag":"CmmCondBranch",
    --   "contents": [ <pred :: CmmExpr>, <true :: Label>, <false :: Label>, <likely :: Maybe Bool> ] }
    "CmmCondBranch" -> do
      (p, t, f, lk) <- o .: "contents" :: Parser (CmmExpr, Label, Label, Maybe Bool)
      pure CmmCondBranch
            { cml_pred   = p
            , cml_true   = t
            , cml_false  = f
            , cml_likely = lk
            }

    -- { "tag":"CmmCall",
    --   "contents": [ <target :: CmmExpr>
    --               , <cont   :: Maybe Label>
    --               , <regs   :: [GlobalReg]>
    --               , <args   :: Int>
    --               , <retArgs:: Int>
    --               , <retOff :: Int> ] }
    "CmmCall" -> do
      (tgt, mcont, regs, args, retArgs, retOff)
        <- o .: "contents" :: Parser (CmmExpr, Maybe Label, [GlobalReg], Int, Int, Int)
      pure CmmCall
            { cml_target   = tgt
            , cml_cont     = mcont
            , cml_args_regs= regs
            , cml_args     = args
            , cml_ret_args = retArgs
            , cml_ret_off  = retOff
            }

    -- Pendientes: requieren tipos/instancias extra (SwitchTargets, ForeignTarget, CmmFormal/Actual, …)
    "CmmSwitch"       -> fail "parseNodeO_C_json: CmmSwitch no soportado aún (SwitchTargets)."
    "CmmForeignCall"  -> fail "parseNodeO_C_json: CmmForeignCall no soportado aún (ForeignTarget/[CmmFormal]/[CmmActual])."

    other -> fail ("Unsupported CmmNode O C tag: " <> unpack other)

-- | O → C nodes
instance FromJSON (CmmNode O C) where
  parseJSON = parseNodeO_C_json


--deriving instance Generic  ( GHC.Cmm.Dataflow.Block.Block CmmNode GHC.Cmm.Dataflow.Block.C GHC.Cmm.Dataflow.Block.C )

-- Graph' Block CmmNode C C  (closed entry, closed exit)
instance
  FromJSON
    ( GHC.Cmm.Dataflow.Graph.Graph'
        GHC.Cmm.Dataflow.Block.Block
        CmmNode
        GHC.Cmm.Dataflow.Block.C
        GHC.Cmm.Dataflow.Block.C
    )
  where
  parseJSON = withObject "Graph' Block CmmNode C C" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      -- Accept the legacy 3-tuple but ignore entry/exit blocks for C/C graphs.
      -- { "tag":"GMany"
      -- , "contents": [ <_entry :: Block O C>            -- ignored
      --               , <body   :: LabelMap (Block C C)>  -- used
      --               , <_exit  :: Block C O> ] }         -- ignored
      "GMany" -> do
        (_entryOC, bodyCC, _exitCO)
          <- o .: "contents"
             :: Parser
                  ( Block CmmNode O C
                  , LabelMap (Block CmmNode C C)
                  , Block CmmNode C O
                  )
        -- Closed/Closed graphs carry no explicit entry/exit blocks:
        -- entry  :: MaybeO C (Block O C)  = NothingO
        -- exit   :: MaybeO C (Block C O)  = NothingC
        pure (GMany NothingO bodyCC NothingO)

      other ->
        fail ("Unsupported Graph' tag: " <> unpack other)


-- https://hackage-content.haskell.org/package/ghc-9.10.2/docs/GHC-Cmm-Node.html#t:CmmNode

{-
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
-}

-- Block C O  (entry node + open-open middle)
instance FromJSON (Block CmmNode C O) where
  parseJSON = withObject "Block C O" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      -- { "tag":"BFirst", "contents": <CmmNode C O> }
      "BFirst" -> do
        hd <- o .: "contents" :: Parser (CmmNode C O)
        pure (BlockCO hd BNil)  -- single-entry, empty middle

      -- { "tag":"BHead", "contents": [ <CmmNode C O>, <Block O O> ] }
      "BHead" -> do
        (hd, mid) <- o .: "contents" :: Parser (CmmNode C O, Block CmmNode O O)
        pure (BlockCO hd mid)

      -- { "tag":"BCat", "contents": [ <Block C O>, <Block O O> ] }
      "BCat" -> do
        (b1, b2) <- o .: "contents" :: Parser (Block CmmNode C O, Block CmmNode O O)
        pure (blockAppend b1 b2)  -- append OO tail to CO, stays CO

      other -> fail ("Unsupported Block C O tag: " <> unpack other)


-- Block O O  (pure middle)
instance FromJSON (Block CmmNode O O) where
  parseJSON = withObject "Block O O" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      -- { "tag":"BNil" }
      "BNil"     -> pure BNil
      -- { "tag":"BMiddle", "contents": <CmmNode O O> }
      "BMiddle"  -> BMiddle <$> (o .: "contents" :: Parser (CmmNode O O))
      -- { "tag":"BCat", "contents": [ <Block O O>, <Block O O> ] }
      "BCat"     -> do (b1,b2) <- o .: "contents"; pure (BCat b1 b2)
      -- { "tag":"BCons", "contents": [ <CmmNode O O>, <Block O O> ] }
      "BCons"    -> do (n,b) <- o .: "contents";  pure (BCons n b)
      -- { "tag":"BSnoc", "contents": [ <Block O O>, <CmmNode O O> ] }
      "BSnoc"    -> do (b,n) <- o .: "contents";  pure (BSnoc b n)
      other      -> fail ("Unsupported Block O O tag: " <> unpack other)

-- Block O C  (open-open middle + last node)
instance FromJSON (Block CmmNode O C) where
  parseJSON = withObject "Block O C" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      -- { "tag":"BLast", "contents": <CmmNode O C> }
      "BLast" -> do
        lt <- o .: "contents" :: Parser (CmmNode O C)
        pure (BlockOC BNil lt)     -- empty middle + last

      -- { "tag":"BTail", "contents": [ <Block O O>, <CmmNode O C> ] }
      "BTail" -> do
        (mid, lt) <- o .: "contents" :: Parser (Block CmmNode O O, CmmNode O C)
        pure (BlockOC mid lt)

      -- { "tag":"BCat", "contents": [ <Block O O>, <Block O C> ] }
      "BCat" -> do
        (b1, b2) <- o .: "contents" :: Parser (Block CmmNode O O, Block CmmNode O C)
        pure (blockAppend b1 b2)    -- OO + OC → OC

      other -> fail ("Unsupported Block O C tag: " <> unpack other)

-- Block C C  (entry node + middle OO + last node)
instance FromJSON (Block CmmNode C C) where
  parseJSON = withObject "Block C C" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      -- { "tag":"BCat",  "contents": [ <Block C O>, <Block O C> ] }
      "BCat" -> do
        (bco, boc) <- o .: "contents" :: Parser (Block CmmNode C O, Block CmmNode O C)
        let (hd, mid1) = blockSplitHead bco
            (mid2, lt) = blockSplitTail boc
            mid        = blockConcat [mid1, mid2]
        pure (BlockCC hd mid lt)

      -- { "tag":"BHead", "contents": [ <CmmNode C O>, <Block O C> ] }
      "BHead" -> do
        (hd, boc) <- o .: "contents" :: Parser (CmmNode C O, Block CmmNode O C)
        let (mid, lt) = blockSplitTail boc
        pure (BlockCC hd mid lt)

      -- { "tag":"BTail", "contents": [ <Block C O>, <CmmNode O C> ] }
      "BTail" -> do
        (bco, lt) <- o .: "contents" :: Parser (Block CmmNode C O, CmmNode O C)
        let (hd, mid) = blockSplitHead bco
        pure (BlockCC hd mid lt)

      other -> fail ("Unsupported Block C C tag: " <> unpack other)

-- import GHC.Cmm.Dataflow.Graph are imported because of
-- import GHC.Cmm.Dataflow.Block  the above

---Needed for GenCmmGraph CmmNode

instance FromJSON Label where
    parseJSON v = mkHooplLabel <$> (parseJSON v :: Parser Word64)

deriving instance Generic SectionType
instance FromJSON SectionType


-- GenCmmStatics 'False: acepta CmmStatics y CmmStaticsRaw
instance FromJSON (GenCmmStatics 'False) where
  parseJSON = withObject "GenCmmStatics 'False" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      "CmmStatics" -> do
        (lbl, itbl, ccs, payload, nonPtrs) <-
          o .: "contents"
            :: Parser (CLabel, CmmInfoTable, CostCentreStack, [CmmLit], [CmmLit])
        pure (CmmStatics lbl itbl ccs payload nonPtrs)

      "CmmStaticsRaw" -> do
        (lbl, statics) <-
          o .: "contents"
            :: Parser (CLabel, [CmmStatic])
        pure (CmmStaticsRaw lbl statics)

      other -> fail $ "GenCmmStatics 'False: unknown tag " <> unpack other

-- CmmStatic aparece en CmmStaticsRaw :: CLabel -> [CmmStatic]
-- Necesaria para que genericParseJSON de GenCmmStatics 'True compile.
deriving instance Generic CmmStatic
instance FromJSON CmmStatic

-- Necessary for CmmStatic instance
instance FromJSON BS.ByteString where
    parseJSON = withText "ByteString" (pure . TE.encodeUtf8)


-- GenCmmStatics 'True: solo permite CmmStaticsRaw
instance FromJSON (GenCmmStatics 'True) where
  parseJSON = withObject "GenCmmStatics 'True" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      "CmmStaticsRaw" -> do
        (lbl, statics) <-
          o .: "contents"
            :: Parser (CLabel, [CmmStatic])
        pure (CmmStaticsRaw lbl statics)

      "CmmStatics" ->
        fail "GenCmmStatics 'True cannot be constructed with CmmStatics"

      other -> fail $ "GenCmmStatics 'True: unknown tag " <> unpack other

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
--this depends on GHC.Types.Var.Var, which seems to depend on a lot of GHC stuff
-- maybe i should not bother to serialize GHC.Types.Var.Var? 

instance FromJSON GHC.Types.Var.Var where
    parseJSON _ = fail "dummy"



deriving instance Generic GHC.Types.ForeignCall.ForeignCall
instance FromJSON GHC.Types.ForeignCall.ForeignCall

deriving instance Generic GHC.Types.ForeignCall.CCallSpec


instance FromJSON GHC.Types.ForeignCall.CCallSpec where
    parseJSON =
        error "Falla pues"

deriving instance Generic GHC.Types.ForeignCall.CCallTarget


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


instance FromJSON GHC.Types.FM.FastZString where
    parseJSON =
        error "falla pues"

-- deriving instance Generic Data.ByteString.Short.ShortByteString
instance FromJSON Data.ByteString.Short.ShortByteString


instance FromJSON ByteArray where
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
--is this used by something in a cmm module?

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