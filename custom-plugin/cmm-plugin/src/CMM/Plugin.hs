{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module CMM.Plugin (plugin) where

import System.IO
import Data.IORef

import GHC.Plugins
import GHC.Driver.Hooks
import GHC.Driver.Pipeline as Pipeline
import GHC.Driver.Pipeline.Phases
import GHC.Driver.Pipeline.Execute
import GHC.Unit.Module.Status
import qualified GHC.StgToCmm as StgToCmm ( codeGen )
import GHC.StgToCmm.Config
import GHC.Types.IPE
import GHC.Stg.Syntax
import GHC.StgToCmm.Types (ModuleLFInfos)
import GHC.Types.CostCentre
import GHC.Types.HpcInfo
import qualified GHC.Data.Stream as Stream
import GHC.Data.Stream (Stream)
import GHC.Cmm
import GHC.Cmm.Info

import Control.Monad

import qualified Data.Map as Map

import CMM.CmmSerde 
import GHC.Cmm.Dataflow.Label (Label, mkHooplLabel)
import GHC.Types.Unique        (mkUniqueGrimily)


import Data.Aeson (toJSON, fromJSON,ToJSON, FromJSON, Value, Result(..), fromJSON, toJSON)


plugin :: Plugin
plugin = defaultPlugin
  { driverPlugin      = driverFun
  }

driverFun :: [CommandLineOption] -> HscEnv -> IO HscEnv
driverFun cmdOpts hscEnv = do
  putStrLn $ " ###### cmm-plugin driverFun cmdOpts: " ++ show cmdOpts
  let hooks = (hsc_hooks hscEnv)
                { runPhaseHook    = Just (PhaseHook runPhaseFun)
                }
  pure $ hscEnv {hsc_hooks = hooks}

{-
type instance DsForeignsHook = [LForeignDecl GhcTc] -> DsM (ForeignStubs, ForeignStubDecls, OrdList (Id, CoreExpr))

data Hooks
  = Hooks
  { dsForeignsHook         :: !(Maybe DsForeignsHook)
  , tcForeignImportsHook   :: !(Maybe ([LForeignDecl GhcRn] -> TcM ([Id], [LForeignDecl GhcTc], Bag GlobalRdrElt)))
  , tcForeignExportsHook   :: !(Maybe ([LForeignDecl GhcRn] -> TcM (LHsBinds GhcTc, [LForeignDecl GhcTc], Bag GlobalRdrElt)))
  , hscFrontendHook        :: !(Maybe (ModSummary -> Hsc FrontendResult))
  , hscCompileCoreExprHook :: !(Maybe (HscEnv -> SrcSpan -> CoreExpr -> IO (ForeignHValue, [Linkable], PkgsLoaded)))
  , ghcPrimIfaceHook       :: !(Maybe ModIface)
  , runPhaseHook           :: !(Maybe PhaseHook)
  , runMetaHook            :: !(Maybe (MetaHook TcM))
  , linkHook               :: !(Maybe (GhcLink -> DynFlags -> Bool -> HomePackageTable -> IO SuccessFlag))
  , runRnSpliceHook        :: !(Maybe (HsUntypedSplice GhcRn -> RnM (HsUntypedSplice GhcRn)))
  , getValueSafelyHook     :: !(Maybe (HscEnv -> Name -> Type -> IO (Either Type (HValue, [Linkable], PkgsLoaded))))
  , createIservProcessHook :: !(Maybe (CreateProcess -> IO ProcessHandle))
  , stgToCmmHook           :: !(Maybe (StgToCmmConfig -> InfoTableProvMap -> [TyCon] -> CollectedCCs -> [CgStgTopBinding] -> HpcInfo -> Stream IO CmmGroup ModuleLFInfos))
  , cmmToRawCmmHook        :: !(forall a . Maybe (DynFlags -> Maybe Module -> Stream IO CmmGroupSRTs a -> IO (Stream IO RawCmmGroup a)))
  }
-}
{-
data TPhase res where
  T_Unlit :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_FileArgs :: HscEnv -> FilePath -> TPhase (DynFlags, Messages PsMessage, [Warn])
  T_Cpp   :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_HsPp  :: PipeEnv -> HscEnv -> FilePath -> FilePath -> TPhase FilePath
  T_HscRecomp :: PipeEnv -> HscEnv -> FilePath -> HscSource -> TPhase (HscEnv, ModSummary, HscRecompStatus)
  T_Hsc :: HscEnv -> ModSummary -> TPhase (FrontendResult, Messages GhcMessage)
  T_HscPostTc :: HscEnv -> ModSummary
              -> FrontendResult
              -> Messages GhcMessage
              -> Maybe Fingerprint
              -> TPhase HscBackendAction
  T_HscBackend :: PipeEnv -> HscEnv -> ModuleName -> HscSource -> ModLocation -> HscBackendAction -> TPhase ([(String, FilePath)], [FilePath], ModIface, Maybe Linkable, FilePath)
  T_CmmCpp :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_Cmm :: PipeEnv -> HscEnv -> FilePath -> TPhase ([FilePath], FilePath)
  T_Cc :: Phase -> PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_As :: Bool -> PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> TPhase FilePath
  T_LlvmOpt :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_LlvmLlc :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_LlvmMangle :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_MergeForeign :: PipeEnv -> HscEnv -> FilePath -> [(String, FilePath)] -> [FilePath] -> TPhase FilePath
-}

runPhaseFun :: forall a . TPhase a -> IO a
runPhaseFun phase = do
  let phaseStr = case phase of
        T_Unlit{}         -> "T_Unlit"
        T_FileArgs{}      -> "T_FileArgs"
        T_Cpp{}           -> "T_Cpp"
        T_HsPp{}          -> "T_HsPp"
        T_HscRecomp{}     -> "T_HscRecomp"
        T_Hsc{}           -> "T_Hsc"
        T_HscPostTc{}     -> "T_HscPostTc"
        T_HscBackend{}    -> "T_HscBackend"
        T_CmmCpp{}        -> "T_CmmCpp"
        T_Cmm{}           -> "T_Cmm"
        T_Cc{}            -> "T_Cc"
        T_As{}            -> "T_As"
        T_Js{}            -> "T_Js"
        T_ForeignJs{}     -> "T_ForeignJs"
        T_LlvmOpt{}       -> "T_LlvmOpt"
        T_LlvmLlc{}       -> "T_LlvmLlc"
        T_LlvmAs{}        -> "T_LlvmAs"
        T_LlvmMangle{}    -> "T_LlvmMangle"
        T_MergeForeign{}  -> "T_MergeForeign"

  putStrLn $ " ###### cmm-plugin runPhaseFun phase: " ++ phaseStr
  --undefined

  case phase of
    T_HscBackend pipeEnv hscEnv modName hscSource modLocation action@HscRecomp{..} -> do
      -- HINT: setup stgToCmmHook to capture stg binds
      let hooks           = (hsc_hooks hscEnv)
                              { stgToCmmHook    = Just (stgToCmmFun hscEnvWithHooks)
                              , cmmToRawCmmHook = Just (cmmToRawCmmFun hscEnv)
                              }
          hscEnvWithHooks = hscEnv {hsc_hooks = hooks}
      runPhase (T_HscBackend pipeEnv hscEnvWithHooks modName hscSource modLocation action)

    _ -> runPhase phase

{-
stgToCmmFun :: HscEnv -> StgToCmmConfig -> InfoTableProvMap -> [TyCon] -> CollectedCCs -> [CgStgTopBinding] -> HpcInfo -> Stream IO CmmGroup ModuleLFInfos
stgToCmmFun hscEnv cfg itpm tcList ccc stgBinds hpcInfo = do
  liftIO $ do
    putStrLn $ " ###### run stgToCmmFun"
  StgToCmm.codeGen (hsc_logger hscEnv) (hsc_tmpfs hscEnv) cfg itpm tcList ccc stgBinds hpcInfo
-}
stgToCmmFun :: HscEnv
            -> StgToCmmConfig
            -> InfoTableProvMap
            -> [TyCon]
            -> CollectedCCs
            -> [CgStgTopBinding]
            -> HpcInfo
            -> Stream IO CmmGroup ModuleLFInfos
stgToCmmFun hscEnv cfg itpm tcList ccc stgBinds hpcInfo = do
  liftIO $ do
    putStrLn " ###### run stgToCmmFun"
    --modifyIORef globalEnvIORef $ \d -> d { geStgBinds = Just stgBinds }

  let stream =
        StgToCmm.codeGen
          (hsc_logger hscEnv)
          (hsc_tmpfs hscEnv)
          cfg itpm tcList ccc stgBinds hpcInfo

      serde :: forall a. (Data.Aeson.ToJSON a, Data.Aeson.FromJSON a) => a -> IO a
      serde parameter = do
        let v = Data.Aeson.toJSON parameter
        liftIO (print v)
        liftIO (print "dubear33")
        case Data.Aeson.fromJSON v of
          Data.Aeson.Success x -> pure x
          Data.Aeson.Error msg -> fail ("serde roundtrip failedd: " ++ msg)
          --Data.Aeson.Error msg -> pure parameter 
          --if you edit this out, when parsing fails, the original cmm variable will be used instead of failing the compilation

  Stream.mapM serde stream


cmmToRawCmmFun :: HscEnv -> DynFlags -> Maybe Module -> Stream IO CmmGroupSRTs a -> IO (Stream IO RawCmmGroup a)
cmmToRawCmmFun hscEnv dflags mMod cmms = do
  let logger    = hsc_logger hscEnv
      profile   = targetProfile dflags
      platform  = targetPlatform dflags
  rawcmms0 <- cmmToRawCmm logger profile cmms

  -- name pretty printer setup
  let qualifyImportedNames mod _
        | Just mod == mMod  = NameUnqual
        | otherwise         = NameNotInScope1
      print_unqual = QueryQualify qualifyImportedNames
                                  neverQualifyModules
                                  neverQualifyPackages
                                  alwaysPrintPromTick
      dumpStyle = mkDumpStyle print_unqual

  let dump a = do
        let cmmDoc = vcat $ map (\i -> pdoc platform i $$ blankLine) a
        --hPutStr cmmHandle . showSDoc dflags $ withPprStyle dumpStyle cmmDoc
        pure a
  pure $ Stream.mapM dump rawcmms0
