The following is an example cmm procedure in parseable form. The parser transforms it into the internal cmm AST

```c
bar
{
  // this is low-level cmm code, indicated by the fact that we did not
  // put an argument list on bar.

 // bits64 x;
 // x = R1;  // the calling convention is explicit: better be careful
           // that this works on all platforms!

  jump %ENTRY_CODE(Sp(0))[];
  //jump %ENTRY_CODE(Sp(0))[] note this is the version found in
  // documentation, notice the two errors : https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Cmm/Parser.y
}
``

We will analyze how low level calls ( as seen in the example above, called jumps in the parseable representation ) 
get represented in the parser, AST and pretty printer
 
-How call gets dealt with by the parser

/ghc-9.10.1/complier/GHC/Cnmp/Parser.v.source

704    'return' '(' exprs0 ')' ';'
705          { doReturn $3 }
706    | 'jump' expr vols ';'
707          { doRawJump $2 $3 }
708    | 'jump' expr '(' exprs0 ')' ';'
709          { doJumpWithStack $2 [] $4 }

ghc-9.10.1/compiler/GHC/Cmm/Parser_v.source

1377 doRawJump :: CmmParse CmmExpr -> [GlobalReg] -> CmmParse (
1378 doRawJump expr_code vols = do
1379     profile <- getProfile
1380     expr <- expr_code
1381     updfr_off <- getUpdFrameOff
1382     emit (mkRawJump profile expr updfr_off vols)


ghc-9.10.1/compiler/GHC/Cmm/Graph.hs 

211 mkRawJump :: Profile -> CmmExpr -> UpdFrameOffset -> [GlobalReg]
212              -> CmmAGraph
213 mkRawJump profile e updfr_off vols =
214     lastWithArgs profile Jump Old NativeNodeCall [] updfr_off $
215     \arg_space _ -> tccall e Nothing updfr_off 0 arg_space vols
216


ghc-9.10.1/compiler/GHC/Cmm/Graph.hs 

492 toCall :: CmmExpr -> Maybe BlockId -> UpdFrameOffset -> ByteOff
493       -> ByteOff -> [GlobalReg]
494       -> CmmAGraph
495 toCall e cont updfr_off res_space arg_space regs =
496    mklast $ CmmCall e cont regs arg_space res_space updfr_off
497


"
Csaba
call is for FFI in the parser AFAIK

GunpowderGuy — 29/07/2025, 16:36
i think if found a syntax for "call" not related to ffi, give me a second
"
-In response to this, Csaba found the definition for high level cmm call. It seems 
its implemented entirely outside the cmm data structure

The code above is how the parser deals with low level cmm calls. The way that 
most closely corresponds to calls in the Cmm AST. Even then , you can see the 
parser infers some information instead of exposing everything the AST has to offer.

The parser also has a high level syntax. This deviates from the AST even more 
and offers features closer to C. Here is how high level calls get implemented

ghc-9.10.1/compiler/GHC/Cmm/Parser.v.source

712    | 'call' expr '(' exprso ')' ';'
713          { docall $2 [] $4 }
714    | '(' formats ')' '=' 'call' expr '(' exprso ')' ';'
715          { docall $6 $2 $8 }


ghc-9.10.1/compiler/GHC/Cmm/Parser_v.source

1394 doCall :: CmmParse CmmExpr -> [CmmParse LocalReg] -> [CmmParse CmmExpr]
1395          -> CmmParse ()
1396 doCall expr_code res_code args_code = do
1397     expr <- expr_code
1398     args <- sequence args_code
1399     ress <- sequence res_code
1400     updfr_off <- getUpdFrameOff
1401     c <- code $ mkCall expr (NativeNodeCall,NativeReturn) ress args updfr_off []
1402     emit c


ghc-9.10.1/compiler/GHC/StgToCmm/Monad.hs 

859 mkCall :: CmmExpr -> (Convention, Convention) -> [CmmFormal] -> [CmmExpr]
860          -> UpdFrameOffset -> [CmmExpr] -> FCode CmmAGraph
861 mkCall f (callConv, retConv) results actuals updfr_off extra_stack = do
862     profile <- getProfile
863     k <- newBlockId
864     tscp <- getTickScope
865     let area = Young k
866         (off, _, copyin) = copyInOflow profile retConv area results []
867         copyout = mkCallReturnsTo profile f callConv actuals k off updfr_off extra_stack
868     return $ catAGraphs [copyout, mklabel k tscp, copyin]



ghc-9.10.1/compiler/GHC/CmmGraph.hs 

248 mkCallReturnsTo :: Profile -> CmmExpr -> Convention -> [CmmExpr]
249       -> BlockId
250       -> ByteOff
251       -> UpdFrameOffset
252       -> [CmmExpr]
253       -> CmmAGraph
254 mkCallReturnsTo profile f callConv actuals ret_lbl ret_off updfr_off extra_stack =
255   lastWithArgsAndExtraStack profile Call (Young ret_lbl) callConv actuals
256     updfr_off extra_stack $
257       toCall f (Just ret_lbl) updfr_off ret_off
258



ghc-9.10.1/compiler/GHC/CmmGraph.hs :

492 toCall :: CmmExpr -> Maybe BlockId -> UpdFrameOffset -> ByteOff
493       -> ByteOff -> [GlobalReg]
494       -> CmmAGraph
495 toCall e cont updfr_off res_space arg_space regs =
496    mkLast $ CmmCall e cont regs arg_space res_space updfr_off




"so part of high level syntax? like call with arguments or returns is considered high level? "
The following is an explanation taken from comments in GHC source code

ghc-9.10.1/compiler/GHC/Cnm/Parser.y.source [B---] 0 L:  
71 High-level only:  
72  
73 - tail-calls with arguments, e.g.  
74   jump stg_fun (arg1, arg2);  
75  
76 - function calls:  
77   (ret1,ret2) = call stg_fun (arg1, arg2);  
78  
79   This makes a call with the NativeNodeCall convention, and the values are returned  
80   to the following code using the NativeReturn convention.  
81  
82  
83 - returning:  
84   return (ret1, ret2)  
85  
86   These use the NativeReturn convention to return zero or more results to the caller.  
87  
88  
89 - pushing stack frames:  
90   push (info_ptr, field1, ..., fieldN) { ... statements ... }  
91  
92 - reserving temporary stack space:  
93  
94   reserve N = x { ... }  
95  
96   this reserves an area of size N (words) on the top of the stack, and binds its  
97   address to x (a local register). Typically this is used for allocating temporary  
98   storage for passing to foreign functions.  
99  
100  Note that if you make any native calls or invoke the GC in the scope of the reserve  
101  block, you are responsible for ensuring that the stack you reserved is laid out  
102  correctly with an info table.  


Using grep for figuring out the pretty printer:
We can also see how calls get represented in the pretty printer. Lets start by searching all
the ocurrences of files under Cmm folder , referring to SDoc ( the pretty printing library ) 


grep -rnw "SDoc"
CLabel.hs:313:{-# SPECIALIZE pprModuleLabelKind :: ModuleLabelKind -> SDoc #-}
CLabel.hs:467:pprDebugCLabel :: Platform -> CLabel -> SDoc
CLabel.hs:1446:{-# SPECIALIZE pprAsmLabel :: Platform -> CLabel -> SDoc #-}
CLabel.hs:1451:{-# SPECIALIZE pprCLabel :: Platform -> CLabel -> SDoc #-}
CLabel.hs:1602:{-# SPECIALIZE pprCLabelStyle :: Platform -> LabelStyle -> CLabel -> SDoc #-}
CLabel.hs:1638:{-# SPECIALIZE ppInternalProcLabel :: Module -> CLabel -> Maybe SDoc #-}
DebugBlock.hs:557:{-# SPECIALIZE pprUnwindExpr :: Rational -> Platform -> UnwindExpr -> SDoc #-}
Expr.hs:102:pprArea :: Area -> SDoc
Expr.hs:428:pprExpr :: Platform -> CmmExpr -> SDoc
Expr.hs:452:pprExpr1, pprExpr7, pprExpr8 :: Platform -> CmmExpr -> SDoc
Expr.hs:458:infixMachOp1, infixMachOp7, infixMachOp8 :: MachOp -> Maybe SDoc
Expr.hs:493:pprExpr9 :: Platform -> CmmExpr -> SDoc
Expr.hs:508:genMachOp :: Platform -> MachOp -> [CmmExpr] -> SDoc
Expr.hs:537:infixMachOp :: MachOp -> Maybe SDoc
Expr.hs:554:pprLit :: Platform -> CmmLit -> SDoc
Expr.hs:570:pprLit1 :: Platform -> CmmLit -> SDoc
Expr.hs:574:ppr_offset :: Int -> SDoc
Expr.hs:580:commafy :: [SDoc] -> SDoc
Info/Build.hs:1328:srtTrace :: String -> SDoc -> b -> b
Info/Build.hs:1332:srtTraceM :: Applicative f => String -> SDoc -> f ()
Lint.hs:43:        => Platform -> GenCmmGroup d h CmmGraph -> Maybe SDoc
Lint.hs:46:cmmLintGraph :: Platform -> CmmGraph -> Maybe SDoc
Lint.hs:49:runCmmLint :: OutputableP Platform a => Platform -> (a -> CmmLint b) -> a -> Maybe SDoc
Lint.hs:249:                             => SDoc -> a -> CmmLint ()
Lint.hs:269:newtype CmmLint a = CmmLint { unCL :: Platform -> Either SDoc a }
Lint.hs:271:  deriving (Applicative, Monad) via ReaderT Platform (Except SDoc)
Lint.hs:276:cmmLintErr :: SDoc -> CmmLint a
Lint.hs:279:addLintInfo :: SDoc -> CmmLint a -> CmmLint a
MachOp.hs:207:pprMachOp :: MachOp -> SDoc
MachOp.hs:825:pprCallishMachOp :: CallishMachOp -> SDoc
Node.hs:124:pprNode :: Platform -> CmmNode e x -> SDoc
Node.hs:127:    pp_node :: SDoc
Node.hs:237:    pp_debug :: SDoc
Node.hs:254:    commafy :: [SDoc] -> SDoc
Node.hs:269:pprBlock :: IndexedCO x SDoc SDoc ~ SDoc
Node.hs:270:         => Platform -> Block CmmNode e x -> IndexedCO e SDoc SDoc
Node.hs:279:pprGraph :: Platform -> Graph CmmNode e x -> SDoc
Node.hs:288:                      => MaybeO ex (Block CmmNode e x) -> SDoc
Node.hs:421:pprForeignConvention :: ForeignConvention -> SDoc
Node.hs:433:pprReturnInfo :: CmmReturnInfo -> SDoc
Node.hs:448:pprForeignTarget :: Platform -> ForeignTarget -> SDoc
Node.hs:452:    ppr_target :: CmmExpr -> SDoc
Node.hs:466:pprConvention :: Convention -> SDoc
Pipeline.hs:373:dumpWith :: Logger -> DumpFlag -> String -> DumpFormat -> SDoc -> IO ()
Reg.hs:98:pprReg :: CmmReg -> SDoc
Reg.hs:143:pprLocalReg :: LocalReg -> SDoc
Reg.hs:296:{-# SPECIALIZE pprGlobalReg :: GlobalReg -> SDoc #-}
diego@fedora:~/Documentos/ghc/compiler/GHC/Cmm$ 

The following is the definition of calls in the cmm AST
grep -rnw --include="Node.hs" "CmmCall"

  CmmCall :: {                -- A native call or tail call
      cml_target :: CmmExpr,  -- never a CmmPrim to a CallishMachOp!

      cml_cont :: Maybe Label,
          -- Label of continuation (Nothing for return or tail call)
          --
          -- Note [Continuation BlockIds]
          -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          -- These BlockIds are called
          -- Continuation BlockIds, and are the only BlockIds that can
          -- occur in CmmExprs, namely as (CmmLit (CmmBlock b)) or
          -- (CmmStackSlot (Young b) _).

      cml_args_regs :: [GlobalRegUse],
          -- The argument GlobalRegs (Rx, Fx, Dx, Lx) that are passed
          -- to the call.  This is essential information for the
          -- native code generator's register allocator; without
          -- knowing which GlobalRegs are live it has to assume that
          -- they are all live.  This list should only include
          -- GlobalRegs that are mapped to real machine registers on
          -- the target platform.

      cml_args :: ByteOff,
          -- Byte offset, from the *old* end of the Area associated with
          -- the Label (if cml_cont = Nothing, then Old area), of
          -- youngest outgoing arg.  Set the stack pointer to this before
          -- transferring control.
          -- (NB: an update frame might also have been stored in the Old
          --      area, but it'll be in an older part than the args.)

      cml_ret_args :: ByteOff,
          -- For calls *only*, the byte offset for youngest returned value
          -- This is really needed at the *return* point rather than here
          -- at the call, but in practice it's convenient to record it here.

      cml_ret_off :: ByteOff
        -- For calls *only*, the byte offset of the base of the frame that
        -- must be described by the info table for the return point.
        -- The older words are an update frames, which have their own
        -- info-table and layout information

        -- From a liveness point of view, the stack words older than
        -- cml_ret_off are treated as live, even if the sequel of
        -- the call goes into a loop.
  } -> CmmNode O C

-- Haskell record syntax. Code found in Cmm/Node.hs line 108
pprNode :: Platform -> CmmNode e x -> SDoc
pprNode platform node = pp_node <+> pp_debug


prNode :: Platform -> CmmNode e x -> SDoc
pprNode platform node = pp_node <+> pp_debug
  where
    pp_node :: SDoc
    pp_node = case node of
      -- label:
      CmmEntry id tscope ->
         (sdocOption sdocSuppressUniques $ \case
            True  -> text "_lbl_"
            False -> ppr id
         )
         <> colon
         <+> ppUnlessOption sdocSuppressTicks (text "//" <+> ppr tscope)

      -- // text
      CmmComment s -> text "//" <+> ftext s

      -- //tick bla<...>
      CmmTick t -> ppUnlessOption sdocSuppressTicks
                     (text "//tick" <+> ppr t)

      -- unwind reg = expr;
      CmmUnwind regs ->
          text "unwind "
          <> commafy (map (\(r,e) -> ppr r <+> char '=' <+> pdoc platform e) regs) <> semi

      -- reg = expr;
      CmmAssign reg expr -> ppr reg <+> equals <+> pdoc platform expr <> semi

      -- rep[lv] = expr;
      CmmStore lv expr align -> rep <> align_mark <> brackets (pdoc platform lv) <+> equals <+> pdoc platform expr <> semi
          where
            align_mark = case align of
                           Unaligned -> text "^"
                           NaturallyAligned -> empty
            rep = ppr ( cmmExprType platform expr )

      -- call "ccall" foo(x, y)[r1, r2];
      -- ToDo ppr volatile
      CmmUnsafeForeignCall target results args ->
          hsep [ ppUnless (null results) $
                    parens (commafy $ map ppr results) <+> equals,
                 text "call",
                 pdoc platform target <> parens (commafy $ map (pdoc platform) args) <> semi]

      -- goto label;
      CmmBranch ident -> text "goto" <+> ppr ident <> semi

      -- if (expr) goto t; else goto f;
      CmmCondBranch expr t f l ->
          hsep [ text "if"
               , parens (pdoc platform expr)
               , case l of
                   Nothing -> empty
                   Just b -> parens (text "likely:" <+> ppr b)
               , text "goto"
               , ppr t <> semi
               , text "else goto"
               , ppr f <> semi
               ]

      CmmSwitch expr ids ->
          hang (hsep [ text "switch"
                     , range
                     , if isTrivialCmmExpr expr
                       then pdoc platform expr
                       else parens (pdoc platform expr)
                     , text "{"
                     ])
             4 (vcat (map ppCase cases) $$ def) $$ rbrace
          where
            (cases, mbdef) = switchTargetsFallThrough ids
            ppCase (is,l) = hsep
                            [ text "case"
                            , commafy $ toList $ fmap integer is
                            , text ": goto"
                            , ppr l <> semi
                            ]
            def | Just l <- mbdef = hsep
                            [ text "default:"
                            , braces (text "goto" <+> ppr l <> semi)
                            ]
                | otherwise = empty

            range = brackets $ hsep [integer lo, text "..", integer hi]
              where (lo,hi) = switchTargetsRange ids

      CmmCall tgt k regs out res updfr_off ->
          hcat [ text "call", space
               , pprFun tgt, parens (interpp'SP regs), space
               , returns <+>
                 text "args: " <> ppr out <> comma <+>
                 text "res: " <> ppr res <> comma <+>
                 text "upd: " <> ppr updfr_off
               , semi ]
          where pprFun f@(CmmLit _) = pdoc platform f
                pprFun f = parens (pdoc platform f)

                returns
                  | Just r <- k = text "returns to" <+> ppr r <> comma
                  | otherwise   = empty

      CmmForeignCall {tgt=t, res=rs, args=as, succ=s, ret_args=a, ret_off=u, intrbl=i} ->
          hcat $ if i then [text "interruptible", space] else [] ++
               [ text "foreign call", space
               , pdoc platform t, text "(...)", space
               , text "returns to" <+> ppr s
                    <+> text "args:" <+> parens (pdoc platform as)
                    <+> text "ress:" <+> parens (ppr rs)
               , text "ret_args:" <+> ppr a
               , text "ret_off:" <+> ppr u
               , semi ]

    pp_debug :: SDoc
    pp_debug =
      if not debugIsOn then empty
      else case node of
             CmmEntry {}             -> empty -- Looks terrible with text "  // CmmEntry"
             CmmComment {}           -> empty -- Looks also terrible with text "  // CmmComment"
             CmmTick {}              -> empty
             CmmUnwind {}            -> text "  // CmmUnwind"
             CmmAssign {}            -> text "  // CmmAssign"
             CmmStore {}             -> text "  // CmmStore"
             CmmUnsafeForeignCall {} -> text "  // CmmUnsafeForeignCall"
             CmmBranch {}            -> text "  // CmmBranch"
             CmmCondBranch {}        -> text "  // CmmCondBranch"
             CmmSwitch {}            -> text "  // CmmSwitch"
             CmmCall {}              -> text "  // CmmCall"
             CmmForeignCall {}       -> text "  // CmmForeignCall"

    commafy :: [SDoc] -> SDoc
    commafy xs = hsep $ punctuate comma xs


