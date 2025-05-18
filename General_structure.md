https://gitlab.haskell.org/ghc/ghc

Here's the concise list of key Cmm-related files in GHC:

    Parsing

        compiler/GHC/Cmm/Parser.y: Parses textual .cmm files.

    STG → Cmm

        compiler/GHC/StgToCmm.hs: Main driver for STG-to-Cmm translation.

        compiler/GHC/StgToCmm/Expr.hs: Translates STG expressions to Cmm.

    Cmm Optimizations

        compiler/GHC/Cmm/Pipeline.hs: Coordinates Cmm optimization passes.

    Backends

        Native Code: compiler/GHC/CmmToAsm.hs

        LLVM: compiler/GHC/CmmToLlvm.hs

        ## WASM: (Experimental, external repos e.g., ghc-wasm-meta).

        https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/CmmToAsm/Wasm.hs

        https://gitlab.haskell.org/ghc/ghc/-/tree/master/compiler/GHC/CmmToAsm/Wasm
    Cmm Syntax

        ## compiler/GHC/Cmm/Syntax.hs: Core Cmm AST definitions.

        https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Cmm.hs

    Utilities

       compiler/GHC/Cmm/Node.hs: Cmm node analysis/manipulation.

       compiler/GHC/Cmm/Lint.hs: Validates Cmm invariants.
