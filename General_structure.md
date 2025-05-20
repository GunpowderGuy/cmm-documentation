https://gitlab.haskell.org/ghc/ghc

Cmm is a polymorphic structure or set of related structures that share definitions ( the different variants have their own top level definitions but share innards ) .

Said Intermediate Representation / set of IRs is meant to be used as a backend from higher level languages and compile to machine code. Much like llvm would be

In practice Cmm is : 
Compiled from STG ( a GHC IR ) 
or
Parsed from a textual version of cmm ( which cant represent everyhting cmm can ? ) , which can optionally implement some high level featuers

And then directly compiled to machine code , to wasm via a route similar to the prior or to llvm.
or
Be pretty printed to a textual cmm version that is bafflingly different from the parseable one




Here's the concise list of key Cmm-related files in GHC:

    Parsing

        compiler/GHC/Cmm/Parser.y: Parses textual .cmm files.

The top level parsing function

parseCmmFile :: CmmParserConfig
             -> Module
             -> HomeUnit
             -> FilePath
             -> IO (Messages PsMessage, Messages PsMessage, Maybe (DCmmGroup, [InfoProvEnt]))
parseCmmFile cmmpConfig this_mod home_unit filename =

Takes textual cmm and outputs a form of serialized cmm : https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Cmm.hs

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
