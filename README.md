# cmm-documentation
GSoC project to documment and improve the tooling for cmm ( GHC )


Related : 
    Core Syntax and Types

        Primary Module: GHC/Core.hs
        Defines the Core language's AST (Expr, Bind, etc.) and utilities.

    Core-to-Core Passes

        Directory: compiler/GHC/Core/Opt
        Optimizations like inlining, float-out, and specialization are still here.

    Type and Coercion Handling

        Coercions: GHC/Core/Coercion.hs

        Type Utilities: GHC/Core/Type.hs

    Core Linting

        File: GHC/Core/Lint.hs

Cmm : 


C-- (often abbreviated as **Cmm** in GHC) is a low-level, platform-independent intermediate representation (IR) used in the later stages of the GHC compilation pipeline. It is **not a polymorphic structure** in the type-theoretic sense, but rather a **monomorphic, structured representation** designed to model imperative code with explicit control flow, memory operations, and platform-agnostic primitives. However, the Cmm IR is **flexible enough to be processed into multiple backend targets** (e.g., native machine code, LLVM IR, WebAssembly). Let’s break this down:

---

### **Cmm Structure**
Cmm is a **single unified structure** defined in GHC’s source code, but it can represent code in a way that abstracts over backend-specific details. Its key components include:

1. **Control Flow Graphs (CFGs)**  
   Cmm organizes code into basic blocks with explicit jumps/branches, allowing for machine-like control flow. For example:
   ```cmm
   section "data" { ... }
   section "text" { ... }
   ```

2. **Primitive Operations**  
   Cmm includes operations for arithmetic, memory access (loads/stores), and calls to runtime system primitives (e.g., garbage collection hooks).

3. **Platform-Independent Constructs**  
   - **Registers**: Virtual registers (e.g., `R1`, `D2`) that map to physical registers or stack slots later.
   - **Stack Frames**: Explicit management of stack frames for function calls.
   - **Global Data**: Static data blocks (strings, tables, etc.).

4. **Annotations for Backends**  
   Cmm includes metadata (e.g., alignment hints, calling conventions) to guide backend code generators.

---

### **How Cmm is Generated**
Cmm is produced from two sources:
1. **STG-to-Cmm Compilation**  
   The STG IR (a higher-level, lazy functional IR) is lowered to Cmm via the [`GHC.StgToCmm`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/StgToCmm.hs) module. This step handles:
   - Heap allocation (thunks, closures).
   - Lazy evaluation (entering thunks, updating them).
   - Tail calls and garbage collection points.

2. **Textual Cmm Input**  
   GHC allows developers to write Cmm directly (e.g., in `.cmm` files). This is used for low-level runtime system code (e.g., primops, garbage collector). The parser for textual Cmm is in [`GHC.Cmm.Parser`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Cmm/Parser.y).

---

### **How Cmm is Processed**
Cmm undergoes several transformations before being passed to a backend:

1. **Cmm Optimizations**  
   - **Sinking**: Moving computations closer to their uses.
   - **Spilling**: Deciding which virtual registers to spill to the stack.
   - **Dead Code Elimination**: Removing unreachable blocks.

2. **Platform-Specific Lowering**  
   The same Cmm IR is processed differently depending on the target backend:
   - **Native Code Generator (NCG)**: Converts Cmm directly to machine code (x86, ARM, etc.).
   - **LLVM Backend**: Converts Cmm to LLVM IR (via [`GHC.CmmToLlvm`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/CmmToLlvm.hs)).
   - **WebAssembly (WASM)**: Experimental support converts Cmm to WASM via a similar lowering process.
   - **C Backend**: Older backend (now deprecated) that generated C code.

---

### **Key Modules in GHC**
- **Cmm Syntax**: Defined in [`GHC.Cmm.Syntax`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Cmm/Syntax.hs) (AST for Cmm nodes like `CmmGraph`, `CmmBlock`).
- **Cmm Pipeline**: [`GHC.Cmm.Pipeline`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Cmm/Pipeline.hs) coordinates optimization passes.
- **Backends**:
  - [`GHC.CmmToAsm`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/CmmToAsm.hs) (Native code generator).
  - [`GHC.CmmToLlvm`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/CmmToLlvm.hs) (LLVM backend).
  - Experimental WASM backend (not yet upstreamed, but under active development).

---

### **Why Cmm is Not Polymorphic**
- **Monomorphic AST**: The Cmm IR has a fixed structure (e.g., `CmmLoad`, `CmmStore`, `CmmCall` nodes) that is not polymorphic in the type-system sense.
- **Backend Polymorphism**: The *processing* of Cmm is polymorphic—the same IR can be lowered to different targets via backend-specific logic. For example:
  ```haskell
  -- Simplified pseudo-code for backend dispatch
  compileCmm :: Backend -> Cmm -> Either Error MachineCode
  compileCmm NCG = cmmToAsm
  compileCmm LLVM = cmmToLlvm
  compileCmm WASM = cmmToWasm
  ```

---

### **Example: Cmm → LLVM IR**
A Cmm load operation:
```cmm
  R1 = I32[Sp + 8];
```
Might lower to LLVM IR as:
```llvm
  %1 = getelementptr i8, ptr %Sp, i32 8
  %2 = load i32, ptr %1
  store i32 %2, ptr @R1
```

---

### **Summary**
- **Cmm is a single, structured IR** with a fixed AST (not polymorphic).
- **Backends are polymorphic**: The same Cmm can be compiled to machine code, LLVM IR, or WebAssembly via backend-specific logic.
- **STG and textual Cmm** are the two primary sources for Cmm.

For more details, explore the [`compiler/GHC/Cmm`](https://gitlab.haskell.org/ghc/ghc/-/tree/master/compiler/GHC/Cmm) directory in the GHC source.
