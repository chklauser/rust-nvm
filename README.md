Native Virtual Machine
======================

A toy virtual machine executing a register-based "byte code".

And since I'm too lazy to write byte code programs myself, I also included a toy compiler for a toy language.

The Toy Language
----------------
A simple procedural structured programming language. It only handles integers, integer arithmetic and comparison.

Routines have a number of named parameters. Any name used in a routine, that is not a parameter is assumed to be the name of a local variable. Arguments can be passed by value (default) or by reference (`ref`).

Routines can be called via the dedicated `call` statement or with a more familiar "function" syntax. If you use the latter, then the routine's first parameter is assumed to be a by-ref out-parameter that supplies the return value.

```
routine main(result, x, y) {
  call max(ref a, x, y);
  a <- max(x,y); // the same; first parameter implicitly used as return value
  b <- min(x,y);
  while b < a {
    call increment(ref result, ref b)
  }
}

routine increment(v,w) {
  v <- v + 1;
  w <- w + 1
}

routine min(result, x, y) {
  m1 <- 0-1;
  result <- m1*max(m1*x,m1*y)
}

routine max(result, x,y) {
  if x < y {
    result <- y
  } else {
    result <- x
  }
}
```

Execution Engine
----------------
The virtual machine uses one local variable store per stack frame, accessed by an integer index. 

Additionally, each stack frame also has access to a parameter store that is supplied by the caller (`&mut` in Rust). When one toy routine calls another, it supplies it's own local variable store as the parameter store of the callee. That way, an `n`-routines deep call stack requires only `n+1` variable/parameter stores.

A compiler pass analyses each routine to allocate enough "call and evaluation registers" at the beginning of the local variable store, so that routine calls do not interfere with actual local variables.

Instructions operate on local variables ("registers") and one of the operands is used as the destination for instruction output (similar to x86). "Two-Address-Code" if you want.

Compiler
--------
The compiler for the toy language is very basic. In addition to slots for ordinary local variables, it allocates a number of slots for both call arguments as well as "registers" to use for evaluating expressions.

The register allocation algorithm is extremely primitive. It simulates an evaluation stack in the lower part of the local variable store and treats actual local variables as a separate namespace.

### Example
```
routine main(a) {
  v <- 7;
  call max(ref a, 15, v + 3)
}
```

will be translated into something similar to the following

```
// routine main(a)
// parameter store
  0: a
// register store
  0: (stack/call param)
  1: (stack/call param)
  2: (stack/call param)
  3: (stack)
  4: v
// instructions
  // v <- 7
  00: LIT r0 <- "7"
  01: MOV r4 <- r0
  // call main(...)
  02: LDPARAM r0 <- p0
  03: LIT r1 <- "15"
  04: MOV r2 <- r4
  05: LIT r3 <- "3"
  06: ADD r2 += r3
  07: CALL "max"(r0..)
  08: STPARAM p0 <- r0
```

Known Issues
------------

 * Associativity in arithemtic expressions is not handled correctly (`5-2+1` results in 2 and not 4)
 * Couldn't figure out a way to allocate the evaluation stack and local variable store on Rust's stack. The local variable store size is known and remains constant for each routine. I would like to do `[0, .. store_size]` but Rust doesn't seem to support allocating arrays on the stack when their size is not known statically.
 * Register allocation and code generation is very, very inefficient

History
-------

### 2014-10-29 Drop stack machine in favour of register machine
While the stack-based representation (instruction) is much smaller (no need to specify input output locations), it requires us to have both a local variable store *and* an evaluation stack. This means two separate allocations per call. Currently both the local variable store and the evaluation stack need to be allocated on the heap, because Rust has no mechanism for allocating dynamically sized arrays on the stack (`alloca`).

For call-heavy programs, the register-based machine completes in about 60% of the time that the stack-based machine takes. The number of instructions involved is about the same since the compiler just uses the lower region of the local variable store to simulate an evaluation stack.

Changing the stack-based machine in such a way that it allocates one big store for both registers (at the low end) and the stack (at the high end) reduces the speed difference to about 75-80%. That code lives in the `stack_machine` branch.
