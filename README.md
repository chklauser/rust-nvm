Native Virtual Machine
======================

A toy virtual machine executing a stack-based byte code.

And since I'm too lazy to write byte code programs myself, I also included a toy compiler for a toy language.

The Toy Language
----------------
A simple procedural structured programming language. It only handles integers and integer arithmetic and comparison.

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
We currently use a virtual machine that uses

 * a stack for evaluating expressions (per stack frame)
 * a local variable store (per stack frame, accessed by index)

Additionally, each stack frame also has access to a parameter store that is supplied by the caller (`&mut` in Rust). When one toy routine calls another, it supplies it's own local variable store as the parameter store.

A compiler pass analyses each routine to allocate enough "call registers" at the beginning of the local variable store, so that routine calls do not interfere with actual local variables.

Known Issues
------------

 * Associativity in arithemtic expressions is not handled correctly (`5-2+1` results in 2 and not 4)
 * Couldn't figure out a way to allocate the evaluation stack and local variable store on Rust's stack. Both stack size and local variable store size are known and remain constant for each routine. I would like to do `[0, .. stack_size]` but Rust doesn't seem to support allocating arrays on the stack when their size is not known statically.
 