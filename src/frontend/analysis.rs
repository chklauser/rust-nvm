
use super::super::vm::bytecode::*;

#[deriving(Show,Eq,PartialEq,Clone)]
pub type AnalysisResult<T> = Result<T,AnalysisError>;

#[deriving(Show,Eq,PartialEq,Clone)]
pub enum AnalysisError {
  NonConstantStackSize(uint,uint,uint), // instruction, size1, size2
  StaticStackUnderflow(uint), //address
  IllegalControlTransfer(uint,uint) // from addr, to addr
}

// Use abstract interpretation to compute the maximum stack size and make sure
// that stack sizes are constant.
pub fn compute_max_stack_size(routine: &mut Routine) -> AnalysisResult<()> {
  let mut stack_size = Vec::from_elem(routine.instructions.len()+1, None); // indicates size-before
                                                                           // size-as-the-instruction-is-reached
  let mut int_stack = vec![(0,0)]; // instruction address, size before control transfer

  while int_stack.len() > 0 {
    let (addr,size_before) = int_stack.pop().unwrap();    
    let ins = if addr >= routine.instructions.len() {
      // virtual "diverge" instruction past the end of the routine
      // Needs to be constant as well
      Jump(routine.instructions.len())
    } else {
      routine.instructions[addr]
    };
    debug!("SS {:3u} [{:2u}] {}", addr,size_before, ins);

    // Check if we can reach this point with two different stack sizes
    match stack_size[addr] {
      None => {
        // fine so far
        debug!("SS assign {:3u} = [{:2u}]", addr, size_before);
        stack_size[addr] = Some(size_before)
      }
      Some(other_size) if other_size != size_before => 
        return Err(NonConstantStackSize(addr, other_size, size_before)),
      _ => {
        // fine and we can stop evaluating this branch here (fixed-point reached)
        continue;
      }
    }

    // Perform stack size computation
    let next_size = (size_before as int) + ins.stack_size_delta();
    let schedule_next = |addr:uint| {
      debug!("SS schedule {:3u} [{:2i}]",addr,next_size);
      if next_size < 0 {
        return Err(StaticStackUnderflow(addr))
      }
      int_stack.push((addr,next_size as uint));
      return Ok(())
    };
    match ins {
      Jump(x) => try!(schedule_next(x)),
      JumpZero(x) => {
        try!(schedule_next(x));
        try!(schedule_next(addr+1));
      },
      _ => try!(schedule_next(addr+1))
    }
  }

  // Assig max stack size
  let max = stack_size.iter().filter_map(|x_opt| *x_opt).max_by(|x| *x).unwrap_or(0u);
  debug!("SS max stack size {:}",max);
  routine.max_stack_size = Some(max);

  Ok(())
}

pub fn analyze_program(program: &mut Program) -> AnalysisResult<()> {
  for routine in program.routines.iter_mut() {
    try!(compute_max_stack_size(routine));
  }
  Ok(())
}