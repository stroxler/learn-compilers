
  $ fangu call_in_loop.tig -x
  1
  2
  3
  4
  5
  6
  7
  8
  9
  10

  $ cat _fangbuild/call_in_loop.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      push r12
      push rbx
      mov rbx, 1
      mov r12, 10
      cmp rbx, r12
      jg .L2
  .L0:
      mov rdi, rbx
      call fang_io_print_int
      call fang_io_print_line
      cmp rbx, r12
      jge .L2
      inc rbx
      jmp .L0
  .L2:
      mov rax, 0
      pop rbx
      pop r12
      ret
