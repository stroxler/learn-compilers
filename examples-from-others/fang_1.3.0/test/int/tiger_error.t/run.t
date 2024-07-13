
  $ fangu -x tiger_error.tig
  Fatal error: the Tiger program failed with "division by zero"
  fangu: [ERROR] run ['$TESTCASE_ROOT/_fangbuild/tiger_error']: killed by signal SIGABRT
  [1]

  $ cat _fangbuild/tiger_error.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      push rbp
      mov rbp, rsp
      mov rdi, rbp
      mov rsi, 10
      mov rdx, 0
      call fang.safe_div
      mov rdi, rax
      call fang_io_print_int
      call fang_io_print_line
      mov rax, 0
      leave
      ret
  
  fang.safe_div:
      push r12
      push rbx
      mov rbx, rdx
      mov r12, rsi
      cmp rbx, 0
      jne .L1
      lea rdi, str0
      call fang_io_tiger_error
  .L1:
      mov rdx, 0
      mov rax, r12
      idiv rbx
      pop rbx
      pop r12
      ret
  
  str0:
      .dc.a 16
      .string "division by zero"
