
  $ fangu array_sum.tig -x
  60

  $ cat _fangbuild/array_sum.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      push rbp
      mov rbp, rsp
      mov rdi, 3
      mov rsi, 0
      call fang_alloc_array
      mov rsi, rax
      mov QWORD PTR [rsi + 8], 10
      mov QWORD PTR [rsi + 16], 20
      mov QWORD PTR [rsi + 24], 30
      mov rdi, rbp
      call fang.sum
      mov rdi, rax
      call fang_io_print_int
      call fang_io_print_line
      mov rax, 0
      leave
      ret
  
  fang.sum:
      push r14
      push r13
      push r12
      push rbx
      mov r13, rsi
      mov rax, 0
      mov r12, 0
      mov rbx, QWORD PTR [r13]
      sub rbx, 1
      mov r14, rbx
      cmp r12, r14
      jg .L2
  .L0:
      mov rbx, rax
      mov rdi, r13
      mov rsi, r12
      call fang_check_array_index
      mov rcx, QWORD PTR [r13 + 8*r12 + 8]
      mov rax, rbx
      add rax, rcx
      cmp r12, r14
      jge .L2
      inc r12
      jmp .L0
  .L2:
      pop rbx
      pop r12
      pop r13
      pop r14
      ret
