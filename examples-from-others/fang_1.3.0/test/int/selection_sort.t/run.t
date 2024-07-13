
  $ fangu selection_sort.tig -x
  1
  2
  3
  4
  5

  $ cat _fangbuild/selection_sort.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      push rbp
      mov rbp, rsp
      push r13
      push r12
      push rbx
      mov rdi, 5
      mov rsi, 0
      call fang_alloc_array
      mov QWORD PTR [rax + 8], 5
      mov QWORD PTR [rax + 16], 3
      mov QWORD PTR [rax + 24], 4
      mov QWORD PTR [rax + 32], 1
      mov QWORD PTR [rax + 40], 2
      mov r12, rax
      mov r13, 5
      mov rdi, rbp
      mov rsi, r12
      mov rdx, r13
      call fang.selection_sort
      mov rbx, 0
      sub r13, 1
      cmp rbx, r13
      jg .L10
  .L8:
      mov rdi, r12
      mov rsi, rbx
      call fang_check_array_index
      mov rdi, QWORD PTR [r12 + 8*rbx + 8]
      call fang_io_print_int
      call fang_io_print_line
      cmp rbx, r13
      jge .L10
      inc rbx
      jmp .L8
  .L10:
      mov rax, 0
      pop rbx
      pop r12
      pop r13
      leave
      ret
  
  fang.selection_sort:
      push rbp
      mov rbp, rsp
      sub rsp, 24
      push r15
      push r14
      push r13
      push r12
      push rbx
      mov QWORD PTR [rbp - 8], rdx
      mov r12, rsi
      mov r13, 0
      mov rax, QWORD PTR [rbp - 8]
      sub rax, 2
      mov QWORD PTR [rbp - 16], rax
      mov rax, QWORD PTR [rbp - 16]
      cmp r13, rax
      jg .L7
  .L5:
      mov rbx, r13
      mov rax, r13
      add rax, 1
      mov QWORD PTR [rbp - 24], rax
      mov rax, QWORD PTR [rbp - 8]
      sub rax, 1
      mov r15, rax
      mov rax, QWORD PTR [rbp - 24]
      cmp rax, r15
      jg .L4
  .L2:
      mov rax, QWORD PTR [rbp - 24]
      mov r14, rax
      mov rdi, r12
      mov rsi, r14
      call fang_check_array_index
      mov rax, QWORD PTR [r12 + 8*r14 + 8]
      mov r14, rax
      mov rdi, r12
      mov rsi, rbx
      call fang_check_array_index
      mov rax, QWORD PTR [r12 + 8*rbx + 8]
      cmp r14, rax
      jge .L1
      mov rax, QWORD PTR [rbp - 24]
      mov rbx, rax
  .L1:
      mov rax, QWORD PTR [rbp - 24]
      cmp rax, r15
      jge .L4
      mov rax, QWORD PTR [rbp - 24]
      inc rax
      mov QWORD PTR [rbp - 24], rax
      jmp .L2
  .L4:
      mov rdi, r12
      mov rsi, r13
      call fang_check_array_index
      mov rax, QWORD PTR [r12 + 8*r13 + 8]
      mov r15, rax
      mov r14, r12
      mov rdi, r14
      mov rsi, r13
      call fang_check_array_index
      mov rdi, r12
      mov rsi, rbx
      call fang_check_array_index
      mov rax, QWORD PTR [r12 + 8*rbx + 8]
      mov QWORD PTR [r14 + 8*r13 + 8], rax
      mov rdi, r12
      mov rsi, rbx
      call fang_check_array_index
      mov QWORD PTR [r12 + 8*rbx + 8], r15
      mov rax, QWORD PTR [rbp - 16]
      cmp r13, rax
      jge .L7
      inc r13
      jmp .L5
  .L7:
      mov rax, 0
      pop rbx
      pop r12
      pop r13
      pop r14
      pop r15
      leave
      ret
