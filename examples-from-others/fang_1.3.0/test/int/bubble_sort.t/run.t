
  $ fangu bubble_sort.tig -x
  2
  3
  4
  5
  6
  8
  10
  12

  $ cat _fangbuild/bubble_sort.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      push rbp
      mov rbp, rsp
      push r12
      push rbx
      mov rdi, 8
      mov rsi, 0
      call fang_alloc_array
      mov rbx, rax
      mov QWORD PTR [rbx + 8], 10
      mov QWORD PTR [rbx + 16], 2
      mov QWORD PTR [rbx + 24], 5
      mov QWORD PTR [rbx + 32], 3
      mov QWORD PTR [rbx + 40], 6
      mov QWORD PTR [rbx + 48], 8
      mov QWORD PTR [rbx + 56], 4
      mov QWORD PTR [rbx + 64], 12
      mov r12, 8
      mov rdi, rbp
      mov rsi, rbx
      mov rdx, r12
      call fang.bubble_sort
      mov rdi, rbp
      mov rsi, rbx
      mov rdx, r12
      call fang.print_data
      mov rax, 0
      pop rbx
      pop r12
      leave
      ret
  
  fang.bubble_sort:
      push rbp
      mov rbp, rsp
      sub rsp, 24
      push r15
      push r14
      push r13
      push r12
      push rbx
      mov QWORD PTR [rbp - 24], rdx
      mov QWORD PTR [rbp - 16], rsi
      mov QWORD PTR [rbp - 8], rdi
  .L1:
      mov r13, 0
      mov r12, 1
      mov rbx, QWORD PTR [rbp - 24]
      sub rbx, 1
      cmp r12, rbx
      jg .L7
  .L5:
      mov r14, QWORD PTR [rbp - 16]
      mov r15, r12
      sub r15, 1
      mov rdi, r14
      mov rsi, r15
      call fang_check_array_index
      mov r15, QWORD PTR [r14 + 8*r15 + 8]
      mov r14, QWORD PTR [rbp - 16]
      mov rdi, r14
      mov rsi, r12
      call fang_check_array_index
      mov rax, QWORD PTR [r14 + 8*r12 + 8]
      cmp r15, rax
      jle .L4
      mov rdi, rbp
      mov rsi, r12
      sub rsi, 1
      mov rdx, r12
      call fang.bubble_sort.swap
      mov r13, 1
  .L4:
      cmp r12, rbx
      jge .L7
      inc r12
      jmp .L5
  .L7:
      mov rdi, r13
      call fang_not
      cmp rax, 0
      je .L1
      mov rax, 0
      pop rbx
      pop r12
      pop r13
      pop r14
      pop r15
      leave
      ret
  
  fang.bubble_sort.swap:
      push rbp
      mov rbp, rsp
      sub rsp, 8
      push r15
      push r14
      push r13
      push r12
      push rbx
      mov rbx, rdx
      mov r12, rsi
      mov QWORD PTR [rbp - 8], rdi
      mov rax, QWORD PTR [rbp - 8]
      mov r13, QWORD PTR [rax - 16]
      mov rdi, r13
      mov rsi, r12
      call fang_check_array_index
      mov r13, QWORD PTR [r13 + 8*r12 + 8]
      mov rax, QWORD PTR [rbp - 8]
      mov r14, QWORD PTR [rax - 16]
      mov rdi, r14
      mov rsi, r12
      call fang_check_array_index
      mov rax, QWORD PTR [rbp - 8]
      mov r15, QWORD PTR [rax - 16]
      mov rdi, r15
      mov rsi, rbx
      call fang_check_array_index
      mov rax, QWORD PTR [r15 + 8*rbx + 8]
      mov QWORD PTR [r14 + 8*r12 + 8], rax
      mov rax, QWORD PTR [rbp - 8]
      mov r12, QWORD PTR [rax - 16]
      mov rdi, r12
      mov rsi, rbx
      call fang_check_array_index
      mov QWORD PTR [r12 + 8*rbx + 8], r13
      mov rax, 0
      pop rbx
      pop r12
      pop r13
      pop r14
      pop r15
      leave
      ret
  
  fang.print_data:
      push r13
      push r12
      push rbx
      mov r13, rsi
      mov r12, 0
      sub rdx, 1
      mov rbx, rdx
      cmp r12, rbx
      jg .L12
  .L10:
      mov rdi, r13
      mov rsi, r12
      call fang_check_array_index
      mov rdi, QWORD PTR [r13 + 8*r12 + 8]
      call fang_io_print_int
      call fang_io_print_line
      cmp r12, rbx
      jge .L12
      inc r12
      jmp .L10
  .L12:
      mov rax, 0
      pop rbx
      pop r12
      pop r13
      ret
