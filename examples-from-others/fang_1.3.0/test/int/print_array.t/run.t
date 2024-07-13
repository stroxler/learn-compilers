
  $ fangu print_array.tig -x
  1
  2
  4
  8
  16
  32

  $ cat _fangbuild/print_array.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      push r13
      push r12
      push rbx
      mov rdi, 6
      mov rsi, 0
      call fang_alloc_array
      mov QWORD PTR [rax + 8], 1
      mov QWORD PTR [rax + 16], 2
      mov QWORD PTR [rax + 24], 4
      mov QWORD PTR [rax + 32], 8
      mov QWORD PTR [rax + 40], 16
      mov QWORD PTR [rax + 48], 32
      mov r12, rax
      mov rbx, 0
      mov r13, 5
      cmp rbx, r13
      jg .L2
  .L0:
      mov rdi, r12
      mov rsi, rbx
      call fang_check_array_index
      mov rdi, QWORD PTR [r12 + 8*rbx + 8]
      call fang_io_print_int
      call fang_io_print_line
      cmp rbx, r13
      jge .L2
      inc rbx
      jmp .L0
  .L2:
      mov rax, 0
      pop rbx
      pop r12
      pop r13
      ret
