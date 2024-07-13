
  $ fangu -x array_size.tig
  3
  5
  5
  6
  3

  $ cat _fangbuild/array_size.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      push rbp
      mov rbp, rsp
      push r12
      push rbx
      mov r12, 5
      mov rbx, r12
      add rbx, 1
      mov rdi, 3
      mov rsi, 0
      call fang_alloc_array
      mov QWORD PTR [rax + 8], 1
      mov QWORD PTR [rax + 16], 2
      mov QWORD PTR [rax + 24], 3
      mov rdi, 5
      mov rsi, 10
      call fang_alloc_array
      mov rdi, r12
      mov rsi, 20
      call fang_alloc_array
      mov rdi, rbx
      mov rsi, 30
      call fang_alloc_array
      mov rdi, 3
      call fang_io_print_int
      call fang_io_print_line
      mov rdi, 5
      call fang_io_print_int
      call fang_io_print_line
      mov rdi, 5
      call fang_io_print_int
      call fang_io_print_line
      mov rdi, 6
      call fang_io_print_int
      call fang_io_print_line
      mov rdi, rbp
      call fang.make_numbers
      mov rdi, QWORD PTR [rax]
      call fang_io_print_int
      call fang_io_print_line
      mov rax, 0
      pop rbx
      pop r12
      leave
      ret
  
  fang.make_numbers:
      mov rdi, 3
      mov rsi, 40
      call fang_alloc_array
      ret
