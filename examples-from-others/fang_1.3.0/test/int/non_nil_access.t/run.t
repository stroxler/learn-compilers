
  $ fangu non_nil_access.tig -x
  Joe
  Joe

  $ cat _fangbuild/non_nil_access.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      push rbx
      mov rdi, 2
      call fang_alloc_record
      mov QWORD PTR [rax], 42
      lea rbx, str0
      mov QWORD PTR [rax + 8], rbx
      mov rbx, rax
      mov rdi, QWORD PTR [rax + 8]
      call fang_io_print
      call fang_io_print_line
      mov rdi, QWORD PTR [rbx + 8]
      call fang_io_print
      call fang_io_print_line
      mov rax, 0
      pop rbx
      ret
  
  str0:
      .dc.a 3
      .string "Joe"
