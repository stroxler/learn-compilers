
  $ fangu string_lengths.tig -x
  5606

  $ cat _fangbuild/string_lengths.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      push r13
      push r12
      push rbx
      lea rdi, str0
      lea rbx, str1
      lea r12, str2
      lea r13, str3
      call fang_string_length
      mov rdi, rax
      call fang_io_print_int
      mov rdi, rbx
      call fang_string_length
      mov rdi, rax
      call fang_io_print_int
      mov rdi, r12
      call fang_string_length
      mov rdi, rax
      call fang_io_print_int
      mov rdi, r13
      call fang_string_length
      mov rdi, rax
      call fang_io_print_int
      mov rax, 0
      pop rbx
      pop r12
      pop r13
      ret
  
  str2:
      .dc.a 0
      .string ""
  
  str0:
      .dc.a 5
      .string "apple"
  
  str1:
      .dc.a 6
      .string "banana"
  
  str3:
      .dc.a 6
      .string "carrot"
