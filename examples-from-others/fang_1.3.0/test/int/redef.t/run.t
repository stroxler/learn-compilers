
  $ fangu redef.tig -x
  60

  $ cat _fangbuild/redef.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      push rbp
      mov rbp, rsp
      push r12
      push rbx
      mov rdi, rbp
      call fang.value
      mov rbx, rax
      mov rdi, rbp
      call fang.value_1
      mov r12, rax
      mov rdi, rbp
      call fang.value_2
      mov rdi, rbx
      add rdi, r12
      add rdi, rax
      call fang_io_print_int
      call fang_io_print_line
      mov rax, 0
      pop rbx
      pop r12
      leave
      ret
  
  fang.value:
      mov rax, 10
      ret
  
  fang.value_1:
      mov rax, 20
      ret
  
  fang.value_2:
      mov rax, 30
      ret
