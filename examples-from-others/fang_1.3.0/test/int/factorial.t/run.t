
  $ fangu factorial.tig -x
  720

  $ cat _fangbuild/factorial.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      push rbp
      mov rbp, rsp
      mov rdi, rbp
      mov rsi, 6
      call fang.factorial
      mov rdi, rax
      call fang_io_print_int
      call fang_io_print_line
      mov rax, 0
      leave
      ret
  
  fang.factorial:
      push rbx
      cmp rsi, 1
      jg .L1
      mov rax, 1
  .L2:
      pop rbx
      ret
  .L1:
      mov rbx, rsi
      sub rsi, 1
      call fang.factorial
      mov rcx, rax
      mov rax, rbx
      imul rax, rcx
      jmp .L2
