
  $ fangu multiply.tig -x
  36

  $ cat _fangbuild/multiply.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      push rbp
      mov rbp, rsp
      mov rdi, rbp
      mov rsi, 3
      mov rdx, 12
      call fang.multiply
      mov rdi, rax
      call fang_io_print_int
      call fang_io_print_line
      mov rax, 0
      leave
      ret
  
  fang.multiply:
      mov rax, 0
  .L1:
      add rax, rdx
      dec rsi
      cmp rsi, 0
      jg .L1
      ret
