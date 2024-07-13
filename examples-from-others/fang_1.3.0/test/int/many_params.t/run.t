
  $ fangu many_params.tig -x
  45

  $ cat _fangbuild/many_params.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      push rbp
      mov rbp, rsp
      push 9
      push 8
      push 7
      push 6
      mov rdi, rbp
      mov rsi, 1
      mov rdx, 2
      mov rcx, 3
      mov r8, 4
      mov r9, 5
      call fang.add
      add rsp, 32
      mov rdi, rax
      call fang_io_print_int
      call fang_io_print_line
      mov rax, 0
      leave
      ret
  
  fang.add:
      push rbp
      mov rbp, rsp
      add rsi, rdx
      add rsi, rcx
      add rsi, r8
      add rsi, r9
      mov rcx, QWORD PTR [rbp + 16]
      mov rax, rsi
      add rax, rcx
      mov rcx, QWORD PTR [rbp + 24]
      add rax, rcx
      mov rcx, QWORD PTR [rbp + 32]
      add rax, rcx
      mov rcx, QWORD PTR [rbp + 40]
      add rax, rcx
      leave
      ret
