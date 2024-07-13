
  $ fangu linked_list.tig -x
  60

  $ cat _fangbuild/linked_list.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      push rbp
      mov rbp, rsp
      mov rdi, rbp
      mov rsi, 30
      mov rdx, 0
      call fang.cons
      mov rdx, rax
      mov rdi, rbp
      mov rsi, 20
      call fang.cons
      mov rdx, rax
      mov rdi, rbp
      mov rsi, 10
      call fang.cons
      mov rsi, rax
      mov rdi, rbp
      call fang.sum
      mov rdi, rax
      call fang_io_print_int
      call fang_io_print_line
      mov rax, 0
      leave
      ret
  
  fang.cons:
      push r12
      push rbx
      mov rbx, rdx
      mov r12, rsi
      mov rdi, 2
      call fang_alloc_record
      mov QWORD PTR [rax], r12
      mov QWORD PTR [rax + 8], rbx
      pop rbx
      pop r12
      ret
  
  fang.sum:
      push r13
      push r12
      push rbx
      mov rbx, rsi
      mov r12, rdi
      cmp rbx, 0
      jne .L1
      mov rax, 0
  .L6:
      pop rbx
      pop r12
      pop r13
      ret
  .L1:
      cmp rbx, 0
      jne .L2
      call fang_error_nil_access
  .L2:
      mov r13, QWORD PTR [rbx]
      cmp rbx, 0
      jne .L4
      call fang_error_nil_access
  .L4:
      mov rdi, r12
      mov rsi, QWORD PTR [rbx + 8]
      call fang.sum
      mov rbx, rax
      mov rax, r13
      add rax, rbx
      jmp .L6
