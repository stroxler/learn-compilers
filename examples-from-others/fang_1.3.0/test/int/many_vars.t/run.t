
  $ fangu many_vars.tig -x
  45

  $ cat _fangbuild/many_vars.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      push rbp
      mov rbp, rsp
      sub rsp, 112
      push r15
      push r14
      push r13
      push r12
      push rbx
      mov rax, 1
      mov QWORD PTR [rbp - 8], rax
      mov rax, 2
      mov QWORD PTR [rbp - 16], rax
      mov rax, 3
      mov QWORD PTR [rbp - 24], rax
      mov rax, 4
      mov QWORD PTR [rbp - 32], rax
      mov rax, 5
      mov QWORD PTR [rbp - 40], rax
      mov rax, 6
      mov QWORD PTR [rbp - 48], rax
      mov rax, 7
      mov QWORD PTR [rbp - 104], rax
      mov rax, 8
      mov QWORD PTR [rbp - 112], rax
      mov r13, 9
      mov r14, 10
      mov r12, 11
      mov r11, 12
      mov rcx, 13
      mov rdx, 14
      mov rsi, 15
      mov r8, 16
      mov r9, 17
      mov r10, 18
      mov rbx, 19
      mov rax, 20
      sub rax, 1
      mov QWORD PTR [rbp - 8], rax
      sub rbx, 2
      mov QWORD PTR [rbp - 16], rbx
      sub r10, 3
      mov QWORD PTR [rbp - 24], r10
      sub r9, 4
      mov QWORD PTR [rbp - 32], r9
      sub r8, 5
      mov QWORD PTR [rbp - 40], r8
      sub rsi, 6
      mov QWORD PTR [rbp - 48], rsi
      sub rdx, 7
      mov QWORD PTR [rbp - 104], rdx
      sub rcx, 8
      mov QWORD PTR [rbp - 112], rcx
      mov r13, r11
      sub r13, 9
      mov r14, r12
      sub r14, 10
      mov r12, r14
      sub r12, 11
      mov r11, r13
      sub r11, 12
      mov rcx, QWORD PTR [rbp - 112]
      sub rcx, 13
      mov rdx, QWORD PTR [rbp - 104]
      sub rdx, 14
      mov rsi, QWORD PTR [rbp - 48]
      sub rsi, 15
      mov r8, QWORD PTR [rbp - 40]
      sub r8, 16
      mov r9, QWORD PTR [rbp - 32]
      sub r9, 17
      mov r10, QWORD PTR [rbp - 24]
      sub r10, 18
      mov rbx, QWORD PTR [rbp - 16]
      sub rbx, 19
      mov rax, QWORD PTR [rbp - 8]
      sub rax, 20
      mov rdi, QWORD PTR [rbp - 8]
      mov QWORD PTR [rbp - 96], rdi
      mov rdi, QWORD PTR [rbp - 96]
      mov r15, QWORD PTR [rbp - 16]
      add rdi, r15
      mov QWORD PTR [rbp - 96], rdi
      mov rdi, QWORD PTR [rbp - 96]
      mov QWORD PTR [rbp - 88], rdi
      mov rdi, QWORD PTR [rbp - 88]
      mov r15, QWORD PTR [rbp - 24]
      add rdi, r15
      mov QWORD PTR [rbp - 88], rdi
      mov rdi, QWORD PTR [rbp - 88]
      mov QWORD PTR [rbp - 80], rdi
      mov rdi, QWORD PTR [rbp - 80]
      mov r15, QWORD PTR [rbp - 32]
      add rdi, r15
      mov QWORD PTR [rbp - 80], rdi
      mov rdi, QWORD PTR [rbp - 80]
      mov QWORD PTR [rbp - 72], rdi
      mov rdi, QWORD PTR [rbp - 72]
      mov r15, QWORD PTR [rbp - 40]
      add rdi, r15
      mov QWORD PTR [rbp - 72], rdi
      mov rdi, QWORD PTR [rbp - 72]
      mov QWORD PTR [rbp - 64], rdi
      mov rdi, QWORD PTR [rbp - 64]
      mov r15, QWORD PTR [rbp - 48]
      add rdi, r15
      mov QWORD PTR [rbp - 64], rdi
      mov rdi, QWORD PTR [rbp - 64]
      mov QWORD PTR [rbp - 56], rdi
      mov r15, QWORD PTR [rbp - 56]
      mov rdi, QWORD PTR [rbp - 104]
      add r15, rdi
      mov QWORD PTR [rbp - 56], r15
      mov rdi, QWORD PTR [rbp - 56]
      mov r15, QWORD PTR [rbp - 112]
      add rdi, r15
      add rdi, r13
      add rdi, r14
      add rdi, r12
      add rdi, r11
      add rdi, rcx
      add rdi, rdx
      add rdi, rsi
      add rdi, r8
      add rdi, r9
      add rdi, r10
      add rdi, rbx
      add rdi, rax
      call fang_io_print_int
      call fang_io_print_line
      mov rax, 0
      pop rbx
      pop r12
      pop r13
      pop r14
      pop r15
      leave
      ret
