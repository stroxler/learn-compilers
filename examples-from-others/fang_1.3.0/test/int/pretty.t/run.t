
  $ fangu pretty.tig -x
  apple
   banana
    .
    carrot
     durian
      .
      .
     eggplant
      .
      .
   .
  

  $ cat _fangbuild/pretty.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      push rbp
      mov rbp, rsp
      push r13
      push r12
      push rbx
      mov rdi, 3
      call fang_alloc_record
      mov rbx, rax
      lea rax, str4
      mov QWORD PTR [rbx], rax
      mov rdi, 3
      call fang_alloc_record
      mov r12, rax
      lea rax, str5
      mov QWORD PTR [r12], rax
      mov QWORD PTR [r12 + 8], 0
      mov rdi, 3
      call fang_alloc_record
      mov r13, rax
      lea rax, str6
      mov QWORD PTR [r13], rax
      mov rdi, rbp
      lea rsi, str7
      call fang.leaf
      mov QWORD PTR [r13 + 8], rax
      mov rdi, rbp
      lea rsi, str8
      call fang.leaf
      mov QWORD PTR [r13 + 16], rax
      mov QWORD PTR [r12 + 16], r13
      mov QWORD PTR [rbx + 8], r12
      mov QWORD PTR [rbx + 16], 0
      mov rsi, rbx
      mov rdi, rbp
      call fang.pretty
      mov rdi, rax
      call fang_io_print
      call fang_io_print_line
      mov rax, 0
      pop rbx
      pop r12
      pop r13
      leave
      ret
  
  fang.leaf:
      push rbx
      mov rbx, rsi
      mov rdi, 3
      call fang_alloc_record
      mov QWORD PTR [rax], rbx
      mov QWORD PTR [rax + 8], 0
      mov QWORD PTR [rax + 16], 0
      pop rbx
      ret
  
  fang.pretty:
      push rbp
      mov rbp, rsp
      sub rsp, 16
      mov rdx, rsi
      mov QWORD PTR [rbp - 8], rdi
      lea rax, str0
      mov QWORD PTR [rbp - 16], rax
      mov rdi, rbp
      mov rsi, 0
      call fang.pretty.show
      mov rax, QWORD PTR [rbp - 16]
      leave
      ret
  
  fang.pretty.show:
      push rbp
      mov rbp, rsp
      sub rsp, 16
      push rbx
      mov rbx, rdx
      mov QWORD PTR [rbp - 16], rsi
      mov QWORD PTR [rbp - 8], rdi
      cmp rbx, 0
      jne .L4
      mov rdi, rbp
      lea rsi, str3
      call fang.pretty.show.indent
      mov rax, 0
  .L11:
      mov rax, 0
      pop rbx
      leave
      ret
  .L4:
      cmp rbx, 0
      jne .L5
      call fang_error_nil_access
  .L5:
      mov rdi, rbp
      mov rsi, QWORD PTR [rbx]
      call fang.pretty.show.indent
      cmp rbx, 0
      jne .L7
      call fang_error_nil_access
  .L7:
      mov rdi, QWORD PTR [rbp - 8]
      mov rsi, QWORD PTR [rbp - 16]
      add rsi, 1
      mov rdx, QWORD PTR [rbx + 8]
      call fang.pretty.show
      cmp rbx, 0
      jne .L9
      call fang_error_nil_access
  .L9:
      mov rdi, QWORD PTR [rbp - 8]
      mov rsi, QWORD PTR [rbp - 16]
      add rsi, 1
      mov rdx, QWORD PTR [rbx + 16]
      call fang.pretty.show
      mov rax, 0
      jmp .L11
  
  fang.pretty.show.indent:
      push rbp
      mov rbp, rsp
      sub rsp, 8
      push r13
      push r12
      push rbx
      mov rbx, rsi
      mov QWORD PTR [rbp - 8], rdi
      mov r13, 1
      mov rax, QWORD PTR [rbp - 8]
      mov r12, QWORD PTR [rax - 16]
      cmp r13, r12
      jg .L2
  .L0:
      mov rax, QWORD PTR [rbp - 8]
      mov rdi, QWORD PTR [rax - 8]
      lea rsi, str1
      call fang.pretty.write
      cmp r13, r12
      jge .L2
      inc r13
      jmp .L0
  .L2:
      mov rax, QWORD PTR [rbp - 8]
      mov rax, QWORD PTR [rax - 8]
      mov rdi, QWORD PTR [rax - 16]
      mov rsi, rbx
      call fang_string_concat
      mov rbx, QWORD PTR [rbp - 8]
      mov rbx, QWORD PTR [rbx - 8]
      mov QWORD PTR [rbx - 16], rax
      mov rax, QWORD PTR [rbp - 8]
      mov rdi, QWORD PTR [rax - 8]
      lea rsi, str2
      call fang.pretty.write
      mov rax, 0
      pop rbx
      pop r12
      pop r13
      leave
      ret
  
  fang.pretty.write:
      push rbx
      mov rbx, rdi
      mov rdi, QWORD PTR [rbx - 16]
      call fang_string_concat
      mov QWORD PTR [rbx - 16], rax
      mov rax, 0
      pop rbx
      ret
  
  str0:
      .dc.a 0
      .string ""
  
  str2:
      .dc.a 1
      .string "\n"
  
  str1:
      .dc.a 1
      .string " "
  
  str3:
      .dc.a 1
      .string "."
  
  str4:
      .dc.a 5
      .string "apple"
  
  str5:
      .dc.a 6
      .string "banana"
  
  str6:
      .dc.a 6
      .string "carrot"
  
  str7:
      .dc.a 6
      .string "durian"
  
  str8:
      .dc.a 8
      .string "eggplant"
