
  $ fangu nil_access.tig -x
  Fatal error: access of [nil] value
  fangu: [ERROR] run ['$TESTCASE_ROOT/_fangbuild/nil_access']: killed by signal SIGABRT
  [1]

  $ cat _fangbuild/nil_access.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      mov rax, 0
      call fang_error_nil_access
      mov rdi, QWORD PTR [rax]
      call fang_io_print
      mov rax, 0
      ret
