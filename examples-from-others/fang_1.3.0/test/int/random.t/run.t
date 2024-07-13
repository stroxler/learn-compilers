
  $ fangu -x random.tig
  28
  5
  100
  9
  15
  8
  29
  45
  50
  90
  2
  28
  98
  23
  42
  26
  37
  28
  42
  26
  31
  47
  46
  29
  8
  83
  97
  40
  38
  97
  48
  65
  2
  48
  73
  17
  55
  1
  62
  5
  91
  63
  33
  88
  85
  74
  14
  22
  2
  55
  47
  32
  1
  93
  60
  9
  75
  57
  48
  12
  54
  96
  77
  56
  43
  50
  72
  98
  51
  33
  3
  41
  96
  35
  28
  81
  9
  41
  2
  10
  96
  49
  41
  97
  41
  1
  5
  15
  58
  53
  27
  11
  48
  3
  66
  90
  52
  38
  88
  3

  $ cat _fangbuild/random.s
      .intel_syntax noprefix
      .global fang
  
  fang:
      push r12
      push rbx
      mov rdi, 5
      call fang_io_seed_random
      mov r12, 1
      mov rbx, 100
      cmp r12, rbx
      jg .L2
  .L0:
      mov rdi, 1
      mov rsi, 101
      call fang_io_random
      mov rdi, rax
      call fang_io_print_int
      call fang_io_print_line
      cmp r12, rbx
      jge .L2
      inc r12
      jmp .L0
  .L2:
      mov rax, 0
      pop rbx
      pop r12
      ret
