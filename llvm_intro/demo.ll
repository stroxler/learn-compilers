; Note: llvm will use integers for labels in some contexts,
; but when writing be hand you want to avoid that because it can
; muddle things.


; fun foo(x, y, z) = x * y + z

define i32 @foo (i32 %x, i32 %y, i32 %z) {
  %a = mul i32 %x, %y
  %b = add i32 %a, %z
  ret i32 %b
}
