.data
  l2: .ascii "hello"

.text
main: 
  la $t3, l2
  addi $t4, $t3, 3
  move $a0, $t4
  li $v0, 4
  syscall
  li $t0, 15
  addi $t0, $t0, -12
  j exit
exit:
  li $v0, 10
  syscall