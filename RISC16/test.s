addi 2, 0, 32
jalr 1, 2

lui 1, 0
lui 2, 0
lui 3, 0

addi 2, 0, 10
addi 1, 0, 0
loop: addi 1, 1, 1
beq 2, 1, 1
beq 0, 0, loop

lw 1, 2, 0

lui 1, 0
lui 2, 0
lui 3, 0

addi 1, 1, 55
addi 2, 2, 63
sw 1, 2, 1

lui 1, 0
lui 2, 0
lui 3, 0

addi 2, 2, 50
addi 1, 1, 30
add 3, 2, 1

lui 1, 1
lui 2, 0
lui 3, 0

addi 1, 1, 12
addi 2, 2, 10
nand 3, 2, 1
