.global tigermain

.data
L3:
.long 11
.ascii "IS NOT ONE\n"
L2:
.long 7
.ascii "IS ONE\n"

.text
tigermain:
PUSH %ebp
MOV %esp, %ebp
ADD $-80, %esp
PUSH %ebx
PUSH %edi
PUSH %esi
L11:
MOV %ebp, %ebx
MOV $-4, %edi
ADD %edi, %ebx
MOV $1, %esi
MOV %esi, (%ebx)
MOV %ebp, %ecx
MOV %ebx, -8(%ebp)
MOV $-4, %ebx
ADD %ebx, %ecx
MOV %edi, -12(%ebp)
MOV (%ecx), %edi
MOV %esi, -16(%ebp)
MOV $10, %esi
CMP %esi, %edi
MOV %edi, -28(%ebp)
MOV %ecx, -20(%ebp)
MOV %ebx, -24(%ebp)
MOV %esi, -32(%ebp)
jle L8
L0:
MOV $0, %ebx
MOV %ebx, %eax
MOV %ebx, -36(%ebp)
JMP L10
L8:
PUSH %ecx
PUSH %edx
PUSH %ebp
CALL L1
ADD $4, %esp
POP %edx
POP %ecx
MOV %ebp, %ebx
MOV $-4, %edi
ADD %edi, %ebx
MOV (%ebx), %esi
MOV $10, %ecx
CMP %ecx, %esi
MOV %esi, -48(%ebp)
MOV %ebx, -40(%ebp)
MOV %edi, -44(%ebp)
MOV %ecx, -52(%ebp)
jge L0
L9:
MOV %ebp, %ebx
MOV $-4, %edi
ADD %edi, %ebx
MOV %ebp, %esi
MOV $-4, %ecx
ADD %ecx, %esi
MOV %ebx, -56(%ebp)
MOV (%esi), %ebx
MOV %edi, -60(%ebp)
MOV %ebx, %edi
MOV %esi, -64(%ebp)
MOV $1, %esi
ADD %esi, %edi
MOV %ecx, -68(%ebp)
MOV -56(%ebp), %ecx
MOV %edi, (%ecx)
MOV %ecx, -56(%ebp)
MOV %edi, -76(%ebp)
MOV %ebx, -72(%ebp)
MOV %esi, -80(%ebp)
JMP L8
L10:
POP %esi
POP %edi
POP %ebx
ADD $80, %esp
POP %ebp
RET
L1:
PUSH %ebp
MOV %esp, %ebp
ADD $-32, %esp
PUSH %ebx
PUSH %edi
PUSH %esi
L13:
MOV %ebp, %ebx
MOV $8, %edi
ADD %edi, %ebx
MOV (%ebx), %esi
MOV %esi, %ecx
MOV %ebx, -4(%ebp)
MOV $-4, %ebx
ADD %ebx, %ecx
MOV %edi, -8(%ebp)
MOV (%ecx), %edi
MOV %esi, -12(%ebp)
MOV $1, %esi
CMP %esi, %edi
MOV %edi, -24(%ebp)
MOV %ecx, -16(%ebp)
MOV %ebx, -20(%ebp)
MOV %esi, -28(%ebp)
je L4
L5:
PUSH %ecx
PUSH %edx
PUSH $L3
CALL print
ADD $4, %esp
POP %edx
POP %ecx
MOV %eax, %ebx
MOV %ebx, -32(%ebp)
L6:
MOV -32(%ebp), %ebx
MOV %ebx, %eax
MOV %ebx, -32(%ebp)
JMP L12
L4:
PUSH %ecx
PUSH %edx
PUSH $L2
CALL print
ADD $4, %esp
POP %edx
POP %ecx
MOV -32(%ebp), %ebx
MOV %eax, %ebx
MOV %ebx, -32(%ebp)
JMP L6
L12:
POP %esi
POP %edi
POP %ebx
ADD $32, %esp
POP %ebp
RET
