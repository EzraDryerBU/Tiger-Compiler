.global tigermain

.data
L19:
.long 4
.ascii "asdf"

.text
tigermain:
PUSH %ebp
MOV %esp, %ebp
ADD $-4, %esp
PUSH %ebx
PUSH %edi
PUSH %esi
MOV %ebx, -4(%ebp)
L21:
PUSH %ecx
PUSH %edx
PUSH $L19
PUSH %ebp
CALL L18
ADD $8, %esp
POP %edx
POP %ecx
MOV -4(%ebp), %ebx
MOV %eax, %ebx
PUSH %ecx
PUSH %edx
PUSH %ebx
CALL print
ADD $4, %esp
POP %edx
POP %ecx
MOV %eax, %eax
MOV %ebx, -4(%ebp)
JMP L20
L20:
POP %esi
POP %edi
POP %ebx
ADD $4, %esp
POP %ebp
RET
L18:
PUSH %ebp
MOV %esp, %ebp
ADD $-12, %esp
PUSH %ebx
PUSH %edi
PUSH %esi
MOV %ebx, -4(%ebp)
MOV %edi, -8(%ebp)
MOV %esi, -12(%ebp)
L24:
MOV -8(%ebp), %ebx
MOV %ebp, %ebx
MOV -12(%ebp), %edi
MOV $12, %edi
ADD %edi, %ebx
MOV -4(%ebp), %esi
MOV (%ebx), %esi
MOV %esi, %eax
MOV %esi, -4(%ebp)
MOV %ebx, -8(%ebp)
MOV %edi, -12(%ebp)
JMP L23
L23:
POP %esi
POP %edi
POP %ebx
ADD $12, %esp
POP %ebp
RET
