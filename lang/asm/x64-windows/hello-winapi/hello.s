
hello.exe:     file format pei-x86-64


Disassembly of section .text:

0000000140001000 <.text>:
   140001000:	48 83 ec 28          	sub    rsp,0x28
   140001004:	48 31 c9             	xor    rcx,rcx
   140001007:	48 8d 15 01 20 00 00 	lea    rdx,[rip+0x2001]        # 0x14000300f
   14000100e:	4c 8d 05 eb 1f 00 00 	lea    r8,[rip+0x1feb]        # 0x140003000
   140001015:	4d 31 c9             	xor    r9,r9
   140001018:	e8 05 00 00 00       	call   0x140001022
   14000101d:	48 83 c4 28          	add    rsp,0x28
   140001021:	c3                   	ret
   140001022:	ff 25 d8 0f 00 00    	jmp    QWORD PTR [rip+0xfd8]        # 0x140002000
