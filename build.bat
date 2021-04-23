@echo off
asm68k /m /p Main.asm, badapple.md, , badapple.lst > err.txt
type err.txt
IF NOT EXIST badapple.md pause & exit
error\convsym badapple.lst badapple.md -input asm68k_lst -inopt "/localSign=. /localJoin=. /ignoreMacroDefs+ /ignoreMacroExp- /addMacrosAsOpcodes+" -a
timeout 2
