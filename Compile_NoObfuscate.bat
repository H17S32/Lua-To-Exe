@echo off
set "filename=input.lua"

if not exist "%filename%" (
    echo The file "%filename%" does not exist.
    pause
) else (
    echo [!] Obfuscated lua file
echo [!] Compiling input.lua (This wont take long)
Source\glue.exe Source\Compiler.exe "input.lua" Compiled_Lua.exe
echo [!] Lua file compiled.
pause

)

