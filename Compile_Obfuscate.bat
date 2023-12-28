@echo off
set "filename=input.lua"

if not exist "%filename%" (
    echo The file "%filename%" does not exist.
    pause
)else (
color b
cd /d "%~dp0
cd Source\Obfuscation\netcoreapp2.0
dotnet "IronBrew2 CLI.dll" "..\..\..\Input.lua"
cls
cd ..\..\..
echo [!] Obfuscated lua file
echo [!] Compiling input.lua (This wont take long)
Source\glue.exe Source\Compiler.exe "Source\Obfuscation\netcoreapp2.0\out.lua" Compiled_Lua.exe
echo [!] Lua file compiled.
pause

)

