
set LIB=
set CURR=%CD%


c:
cd "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\bin\"
call compilervars.bat ia32 vs2015

d:
cd %CURR%

set LIB=%LIB%;"C:\Program Files (x86)\Microsoft SDKs\Windows\v7.1A\Lib"
set LIB=%LIB%;"C:\Program Files (x86)\Windows Kits\10\Lib\10.0.10240.0\ucrt\x86"

ifort.exe /c c_interface_module.f90
