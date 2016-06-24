@echo off

rem if %1!==! goto end
rem if %1 == vc12 goto vc12


rem :vc12
call "%VS120COMNTOOLS%\vsvars32.bat"
..\Win32\Debug\cobc -C -frelax-syntax -I "..\..\debugger" ..\..\debugger\gc-debugger.cob

goto end


:end
