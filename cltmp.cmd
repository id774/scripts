@echo off
if exist "c:\tmp\*.*" del /q /s /f "c:\tmp\*.*" >nul
if exist "c:\tmp" for /d %%i in (c:\tmp\*.*) do rmdir /q /s "%%i" > nul
if exist "d:\tmp\*.*" del /q /s /f "d:\tmp\*.*" >nul
if exist "d:\tmp" for /d %%i in (d:\tmp\*.*) do rmdir /q /s "%%i" > nul
