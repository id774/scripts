@echo off
if exist "%TMP%\*.*" del /q /s /f "%TMP%\*.*" >nul
if exist "%TMP%" for /d %%i in (%TMP%\*.*) do rmdir /q /s "%%i" > nul
