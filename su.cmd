@echo off
if "%1"=="" runas /user:supervisor cmd.exe
if not "%1"=="" runas /user:%1 cmd.exe
