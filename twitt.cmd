@echo off
if "%1"=="" goto usage
call rtwitter twitt xxxxxxxx %*
goto end

:usage
echo [Usage]
echo %0 status
:end
