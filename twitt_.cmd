@echo off
if "%1"=="" goto usage
call rtwitter twitt_ xxxxxxxx %*
goto end

:usage
echo [Usage]
echo %0 status
:end
