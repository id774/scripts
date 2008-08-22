@echo off
if "%RUBY%"=="" if exist C:\ruby186\bin\ruby.exe set RUBY=C:\ruby186\bin\ruby.exe
if "%RUBY%"=="" if exist C:\ruby185\bin\ruby.exe set RUBY=C:\ruby185\bin\ruby.exe
if "%RUBY%"=="" set RUBY=C:\ruby\bin\ruby.exe
if "%SCRIPTS%"=="" set SCRIPTS=C:\scripts
set RUBYFILE=%SCRIPTS%\%0.rb
if "%RUBYLOG%"=="" set RUBYLOG=CON
if exist "%RUBY%" goto execute
echo ruby not found.
goto endofruby
:execute
if "%OS%" == "Windows_NT" goto WinNT
"%RUBY%" -Sx "%RUBYFILE%" %1 %2 %3 %4 %5 %6 %7 %8 %9 >> "%RUBYLOG%"
goto endofruby
:WinNT
"%RUBY%" -x "%RUBYFILE%" %* >> "%RUBYLOG%"
goto endofruby
:endofruby
