@echo off
if "%TMP%"=="" goto notmp
if exist "%TMP%\_set_datetime.bat" del "%TMP%\_set_datetime.bat"
if "%RUBY%"=="" if exist C:\ruby186\bin\ruby.exe set RUBY=C:\ruby186\bin\ruby.exe
if "%RUBY%"=="" if exist C:\ruby185\bin\ruby.exe set RUBY=C:\ruby185\bin\ruby.exe
if "%RUBY%"=="" set RUBY=C:\ruby\bin\ruby.exe
if "%SCRIPTS%"=="" set SCRIPTS=C:\scripts
set RUBYFILE=%SCRIPTS%\set_datetime.rb
if exist "%RUBY%" goto execute
echo ruby not found.
goto endofruby
:execute
if "%OS%" == "Windows_NT" goto WinNT
"%RUBY%" -Sx "%RUBYFILE%" %1 %2 %3 %4 %5 %6 %7 %8 %9 > "%TMP%\_set_datetime.bat"
goto endofruby
:WinNT
"%RUBY%" -x "%RUBYFILE%" %* > "%TMP%\_set_datetime.bat"
goto endofruby
:endofruby
call "%TMP%\_set_datetime.bat"
del "%TMP%\_set_datetime.bat"
goto end
:notmp
echo Environment Variable TMP is not set.
goto end
:end
