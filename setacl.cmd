@echo off
if "%1"=="" goto usage
if "%2"=="" goto usage
if not exist "C:\Program Files\%1" goto err1
if not exist "C:\Program Files\%1\DefaultUser" goto err2
md "C:\Program Files\%1\%2"
cacls "C:\Program Files\%1\%2" /C /T /E /P "%2:C"
xcopy "C:\Program Files\%1\DefaultUser" "C:\Program Files\%1\%2" /S /E /V /R /H /Y
goto end
:err1
echo ディレクトリ %1 が存在しません。
goto end
:err2
echo %1 にサブディレクトリ DefaultUser が存在しません。
goto end
:usage
echo アプリケーションごとにユーザーアクセス制御リストを設定します。
echo.
echo usage: setacl [フォルダ名] [ユーザー名]
:end
