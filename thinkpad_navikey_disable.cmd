@echo off

echo Registry Patcher - by id774 <idnanashi@gmail.com>
echo Disable navigation key for ThinkPad Series
echo.

set REGKey=HKLM\SYSTEM\CurrentControlSet\Control\Keyboard Layout
set REGValue="Scancode Map"

set RemoteMachine=%1

if ""=="%1" goto Usage
if "/?"=="%1" goto Usage
if /I "/h"=="%1" goto Usage
if /I "/a"=="%1" goto LocalMachine
if /I "/d"=="%1" goto LocalMachine
if /I "/b"=="%1" goto LocalMachine
set RemoteMachineName=%1
set Action=%2

:Parse
if /I "/a" == "%Action%" goto Add
if /I "/d" == "%Action%" goto Delete
if /I "/b" == "%Action%" goto Browse
goto Usage

:Add
echo - 変更前のレジストリ値を表示 - 操作対象は%RemoteMachineName%
REG QUERY "\\%RemoteMachine%\%REGKey%"
echo.
echo - レジストリキーを追加します - 操作対象は%RemoteMachineName%
REG ADD "\\%RemoteMachine%\%REGKey%" /v %REGValue% /t REG_BINARY /d 00000000000000000300000000006ae0000069e000000000 /f
echo.
echo - 変更後のレジストリ値を表示 - 操作対象は%RemoteMachineName%
REG QUERY "\\%RemoteMachine%\%REGKey%"
goto End

:Delete
echo - 変更前のレジストリ値を表示 - 操作対象は%RemoteMachineName%
REG QUERY "\\%RemoteMachine%\%REGKey%"
echo.
echo - レジストリキーを削除します - 操作対象は%RemoteMachineName%
REG DELETE "\\%RemoteMachine%\%REGKey%" /v %REGValue% /f
echo.
echo - 変更後のレジストリ値を表示 - 操作対象は%RemoteMachineName%
REG QUERY "\\%RemoteMachine%\%REGKey%"
goto End

:Browse
echo - レジストリ値を表示 - 操作対象は%RemoteMachineName%
REG QUERY "\\%RemoteMachine%\%REGKey%"
goto End

:LocalMachine
set Action=%1
set RemoteMachine=.
set RemoteMachineName=localhost
goto Parse

:Usage
echo ------------------------------------------------------------
echo Usage:
echo %0 [machine_name] [/a] [/d] [/b] [/h]
echo /a = Add Registry
echo /d = Delete Registry
echo /b = Browse Registry
echo /h = Help
echo.
echo Examples:
echo %0 mymachine /a (Add Registry on machine "mymachine")
echo %0 /d (Delete Registry on the local machine)
echo ------------------------------------------------------------

:End
set REGKey=
set REGValue=
set Action=
set RemoteMachine=
set RemoteMachineName=
