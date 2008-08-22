@echo off
if "%1"=="n" goto native
if "%1"=="na" goto native
if "%1"=="nat" goto native
if "%1"=="native" goto native
if "%1"=="c" goto cpl
if "%1"=="cp" goto cpl
if "%1"=="cpl" goto cpl
if "%1"=="m" goto msc
if "%1"=="ms" goto msc
if "%1"=="msc" goto msc
if "%1"=="vim" goto gvim
if "%1"=="gvim" goto gvim
if "%1"=="ie" goto ie
if "%1"=="fx" goto fx
if "%1"=="firefox" goto fx
if "%1"=="fox" goto fx
if "%1"=="tb" goto tb
if "%1"=="thunder" goto tb
if "%1"=="thunderbird" goto tb
if "%1"=="vix" goto vix
if "%1"=="susie" goto susie
if "%1"=="vnc" goto vnc
if "%1"=="amp" goto winamp
if "%1"=="winamp" goto winamp
if "%1"=="ex" goto ex
if "%1"=="cmd" goto cmd
if "%1"=="comp" goto comp
if "%1"=="computer" goto comp
if "%1"=="desk" goto desktop
if "%1"=="top" goto desktop
if "%1"=="lock" goto lock
if "%1"=="sys" goto sys
if "%1"=="system" goto sys
if "%1"=="net" goto net
if "%1"=="network" goto net
if "%1"=="fol" goto fol
if "%1"=="fold" goto fol
if "%1"=="folder" goto fol
if "%1"=="wall" goto wall
if "%1"=="firewall" goto wall
if "%1"=="fire" goto wall
if "%1"=="app" goto app
if "%1"=="program" goto app
if "%1"=="power" goto power
if "%1"=="lang" goto lang
if "%1"=="sound" goto sound
if "%1"=="key" goto keyboard
if "%1"=="keyboard" goto keyboard
if "%1"=="mouse" goto mouse
if "%1"=="screen" goto screen
if "%1"=="center" goto center
if "%1"=="security" goto center
if "%1"=="rman" goto rman
if "%1"=="slw" goto slw
if "%1"=="vlc" goto vlc
if "%1"=="tc" goto tc
if "%1"=="truecrypt" goto tc
if "%1"=="spybot" goto spybot
if "%1"=="adaware" goto adaware
if "%1"=="toroi" goto toroi
if "%1"=="pikazip" goto pikazip
if "%1"=="sakuraw" goto sakuraw
if "%1"=="mayu" goto mayu
if "%1"=="madonote" goto madonote
if "%1"=="madotomo" goto madotomo
if "%1"=="ftp" goto ftp
if "%1"=="term" goto term
if "%1"=="scp" goto scp
if "%1"=="addcheck" goto addcheck
if "%1"=="ntfs" goto ntfs
if "%1"=="memclr" goto memclr
if "%1"=="itunes" goto itunes
if "%1"=="lhauty" goto lhauty
if "%1"=="lhaca" goto lhaca
if "%1"=="range" goto range
if "%1"=="binedit" goto binedit
if "%1"=="ipmsg" goto ipmsg
if "%1"=="jmedit2" goto jmedit2
if "%1"=="jmedit" goto jmedit2
if "%1"=="jm" goto jmedit2
if "%1"=="ezhtml" goto ezhtml
if "%1"=="ez" goto ezhtml
if "%1"=="giko" goto giko
if "%1"=="irc" goto irc
if "%1"=="eclipse" goto eclipse
if "%1"=="ftpd" goto ftpd
if "%1"=="trac" goto trac
if "%1"=="skype" goto skype
if "%1"=="mixi" goto mixi
if "%1"=="wire" goto wire
if "%1"=="wireshark" goto wire
if "%1"=="netact" goto netact
if "%1"=="neorage" goto neorage
if "%1"=="clmouse" goto clmouse
if "%1"=="clkey" goto clkey
if "%1"=="proex" goto proex
if "%1"=="snes" goto snes
if "%1"=="nes" goto nes
if "%1"=="oo" goto oo
if "%1"=="ooo" goto oo
if "%1"=="x2003" goto x2003
if "%1"=="w2003" goto w2003
if "%1"=="p2003" goto p2003
if "%1"=="a2003" goto a2003
if "%1"=="v2003" goto v2003
if "%1"=="j2003" goto j2003
if "%1"=="pgadmin" goto pgadmin
if "%1"=="shell" goto shell
if "%1"=="msconfig" goto msconfig
if "%1"=="ip" goto ip
goto usage

:gvim
"%SCRIPTS%\link\gvim.lnk"
goto end

:ie
"%SCRIPTS%\link\Internet Explorer.lnk"
goto end

:fx
"%SCRIPTS%\link\Mozilla Firefox.lnk"
goto end

:tb
"%SCRIPTS%\link\Mozilla Thunderbird.lnk"
goto end

:vix
"%SCRIPTS%\link\VIX.lnk"
goto end

:susie
"%SCRIPTS%\link\Susie32.lnk"
goto end

:vnc
"%SCRIPTS%\link\VNC ビューワの起動.lnk"
goto end

:winamp
"%SCRIPTS%\link\Winamp.lnk"
goto end

:ex
explorer.exe
goto end

:cmd
"%SCRIPTS%\link\コマンド プロンプト.lnk"
goto end

:comp
compmgmt.msc
goto end

:desktop
"%SCRIPTS%\link\デスクトップの表示.scf"
goto end

:lock
"%SCRIPTS%\link\ワークステーションのロック.lnk"
goto end

:sys
"%SCRIPTS%\link\システム.lnk"
goto end

:net
"%SCRIPTS%\link\ネットワーク接続.lnk"
goto end

:fol
"%SCRIPTS%\link\フォルダ オプション.lnk"
goto end

:wall
"%SCRIPTS%\link\Windows ファイアウォール.lnk"
goto end

:app
"%SCRIPTS%\link\プログラムの追加と削除.lnk"
goto end

:power
"%SCRIPTS%\link\電源オプション.lnk"
goto end

:lang
"%SCRIPTS%\link\地域と言語のオプション.lnk"
goto end

:sound
"%SCRIPTS%\link\サウンドとオーディオ デバイス.lnk"
goto end

:keyboard
"%SCRIPTS%\link\キーボード.lnk"
goto end

:mouse
"%SCRIPTS%\link\マウス.lnk"
goto end

:screen
"%SCRIPTS%\link\画面.lnk"
goto end

:center
"%SCRIPTS%\link\セキュリティ センター.lnk"
goto end

:rman
"%SCRIPTS%\link\Rubyリファレンス.lnk"
"%SCRIPTS%\link\Railsドキュメント.lnk"
goto end

:slw
"%SCRIPTS%\link\SecureLockWare.LNK"
goto end

:vlc
"%SCRIPTS%\link\VLC media player.lnk"
goto end

:tc
"%SCRIPTS%\link\TrueCrypt.lnk"
goto end

:spybot
"%SCRIPTS%\link\Spybot - Search & Destroy.lnk"
goto end

:adaware
"%SCRIPTS%\link\Ad-Aware SE Personal.lnk"
goto end

:toroi
"%SCRIPTS%\link\ToRoI Buster 1.0.lnk"
goto end

:pikazip
"%SCRIPTS%\link\PikaZip.lnk"
goto end

:sakuraw
"%SCRIPTS%\link\桜時計.lnk"
goto end

:mayu
"%SCRIPTS%\link\窓使いの憂鬱.lnk"
goto end

:madonote
"%SCRIPTS%\link\窓の手.lnk"
goto end

:madotomo
"%SCRIPTS%\link\窓使いの友.lnk"
goto end

:ftp
"%SCRIPTS%\link\FFFTP.lnk"
goto end

:term
"%SCRIPTS%\link\Tera Term Pro.lnk"
goto end

:scp
"%SCRIPTS%\link\WinSCP.lnk"
goto end

:addcheck
"%SCRIPTS%\link\AddChecker.lnk"
goto end

:ntfs
"%SCRIPTS%\link\NTFS Disk Manager.lnk"
goto end

:memclr
"%SCRIPTS%\link\MemClr.lnk"
goto end

:itunes
"%SCRIPTS%\link\iTunes.lnk"
goto end

:lhauty
"%SCRIPTS%\link\LHAﾕｰﾃｨﾘﾃｨ32.lnk"
goto end

:lhaca
"%SCRIPTS%\link\+Lhaca.lnk"
goto end

:range
"%SCRIPTS%\link\解凍レンジ.lnk"
goto end

:binedit
"%SCRIPTS%\link\Stirling.lnk"
goto end

:ipmsg
"%SCRIPTS%\link\IPMSG for Win32.lnk"
goto end

:jmedit2
"%SCRIPTS%\link\JmEditor2.lnk"
goto end

:ezhtml
"%SCRIPTS%\link\ezHTML.lnk"
goto end

:giko
"%SCRIPTS%\link\ギコナビ.lnk"
goto end

:irc
"%SCRIPTS%\link\TakIRC.lnk"
goto end

:eclipse
rem "%SCRIPTS%\link\AI1E.lnk"
"%SCRIPTS%\link\EasyEclipse for LAMP.lnk"
goto end

:ftpd
"%SCRIPTS%\link\FTPdの起動.lnk"
goto end

:trac
"%SCRIPTS%\link\tracの起動.lnk"
goto end

:skype
"%SCRIPTS%\link\Skype.lnk"
goto end

:mixi
"%SCRIPTS%\link\mixi station.lnk"
goto end

:wire
"%SCRIPTS%\link\Wireshark.lnk"
goto end

:netact
"%SCRIPTS%\link\NetworkActiv PIAFCTM 1.5.lnk"
goto end

:neorage
"%SCRIPTS%\link\NeoRAGExp.lnk"
goto end

:clmouse
"%SCRIPTS%\link\マウス掃除機.lnk"
goto end

:clkey
"%SCRIPTS%\link\キーボード掃除機.lnk"
goto end

:proex
"%SCRIPTS%\link\ProcessExplorer.lnk"
goto end

:snes
"%SCRIPTS%\link\SNEShout.lnk"
goto end

:nes
"%SCRIPTS%\link\nester.lnk"
goto end

:oo
"%SCRIPTS%\link\OpenOffice.org.lnk"
goto end

:x2003
"%SCRIPTS%\link\Microsoft Office Excel 2003.lnk"
goto end

:w2003
"%SCRIPTS%\link\Microsoft Office Word 2003.lnk"
goto end

:p2003
"%SCRIPTS%\link\Microsoft Office PowerPoint 2003.lnk"
goto end

:a2003
"%SCRIPTS%\link\Microsoft Office Access 2003.lnk"
goto end

:v2003
"%SCRIPTS%\link\Microsoft Office Visio 2003.lnk"
goto end

:j2003
"%SCRIPTS%\link\Microsoft Office Project 2003.lnk"
goto end

:neorage
"%SCRIPTS%\link\NeoRAGExp.lnk"
goto end

:snes
"%SCRIPTS%\link\SNEShout.lnk"
goto end

:nes
"%SCRIPTS%\link\nester.lnk"
goto end

:x2003
"%SCRIPTS%\link\Microsoft Office Excel 2003.lnk"
goto end

:w2003
"%SCRIPTS%\link\Microsoft Office Word 2003.lnk"
goto end

:p2003
"%SCRIPTS%\link\Microsoft Office PowerPoint 2003.lnk"
goto end

:a2003
"%SCRIPTS%\link\Microsoft Office Access 2003.lnk"
goto end

:v2003
"%SCRIPTS%\link\Microsoft Office Visio 2003.lnk"
goto end

:j2003
"%SCRIPTS%\link\Microsoft Office Project 2003.lnk"
goto end

:pgadmin
"%SCRIPTS%\link\pgAdmin III.lnk"
goto end

:shell
"%SCRIPTS%\link\Windows PowerShell.lnk"
goto end

:msconfig
C:\WINDOWS\PCHealth\HelpCtr\Binaries\msconfig.exe
goto end

:ip
ipconfig /all
goto end

:native
if "%2"=="" goto usage
%2 %3 %4 %5 %6 %7 %8 %9
goto end

:cpl
if "%2"=="" goto usage
%2.cpl %3 %4 %5 %6 %7 %8 %9
goto end

:msc
if "%2"=="" goto usage
%2.msc %3 %4 %5 %6 %7 %8 %9
goto end

:usage
echo s - Start Command by 2006-2007 id774 <idnanashi@gmail.com>
echo usage: s [command]
echo.
echo Commands...[vim][ie][fx][tb][vix][susie][vnc][amp][ex][cmd]
echo [comp][desk][lock][sys][net][fol][wall][app][power][lang]
echo [sound][key][mouse][screen][center][rman][slw][vlc][tc]
echo [spybot][adaware][toroi][pikazip][sakuraw][mayu][madonote][madotomo]
echo [ftp][term][addcheck][ntfs][memclr][itunes][lhauty][lhaca][range][binedit]
echo [jmedit2][ezhtml][ipmsg][giko][irc][eclipse][ftpd][trac][skype][mixi]
echo [netact][wire][clmouse][clkey][neorage][snes][nes]
echo [oo][(xwpavj)2003][shell][pgadmin][msconfig][ip]
echo.
echo Native Command...[calc][charmap][clipbrd][control][explorer][magnify]
echo [mspaint][notepad][osk][regedit][sndrec32][sndvol32][taskmgr]
echo [utilman][write][wscript][wupdmgr]
echo.
echo CPL Command...[appwiz][desk][hdwwiz][inetcpl][intl][joy][main]
echo [mmsys][ncpa][powercfg][system][timedate]
echo.
echo MSC Command...[certmgr][ciadv][compmgmt][devmgmt]
echo [ntmsmgr][ntmsoprq][perfmon][wmimgmt]
echo.
:end
