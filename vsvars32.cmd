@SET VSINSTALLDIR=C:\vs2005
@SET VCINSTALLDIR=C:\vs2005\VC
@SET FrameworkDir=C:\WINDOWS\Microsoft.NET\Framework
@SET FrameworkVersion=v2.0.50727
@SET FrameworkSDKDir=C:\vs2005\SDK\v2.0
@if "%VSINSTALLDIR%"=="" goto error_no_VSINSTALLDIR
@if "%VCINSTALLDIR%"=="" goto error_no_VCINSTALLDIR

@echo Setting environment for using Microsoft Visual Studio 2005 x86 tools.

@rem
@rem Root of Visual Studio IDE installed files.
@rem
@set DevEnvDir=C:\vs2005\Common7\IDE

@set PATH=C:\vs2005\Common7\IDE;C:\vs2005\VC\BIN;C:\vs2005\Common7\Tools;C:\vs2005\SDK\v2.0\bin;C:\WINDOWS\Microsoft.NET\Framework\v2.0.50727;C:\vs2005\VC\VCPackages;%PATH%
@set INCLUDE=C:\vs2005\VC\INCLUDE;%INCLUDE%
@set LIB=C:\vs2005\VC\LIB;C:\vs2005\SDK\v2.0\lib;%LIB%
@set LIBPATH=C:\WINDOWS\Microsoft.NET\Framework\v2.0.50727

@goto end

:error_no_VSINSTALLDIR
@echo ERROR: VSINSTALLDIR variable is not set. 
@goto end

:error_no_VCINSTALLDIR
@echo ERROR: VCINSTALLDIR variable is not set. 
@goto end

:end
