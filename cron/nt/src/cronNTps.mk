
cronNTps.dll: dlldata.obj cronNT_p.obj cronNT_i.obj
	link /dll /out:cronNTps.dll /def:cronNTps.def /entry:DllMain dlldata.obj cronNT_p.obj cronNT_i.obj \
		kernel32.lib rpcndr.lib rpcns4.lib rpcrt4.lib oleaut32.lib uuid.lib \

.c.obj:
	cl /c /Ox /DWIN32 /D_WIN32_WINNT=0x0400 /DREGISTER_PROXY_DLL \
		$<

clean:
	@del cronNTps.dll
	@del cronNTps.lib
	@del cronNTps.exp
	@del dlldata.obj
	@del cronNT_p.obj
	@del cronNT_i.obj
