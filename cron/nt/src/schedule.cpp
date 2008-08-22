/* _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
	schedule.cpp
		スケジュール用関数
 _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/ */

// ___【include】__________________________________________________
#include "stdafx.h"
#include "schedule.h"


//___ 【static validate】__________________________________________________
vector <CTask> schedule;				// スケジュール配列
volatile BOOL thread_flg;				// スレッド終了用フラグ
static HANDLE hThread = NULL ;			// 監視用スレッドハンドラ
static DWORD threadID ;					// 監視用スレッドＩＤ
static string apppath = _T("");			// アプリケーションパス



//___ 【extern function】__________________________________________________
/* ====================================
	スケジュール読込
==================================== */
BOOL LoadSchedule(){

	//++ アプリケーションパスの取得
	GetAppPath( &apppath );

	//++ ファイル名の取得
	char fname[_MAX_PATH];
	memset( fname, '\0', _MAX_PATH );
	strcpy( fname, (LPTSTR)apppath.c_str() );
	strcat( fname, CRONTAB_INI_FILE );

	//++ ファイルオープン ++
	WriteLog( "┏━━ Start ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓" );
	WriteLog( "スケジュールファイル読込開始" );
    ifstream file;
	file.open( fname );
	if( !file.is_open() ){
		MsgBox( "スケジュールファイルが開けません。", "crontab" );
		WriteLog( "●Error!●スケジュールファイルが開けません。" );
		return false;
	}

	//++ 読み込み ++
    char buf[ROW_SIZE];
	int count = 0, row = 0;
	while( count < TASK_MAX ) {
		memset( buf, '\0', ROW_SIZE );
		file.getline( buf, ROW_SIZE );
		trim( buf );
		row++;
		if( strlen( buf ) != 0 && *buf!='#' ){

			CTask *lptask = new CTask;
			if( !(lptask->SetFormatData( buf )) ){
				char logbuf[BUF_SIZE];
				memset( logbuf, '\0', BUF_SIZE );
				sprintf( logbuf, "line %d: -> %s\nスケジュール定義が不正です", row, buf );
				MsgBox( logbuf, "crontab" );
				WriteLog( logbuf );
				file.close();
				delete lptask;
				return false;
			}
			schedule.push_back( *lptask );
			delete lptask;
			count++;
		}
		if( file.eof() ) break;
    }
	if( count<=0 ){
		MsgBox( "スケジュールが定義されていません。", "crontab" );
		WriteLog( "スケジュールが定義されていません。" );
		file.close();
		return false;
	}

    /* ファイルクローズ */
	file.close();

	WriteLog( "スケジュールファイル読込完了" );
	return true;
}



/* ====================================
	監視用スレッド生成
==================================== */
void CreateScheduleThread(){
	/* 監視用ループスレッドの作成 */
	if( hThread == NULL ){
		hThread = CreateThread( 0, 0, 
					(LPTHREAD_START_ROUTINE)ObserverThread,
									0, 0, &threadID );
		if( hThread == (HANDLE)NULL ){
			//++ スレッド作成失敗時
			DWORD dwErrorNumber = GetLastError();
			WriteErrLog( "監視スレッドを開始できませんでした。" );
			ExitThread( dwErrorNumber );
			//CloseHandle( hThread );
			hThread = NULL;
		}else{
			WriteLog( "監視スレッドを開始しました。" );
		}
	}

}



/* ====================================
	監視用スレッド停止
==================================== */
void StopScheduleThread(){
	/* 監視スレッド停止 */
	thread_flg = false;
	if( hThread ) {
		while( WaitForSingleObject( hThread, 0 ) == WAIT_TIMEOUT ){
			if( hThread )
			    CloseHandle( hThread );
			hThread = NULL;
		}
		WriteLog( "監視スレッドを停止しました。" );
	}
}



/* ====================================
	監視用スレッド一時停止
==================================== */
void SuspendScheduleThread(){
	/* 監視スレッド一時停止 */
	if( hThread ){
		SuspendThread( hThread );
		WriteLog( "監視スレッドを一時停止しました。" );
	}
}



/* ====================================
	監視用スレッド再開
==================================== */
void ResumeScheduleThread(){
	/* 監視スレッド再開 */
	if( hThread ){
		ResumeThread( hThread );
		WriteLog( "監視スレッドを再開しました。" );
	}
}



/* ====================================
	監視用モジュール
==================================== */
void ObserverThread()
{
	vector <CTask>::iterator p;		// スケジュール配列へのイテレーター

	// サービス停止時に、volatile フラグが False になるまでループさせる
	thread_flg = true;
	while( thread_flg ){
		time_t now = time(NULL);
		struct tm *pnow = localtime(&now);
		pnow->tm_sec = 0;
		now = mktime( pnow );
		int cnt = 0;
		for( p = schedule.begin(); p != schedule.end(); p++ ){
			cnt++;
			if( p->m_time != now ){
				if( ( p->minute.find( pnow->tm_min )!=p->minute.end() ) && 
					( p->hour.find( pnow->tm_hour )!=p->hour.end() ) && 
					( p->dom.find( pnow->tm_mday )!=p->dom.end() ) && 
					( p->month.find( pnow->tm_mon + 1 )!=p->month.end() ) && 
					( p->dow.find( pnow->tm_wday )!=p->dow.end() ) )
				{
					char buf[LOG_SIZE];
					memset( buf, '\0', LOG_SIZE );
					sprintf( buf, "スケジュール %d を実行しています。。", cnt );
					WriteLog( buf );
					if( DoSystemCommand( (char*)p->m_cmd.c_str() ) ){
						p->m_time = now;
					}
				}
			}
		}
		now = time(NULL);
		Sleep( ( 60 - ( localtime(&now)->tm_sec ) ) * 1000 );
	}
	WriteLog( "スレッドを開放しました。" );
	ExitThread( 0 );
}



/* ====================================
	新しいプロセスを起動させる関数
==================================== */
BOOL DoSystemCommand( const char *cmd )
{

	// STARTUPINFOR 構造体取得
	STARTUPINFO startUpInfo;
	PROCESS_INFORMATION procInfo;
	GetStartupInfo( &startUpInfo ) ;
        
	// プロセスを作成
	BOOL retval;
	char logbuf[LOG_SIZE];
	if( retval = CreateProcess( 0, (char*)cmd, 0, 0, FALSE, CREATE_NEW_CONSOLE, 
		0, 0, &startUpInfo, &procInfo ) ){

		//++ コマンド実行成功時
		memset( logbuf, '\0', LOG_SIZE );
		sprintf( logbuf, "ooooooo コマンドを実行しました。[>> %s ]", cmd );
		WriteLog( logbuf );

		// ハンドラを閉じる
		CloseHandle( procInfo.hThread );
		CloseHandle( procInfo.hProcess );

	}else{

		//++ コマンド実行失敗時
		memset( logbuf, '\0', LOG_SIZE );
		sprintf( logbuf, "コマンドの実行に失敗しました。[>> %s ]", cmd );
		WriteErrLog( logbuf );

	};
	return retval;
}



/* ====================================
	ログ出力関数
==================================== */
void WriteLog( const char *buf ){

	//++ ファイル名の取得
	char fname[_MAX_PATH];
	memset( fname, '\0', _MAX_PATH );
	strcpy( fname, (LPTSTR)apppath.c_str() );
	strcat( fname, LOG_FILE );

	//++ タイムスタンプの取得
	char tm_stamp[64];
	time_t now = time(NULL);
	struct tm *pnow = localtime( &now );
	memset( tm_stamp, '\0', 64 );
	strftime( tm_stamp, 64, "%m/%d %H:%M:%S  >", pnow );

	//++ ログ出力
    ofstream file;
	file.open( fname, ios::out | ios::app );
	if( !file.is_open() ) return;
	file << tm_stamp << buf << endl;
	file.close();
	
}


/* ====================================
	エラーログ出力関数
==================================== */
void WriteErrLog( const char *buf, DWORD dwErrorNumber ){
	char logbuf[LOG_SIZE];
	LPVOID lpMsgBuf;
	FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
			NULL, dwErrorNumber, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR) &lpMsgBuf, 0, NULL );

	memset( logbuf, '\0', LOG_SIZE );
	sprintf( logbuf, "●Error!● %s \nError Number:%d\nError String:%s\n", buf, dwErrorNumber, lpMsgBuf );
	WriteLog( logbuf );
}



/* ====================================
	メッセージボックス関数
==================================== */
void MsgBox( const char* Text, const char* Caption )
{
	MessageBox( NULL, Text, Caption, MB_OK | MB_ICONEXCLAMATION | MB_SERVICE_NOTIFICATION );
}


/* ====================================
	アプリケーションパスの取得
==================================== */
void GetAppPath( string *path )
{
	char module[_MAX_PATH];
	char drive[_MAX_DRIVE];
	char dir[_MAX_DIR];
	char fname[_MAX_FNAME];
	char ext[_MAX_EXT];

	// モジュールファイル名の取得
	memset( module, '\0', _MAX_PATH );
	GetModuleFileName( NULL, module, _MAX_PATH );

	// ファイル名の分割
	memset( drive, '\0', _MAX_DRIVE );
	memset( dir, '\0', _MAX_DIR );
	memset( fname, '\0', _MAX_FNAME );
	memset( ext, '\0', _MAX_EXT );
	_splitpath( module, drive, dir, fname, ext );

	// パスを返す
	path->assign( drive );
	path->append( dir );
}


/* ====================================
	文字列をトリムする
==================================== */
void trim( char *src )
{
	char *pl,*ps;

	// RTrim
	pl = src + strlen( src ) - 1;
	while( pl >= src && ( ( *pl>=0x09 && *pl<=0x0d ) || ( *pl==0x20 ) ) )
		*pl-- = '\0';

	// LTrim
	ps = src;
	while( ps <= pl && ( ( *ps>=0x09 && *ps<=0x0d ) || ( *ps==0x20 ) ) )
		*ps++;
	strcpy( src, ps );

}

