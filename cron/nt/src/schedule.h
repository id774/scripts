/* _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
	schedule.h
		スケジュール用関数定義
 _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/ */
#if !defined(__schedule__h)
#define __schedule__h



// ___【include】__________________________________________________
#include <time.h>
#include <string>
#include <fstream>
#include <vector>
#include "CTask.h"
using namespace std;



// ___ 【define】__________________________________________________
#define CRONTAB_INI_FILE	"crontab.ini"	// INIファイル名
#define LOG_FILE			"crontab.log"	// ログファイル名
#define TASK_MAX			30				// 最大タスク数

#define ROW_SIZE			2046			// INIファイル内の一行の最大文字数
#define LOG_SIZE			2046			// LOGファイル内の一行の最大文字数
#define BUF_SIZE			256				// 指定できる範囲の最大文字数




// ___ 【function】__________________________________________________
extern BOOL LoadSchedule();
extern void CreateScheduleThread();
extern void StopScheduleThread();
extern void SuspendScheduleThread();
extern void ResumeScheduleThread();
extern void ObserverThread( void );
extern BOOL DoSystemCommand( const char * );
extern void WriteLog( const char * );
extern void WriteErrLog( const char *, DWORD dwErrorNumber = GetLastError() );
extern void MsgBox( const char * , const char *  );
extern void GetAppPath( string *path );
extern void trim( char *src );


#endif
