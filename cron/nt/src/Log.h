/* _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
	Log.h
		ログ出力用クラス定義
 _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/ */

ofstream log;
#define log_open(fname) ( log.open(fname,); );
#define log_close() (
log.open();


log_open(


// ___【include】__________________________________________________
#include "stdafx.h"
#include <time.h>
#include <string.h>
#include <fstream>
using namespace std;

fstream log
class CLog {
public:
	ofstream fp;

public:
	CLog( const char* nm ){
		fp.open( nm, ios::out | ios::app );
		if( !fp.is_open() ) return NULL;
		return fp;
	}
	~CLog(){
		fp.close();
	}
	CLog& operator<<(void* src){
		char tm_stamp[64];
		memset( tm_stamp, '\0', 64 );
		strftime( tm_stamp, 64, "%m/%d %H:%M:%S  >", localtime( time(NULL) ) );
		this << tm_stamp << src;
	}

};
