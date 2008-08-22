/* _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
	CTask.h
		タスク用クラス定義
 _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/ */
#if !defined(__ctask__h)
#define __ctask__h



// ___【include】__________________________________________________
#include "stdafx.h"
#include <time.h>
#include <string>
#include <set>
using namespace std;


// ___ 【CTask class】__________________________________________________
/*
	スケジュールを格納する、クラスです。
	このクラスの vector 配列を、static に保持します。
*/
class CTask
{
public:
	set<unsigned int> minute;		// 分
	set<unsigned int> hour;			// 時
	set<unsigned int> dom;			// 日
	set<unsigned int> month;		// 月
	set<unsigned int> dow;			// 曜日
	string m_cmd;					// 実行コマンド

	time_t m_time;					// 実行済みチェック用

public:

	/* -----------------------------------------------------------
		CTask コンストラクタ
	----------------------------------------------------------- */
	CTask() {
		m_cmd	= _T("");
		m_time	= NULL;
	}
	

	/* -----------------------------------------------------------
		INIファイルの書式から、各プロパティをセットさせるメソッド

		[書式]min hor dom mon dow cmd
		min: 分
		hor: 時間
		dom: 日
		mon: 月
		dow: 曜日
		cmd: 実行するコマンド（空白があってもよい）

		(例)
		*:			毎～
		1:			単体指定
		1,2,3:		複数指定
		1-10:		範囲指定
		5,10-15,23	重複指定									*/
	//	*/5:		間隔指定（ただし、0から起算する）
	/*
		(注記)
		・複数指定、または重複指定時に、同じ数値がある場合は、
		　エラーにはならずに、重複せずにスケジュール登録されます。
		・指定した範囲以下、以上の値を設定すると、エラーになります。
	----------------------------------------------------------- */
	BOOL SetFormatData( const char *buf){

		// 書式化された、一行から、各データ値を取得
		unsigned char mi[256], hr[256], dt[256], mt[256], wk[256], cm[2046];
		memset( mi,	0, 256 );
		memset( hr,	0, 256 );
		memset( dt,	0, 256 );
		memset( mt,	0, 256 );
		memset( wk,	0, 256 );
		memset( cm,	0, 2046 );
		if( sscanf( buf, "%s %s %s %s %s %[^\0]", mi, hr, dt, mt, wk, cm ) != 6 )
			return false;

		// 取得データの、不正チェックとセット
		if( get_data( (char*)mi, &minute,	0, 59 )<=0 ) return false;
		if( get_data( (char*)hr, &hour,		0, 24 )<=0 ) return false;
		if( get_data( (char*)dt, &dom,		1, 31 )<=0 ) return false;
		if( get_data( (char*)mt, &month,	1, 12 )<=0 ) return false;
		if( get_data( (char*)wk, &dow,		0,  6 )<=0 ) return false;
		m_cmd.append( (char*)cm );
		m_time = NULL;

		return true;
	}


	/* -----------------------------------------------------------
		データチェック／セット関数
	----------------------------------------------------------- */
	unsigned int get_data( const char *src, set<unsigned int> *lst, unsigned int min, unsigned int max ){

		unsigned int cnt;		// セットした個数（返り値）
		unsigned int val;		// セットする値の汎用変数
		if( *src=='*' ){
			unsigned int step = 1;
			if( *(src+1)=='/' ){
				if( ( sscanf( src, "*/%u", &step )!=1 ) || ( step<=0 ) ) return 0;
			}
			cnt = 0;
			for( val=min; val<=max; val+=step ){
				lst->insert( val );
				cnt++;
			}
		}else{
			char *p = NULL;
			unsigned int st, ls;
			cnt = 0;
			p = strtok( (char*)src, "," );
			while( p != NULL ){
				if( sscanf( p, "%d-%d", &st, &ls )==2 ){
					if( ls<min || ls>max || st<min || st>max || ls<=st ) return 0;
					for( val=st; val<=ls; val++ ){
						if( lst->find(val)==lst->end() ){
							lst->insert( val );
							cnt++;
						}
					}
				}else{
					val = cnv_to_ui( p );
					if( lst->find(val)==lst->end() ){
						lst->insert( val );
						cnt++;
					}
				}
				p = strtok( NULL, "," );
			}
		}
		return cnt;
	}


	/* -----------------------------------------------------------
		文字列→数値変換関数
	----------------------------------------------------------- */
	unsigned int cnv_to_ui( const char *str ){
		unsigned int i, val = 0;
		for( i=0; i<strlen(str); i++ ){
			if( str[i] >= 0x30 && str[i] <= 0x39 ){
				val *= 10;
				val += ( str[i] - 0x30 );
			}
		}
		return val;
	}


};


#endif
