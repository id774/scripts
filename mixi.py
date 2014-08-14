#!/usr/bin/env python
# -*- coding: euc-jp -*-

"""
これは mixi (1) にアクセスし、操作をするための Python ライブラリです。
このライブラリを使用するためには、BeautifulSoup (2) をインストールしておく必要があります。

    1: http://mixi.jp/
    2: http://www.crummy.com/software/BeautifulSoup/

Author : OHTANI Hiroki       <hiro@liris.org>
       : SETOGUCHI Mitsuhiro <setomits@matatabi.homeip.net>
"""

import urllib
import re
import time
import warnings
import BeautifulSoup

############################
# Variables
############################
__version__ = '0.2.0'

WARNING = "Oh, we got mixi access restriction. We defer to access for a while.."
MIXI_BASE_URL = "http://mixi.jp"
CACHE_SEC = 600


############################
# Classes
############################
class LoginFailure(Exception):

    """
    ログインに失敗したときに送出される例外のクラスです。
    登録しているユーザ名かパスワードに誤りがあるときや、
    mixi サービスにエラーがあるときなどにログインに失敗することがあります。
    """

class MIXI:

    """
    MIXI クラス
    """

    ############################
    # Internal Class Methods
    ############################

    def __init__(self, proxy=None, err_retry_interval=5):
        """
        - proxy: プロキシサーバを使用して接続する場合は、
        プロキシサーバの URL を指定します。
        この値が None のとき（デフォルト）は、プロキシサーバを使用しません。
        - err_retry_interval: mixi のサーバがビジー状態であるなどで、
        アクセス制限を受けた際、指定した秒だけアクセスを控えます。
        """
        self.cookie = []
        self.cache = {}
        proxy_dict = None
        if proxy:
            proxy_dict = {"http": proxy}
        self.opener = urllib.FancyURLopener(proxy_dict)
        self.mixiid = None
        self.last_access = 0
        self.err_retry_interval = err_retry_interval

    def _get_soup(self, url, retry_cnt=0):
        now = time.time()
        u = "%s/%s" % (MIXI_BASE_URL, url)
        if self.cache.has_key(u) and self._fresh(self.cache[u], now):
            return self.cache[u][1]

        if retry_cnt:
            warnings.warn(WARNING)
            time.sleep(self.err_retry_interval)
        self.last_access = now
        try:
            response = self.opener.open(u)
            data = response.read().decode("euc-jp", "ignore").encode("euc-jp")
            response.close()
            soup = BeautifulSoup.BeautifulSoup(data)
            self._purge_cache(now)
            self.cache[u] = (now, soup)

            return soup
        except:
            if retry_cnt == 0:
                return self._get_soup(url, 1)
            else:
                return None

    def _fresh(self, (accessed, soup), now):
        if now - accessed < CACHE_SEC:
            return True
        else:
            return False

    def _purge_cache(self, now):
        for u in self.cache.keys():
            if self._fresh(self.cache[u], now):
                pass
            else:
                del self.cache[u]

    def _get_binary(self, url):
        response = self.opener.open(url)
        data = response.read()
        response.close()

        return data

    def _get_absurl(self, url):
        try:
            params = dict([kv.split("=")
                           for kv in url[url.find("?") + 1:].split("&")])
            if params.has_key("url"):
                return urllib.unquote(params["url"])
            return urllib.unquote("%s/%s" % (MIXI_BASE_URL, url))
        except:
            return url

    def _ymdhm2ptime(self, ymdhm):
        fmt = u"%Y年%m月%d日 %H:%M"
        return time.strptime(ymdhm.replace("&nbsp;", " "), fmt)

    def _mdhm2ptime(self, mdhm, backyear=0):
        ymdhm = u"%d年%s" % (time.localtime()[0] - backyear, mdhm)
        tt = self._ymdhm2ptime(ymdhm)
        if abs(time.time() - time.mktime(tt)) < 86400 * 30 \
                or tt == (0, 0, 0, 0, 0, 0, 0, 0, 0):
            return tt
        else:
            return self._mdhm2ptime(mdhm, backyear + 1)

    def _url_to_ids(self, url):
        pat = re.compile(u"id=(\d*)")

        return pat.findall(url)

    def _get_next_link(self, soup):
        ancs = soup.findAll("a")
        for anc in ancs:
            if anc.string == u"次を表示":
                return anc["href"]
        else:
            return None

    def _node_to_text(self, node):
        t = u""

        if node.name == "br":
            t += u"\n"
        for content in node.contents:
            if type(content) == BeautifulSoup.NavigableString:
                content = content.replace(u"&nbsp;", u" ")
                t += content.strip()
            else:
                t += self._node_to_text(content)

        return t

    def _get_name_and_num(self, s):
        n = s.rfind(u"(")
        name = s[:n - 2]
        num = int(s[n + 1:-1])

        return name, num

    def _get_list_friend(self, u, result=None):
        soup = self._get_soup(u)
        friendDivs = soup.findAll("div",
                                  {"class": re.compile("^iconState[0-9]*")})
        if not result:
            result = []

        for div in friendDivs:
            anc = div.find("a", href=re.compile(r"^show_friend.pl"))
            if not anc:
                continue
            item = {}
            item["link"] = self._get_absurl(anc["href"])
            item["id"] = self._url_to_ids(item["link"])[0]
            nameContent = div.span.string
            item["name"], item["num"] = self._get_name_and_num(nameContent)
            result.append(item)

        next_link = self._get_next_link(soup)
        if next_link:
            result = self._get_list_friend(next_link, result)

        return result

    def _get_diary_last_comment(self, soup):
        item = {}
        dls = soup.findAll("dl", {"class": "commentList01"})
        if not dls:
            return item

        lastDl = dls[-1]
        dt = lastDl.find("span", {"class": "commentTitleDate"}).string
        item["date"] = self._ymdhm2ptime(dt)

        anc = lastDl.a
        item["id"] = self._url_to_ids(anc["href"])[0]
        item["creator"] = anc.string

        dd = lastDl.dd
        item["content"] = self._node_to_text(dd)

        return item

    def _get_diary_entry(self, url, with_content, with_last_comment):
        """
        get the diary entry(content).
        url - url to get.
        """

        if not with_content:
            return u"", {}, u""

        if url.startswith(MIXI_BASE_URL):
            soup = self._get_soup(url[len(MIXI_BASE_URL) + 1:])
            if not soup:
                return [u"error"], {}

            node = soup.find("div", {"id": "diary_body"})
            entry = self._node_to_text(node)
            detail = u"".join([unicode(n) for n in node])

            if with_last_comment:
                last_comment = self._get_diary_last_comment(soup)
            else:
                last_comment = {}

            return entry, last_comment, detail
        else:
            return u"", {}, ""

    def _get_message_entry_and_date(self, url):
        soup = self._get_soup(url)
        td = soup.find("td", {"class": "h120"})

        return self._node_to_text(td), ""

    def _get_bbs_last_comment(self, url):
        soup = self._get_soup(url[len(MIXI_BASE_URL) + 1:])
        item = {}
        commentList = soup.find("dl", {"class": "commentList01"})
        if commentList:
            dls = commentList.findAll("dl", {"class": "commentContent01"})
            if not dls:
                return item
            dl = dls[-1]
            anc = dl.a
            item["id"] = self._url_to_ids(anc["href"])
            item["creator"] = anc.string
            item["content"] = self._node_to_text(dl.find("dd"))
        else:
            dls = soup.find("dl", {"class": "bbsList01 bbsDetail"})
            if dls:
                anc = dls.find("a")
                item["id"] = self._url_to_ids(anc["href"])
                item["creator"] = anc.string
                body = dls.find("dd").find("dd")
                item["content"] = self._node_to_text(body)

        return item

    ############################
    # Class Methods
    ############################
    def login(self, email, passwd):
        """
        mixi にログインするためのメソッド。
        ログインに失敗すると、 LoginFailure 例外が送出されます。

        - email: ログインするための E-mail アドレス
        - password: ログインするためのパスワード
        """
        params = urllib.urlencode({"email": email, "password": passwd,
                                   "next_url": "home.pl"})
        response = self.opener.open("%s/login.pl" % MIXI_BASE_URL, params)
        s = response.read()
        headers = response.headers
        if not response.headers.status and headers.getheaders("Set-Cookie"):
            self.cookie = response.headers.getheaders("Set-Cookie")
            cl = []
            for c in self.cookie:
                c = c[:c.find(";")]
                if c.find("BF_SESSION=") == 0:
                    l = len("BF_SESSION=")
                    self.mixiid = c[l:c.find("_", l)]
                cl.append(c)
            self.opener.addheader("Cookie", ";".join(cl))
            response.close()
        else:
            response.close()
            raise LoginFailure

    def new_friend_diary(self, maxcount=50,
                         with_content=False, with_last_comment=False):
        """
        マイミクの最新日記を取得するメソッド。
        成功するとエントリのリストが返ります。
        各エントリはマップの形式になっていて、
        date, link, id, title, creator, content, last_comment
        がキーとなっています。

        date はタプル、 link / id は文字列、
        title / creator / content / last_comment は Unicode
        となっています。

        - maxcount: いくつの最新日記を取得するか指定します。最大50。デフォルトは50。
        - with_content: 本文を取得するか否かを指定します。デフォルトは False 。
        - with_last_comment: 日記エントリの最後につけられたコメントを取得するか否かを
        指定します。デフォルトは False 。
        """
        _maxcount = min(maxcount, 50)
        u = "new_friend_diary.pl"

        soup = self._get_soup(u)
        entryList = soup.find("ul", {"class": "entryList01"}).findAll("li")
        result = []
        for li in entryList[:min(_maxcount, len(entryList))]:
            item = {}

            dt = li.dt
            item["date"] = self._ymdhm2ptime(dt.string)

            anc = li.a
            href = anc["href"]
            if not href.startswith("view_diary.pl") or "?url=http" in href:
                continue

            item["title"] = anc.string
            item["link"] = self._get_absurl(anc["href"])
            item["creator"] = anc.nextSibling.strip()[1:-1]
            item["id"] = self._url_to_ids(anc["href"])[-1]
            item["content"], item["last_comment"], item["content_detail"] = \
                self._get_diary_entry(item["link"],
                                      with_content, with_last_comment)

            result.append(item)

        return result

    def friend_diary(self, mixiid, maxcount=30,
                     with_content=False, with_last_comment=False):
        """
        日記を取得するメソッド。
        成功するとエントリのリストが返ります。
        各エントリはマップの形式になっていて、
        date, link, id, title, creator, content, last_comment
        がキーとなっています。

        date はタプル、link / id は文字列、
        title / creator / content / last_comment は Unicode 、
        となっています。

        - maxcount: いくつの最新日記を取得するか指定します。
        最大30。デフォルトは30。
        - with_content: 本文を取得するか否かを指定します。
        デフォルトは False 。
        - with_last_comment: 日記エントリの最後につけられたコメントを
        取得するか否かを指定します。
        デフォルトは False 。
        """
        _maxcount = min(maxcount, 30)
        u = "list_diary.pl?id=%s" % (mixiid)
        soup = self._get_soup(u)
        creator = soup.head.title.string[len("[mixi] "):]

        diaryTitles = soup.findAll("div", {"class": "listDiaryTitle"})
        if not diaryTitles:
            return []

        result = []
        for entry in diaryTitles[:_maxcount]:
            dt = entry.find("dd").string.replace("\n", " ").replace("\r", "")
            anc = entry.find("a")
            item = {}
            item["creator"] = creator
            item["date"] = self._ymdhm2ptime(dt)
            item["title"] = anc.string
            item["link"] = self._get_absurl(anc["href"])
            item["id"] = mixiid
            item["content"], item["last_comment"], item["content_detail"] = \
                self._get_diary_entry(item["link"],
                                      with_content, with_last_comment)

            result.append(item)

        return result

    def search_diary(self, maxcount=50, keyword=None):
        """
        mixi内での新着日記、あるいは何らかのキーワードで日記を検索するメソッド。
        成功するとエントリのリストが返ります。
        各エントリはマップの形式になっていて、
        date, link, id, title, creator, content, last_comment
        がキーとなっています。

        date はタプル、link / id は文字列、
        title / creator / content / last_comment は Unicode 、
        となっています。

        - maxcount: いくつの最新日記を取得するか指定します。
        最大50。デフォルトは50。
        - keyword: Unicodeで指定します。
        """
        if keyword:
            params = {'submit': 'search',
                      'keyword': keyword.encode('euc-jp', 'ignore')}
            u = "search_diary.pl?%s" % urllib.urlencode(params)
        else:
            u = "search_diary.pl"

        _maxcount = min(maxcount, 50)
        soup = self._get_soup(u)
        tables = soup.findAll("table", {"border": "0", "cellspacing": "1",
                                        "cellpadding": "4", "width": "550"})
        result = []
        for table in tables[:min(_maxcount, len(tables))]:
            item = {}
            anc = table.find("a")
            item["link"] = self._get_absurl(anc["href"])
            item["id"] = self._url_to_ids(item["link"])[1]
            tds = table.findAll("td", {"bgcolor": "#FFFFFF"})
            item["creator"] = self._node_to_text(tds[0]).strip()
            item["title"] = self._node_to_text(tds[1])
            item["summary"] = self._node_to_text(tds[2])
            item["date"] = self._mdhm2ptime(tds[3].string)
            result.append(item)

        return result

    def show_log(self):
        """
        足あとを取得するメソッド。
        成功するとユーザのリストが返ります。
        各ユーザはマップの形式になっていて、
        date, link, id, creator
        がキーとなっています。

        date はタプル、link / id は文字列、
        creator は Unicode 
        となっています。
        """
        u = "show_log.pl"

        soup = self._get_soup(u)
        td = soup.find("ul", {"class": "log new_log"})
        items = td.findAll("li")

        result = []
        for it in items:
            anc = it.find("a")
            item = {}
            item["date"] = self._ymdhm2ptime(anc.previous.strip())
            item["link"] = self._get_absurl(anc["href"])
            item["creator"] = anc.string
            item["id"] = self._url_to_ids(item["link"])[0]
            result.append(item)

        return result

    def list_friend(self, mixiid=None):
        """
        自分、あるいは指定したユーザのマイミクシィ一覧を取得するメソッド。
        成功するとユーザのリストが返ります。
        各ユーザはマップ形式になっていて、
        link, id, name, num
        がキーとなっています。

        link / id は文字列、name は Unicode、num は整数
        となっています。

        mixiid: マイミクシィ一覧を取得したいユーザの ID を文字列で指定します。
        None の場合（デフォルト）は自分のマイミクシィ一覧が返ります。
        """
        if not mixiid:
            mixiid = self.mixiid
        u = "list_friend.pl?id=%s" % (mixiid)

        return self._get_list_friend(u)

    def get_profile(self, mixiid=None):
        """
        自分、あるいは指定したユーザのプロフィールを取得するメソッド。
        成功するとマップ形式でプロフィールが返ります。
        キーは人によって異なります。
        """
        if not mixiid or mixiid == self.mixiid:
            u = "show_profile.pl?id=%s" % self.mixiid
        else:
            u = "show_friend.pl?id=%s" % mixiid

        soup = self._get_soup(u)
        dls = soup.find("div", {"id": "profile"})
        dls = dls.find("ul").findAll("dl")
        item = {}
        for dl in dls:
            try:
                key_node = dl.find("dt")
                val_node = dl.find("dd")
                key = "".join(self._node_to_text(key_node))
                val = self._node_to_text(val_node)
                item[key] = val
            except:
                pass

        return item

    def get_image(self, mixiid=None, thumbnail=False):
        """
        自分、あるいは指定したユーザの写真のファイル名と画像ファイルを取得するメソッド。
        thumbnail が True の場合はサムネイル画像となります。

        mixiid: 写真を取得したいユーザの ID を文字列で指定します。
        None の場合（デフォルト）は自分の写真となります。
        thumbnail: True の場合はサムネイル画像となります。デフォルトは False。
        """
        if not mixiid or mixiid == self.mixiid:
            u = "home.pl"
        else:
            u = "show_friend.pl?id=%s" % mixiid

        soup = self._get_soup(u)
        src = soup.find("div", {"class": "contents01"}).img["src"]

        if src.count("noimage_member"):
            return None, None
        else:
            if thumbnail:
                src = src[:-4] + "s.jpg"
            return src[src.rfind("/") + 1:], self._get_binary(src)

    def list_message(self):
        """
        受信箱内のメッセージを取得するメソッド。
        成功するとメッセージのリストが返ります。
        各メッセージはマップの形式になっていて、
        date, link, id, title, creator, content, last_comment
        がキーとなっています。

        date はタプル、link / id は文字列、
        title / creator / content / last_comment は Unicode
        となっています。
        """
        u = "list_message.pl"

        soup = self._get_soup(u)
        table = soup.find("table", {"border": "0", "cellspacing": "0",
                                    "cellpadding": "0", "width": "553"})
        trs = table.findAll("tr")
        result = []
        for tr in trs[2:]:
            tds = tr.findAll("td")
            if len(tds) == 1:
                continue

            item = {}
            item["creator"] = tds[2].string
            item["link"] = self._get_absurl(tds[3].a["href"])
            item["title"] = tds[3].a.string
            item["content"], item["date"] = \
                self._get_message_entry_and_date(tds[3].a["href"])

            result.append(item)

        return result

    def new_bbs(self, maxcount=50, with_last_comment=None):
        """
        コミュニティの最新書き込みを取得するメソッド。
        成功するとエントリのリストが返ります。
        各エントリはマップの形式になっていて、
        date, link, title, community, last_comment
        がキーとなっています。

        date はタプル、link は文字列、
        title / community / content / last_comment は Unicode
        となっています。
        """
        _maxcount = min(maxcount, 50)

        u = "new_bbs.pl"

        soup = self._get_soup(u)
        entryList = soup.find("ul", {"class": "entryList01"}).findAll("dl")

        result = []
        for dl in entryList[:min(_maxcount, len(entryList))]:
            item = {}
            td = dl.find("dt")
            item["date"] = self._ymdhm2ptime(td.string)

            anc = dl.find("a")
            item["title"] = anc.string
            item["link"] = self._get_absurl(anc["href"])
            item["community"] = anc.nextSibling.strip()[1:-1]
            if with_last_comment:
                item["last_comment"] = self._get_bbs_last_comment(item["link"])

            result.append(item)

        return result
