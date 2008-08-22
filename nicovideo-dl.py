#!/usr/bin/python
# -*- coding: utf-8 -*-
#
# Copyright (c) 2006-2008 Ricardo Garcia Gonzalez
# Copyright (c) 2008 Ying-Chun Liu (PaulLiu)
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
# OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.
# 
# Except as contained in this notice, the name(s) of the above copyright
# holders shall not be used in advertising or otherwise to promote the
# sale, use or other dealings in this Software without prior written
# authorization.
#
import getpass
import httplib
import math
import netrc
import optparse
import os
import re
import socket
import string
import sys
import time
import urllib2
import cgi

# Global constants
const_1k = 1024
const_initial_block_size = 10 * const_1k
const_epsilon = 0.0001
const_timeout = 120

const_video_url_str = 'http://www.nicovideo.jp/watch/%s'
const_video_url_re = re.compile(r'^((?:http://)?(?:\w+\.)?nicovideo\.jp/(?:v/|(?:watch(?:\.php)?))?/(\w+))')
const_login_url_str = 'https://secure.nicovideo.jp/secure/login?site=niconico'
const_login_post_str = 'current_form=login&next_url=%%2Fwatch%%2F%s&mail=%s&password=%s&login_submit=Log+In'
const_url_url_param_re = re.compile(r"url[=](http[^&]*)")
const_video_url_info_str = 'http://www.nicovideo.jp/api/getflv?v=%s'
const_video_title_re = re.compile(r'<title>(.*)</title>', re.M | re.I)
const_video_type_re = re.compile(r'^http://.*\.nicovideo\.jp/smile\?(.*?)=.*')

# Print error message, followed by standard advice information, and then exit
def error_advice_exit(error_text):
	sys.stderr.write('Error: %s.\n' % error_text)
	sys.stderr.write('Try again several times. It may be a temporary problem.\n')
	sys.stderr.write('Other typical problems:\n\n')
	sys.stderr.write('* Video no longer exists.\n')
	sys.stderr.write('* You provided the account data, but it is not valid.\n')
	sys.stderr.write('* The connection was cut suddenly for some reason.\n')
	sys.stderr.write('* Your account is free and perhaps only usable at 02:00 to 19:00 (+0900).\n')
	sys.stderr.write('* Niconico changed their system, and the program no longer works.\n')
	sys.stderr.write('\nTry to confirm you are able to view the video using a web browser.\n')
	sys.stderr.write('Use the same video URL and account information, if needed, with this program.\n')
	sys.stderr.write('When using a proxy, make sure http_proxy has http://host:port format.\n')
	sys.stderr.write('Try again several times and contact me if the problem persists.\n')
	sys.exit('\n')

# Wrapper to create custom requests with typical headers
def request_create(url, extra_headers, post_data=None):
	retval = urllib2.Request(url)
	if post_data is not None:
		retval.add_data(post_data)
	# Try to mimic Firefox, at least a little bit
	retval.add_header('User-Agent', 'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.1.11) Gecko/20071127 Firefox/2.0.0.11')
	retval.add_header('Accept-Charset', 'ISO-8859-1,utf-8;q=0.7,*;q=0.7')
	retval.add_header('Accept', 'text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5')
	retval.add_header('Accept-Language', 'en-us,en;q=0.5')
	if extra_headers is not None:
		for header in extra_headers:
			retval.add_header(header[0], header[1])
	return retval

# Perform a request, process headers and return response
def perform_request(url, headers=None, data=None):
	request = request_create(url, headers, data)
	response = urllib2.urlopen(request)
	return response

# Conditional print
def cond_print(str):
	global cmdl_opts
	if not (cmdl_opts.quiet or cmdl_opts.get_url):
		sys.stdout.write(str)
		sys.stdout.flush()

# Title string normalization
def title_string_norm(title):
	title_p = u'窶舌ル繧ｳ繝九さ蜍慕判(SP1)'
	title_s = unicode(title.decode('utf-8', 'ignore'))
	if (title_s.endswith(title_p)):
		title_s = title_s[:title_s.rfind(title_p)]
	title_s = title_s.replace(os.sep, u'%')
	title_s = u'_'.join(title_s.split())
	title_s = title_s.encode('utf-8','ignore')
	return title_s

# Title string minimal transformation
def title_string_touch(title):
	return title.replace(os.sep, '%')

# Generic download step
def download_step(return_data_flag, step_title, step_error, url, post_data=None):
	try:
		cond_print('%s... ' % step_title)
		data = perform_request(url, data=post_data).read()
		cond_print('done.\n')
		if return_data_flag:
			return data
		return None

	except (urllib2.URLError, ValueError, httplib.HTTPException, TypeError, socket.error):
		cond_print('failed.\n')
		error_advice_exit(step_error)

	except KeyboardInterrupt:
		sys.exit('\n')

# Generic extract step
def extract_step(step_title, step_error, regexp, data):
	try:
		cond_print('%s... ' % step_title)
		match = regexp.search(data)
		
		if match is None:
			cond_print('failed.\n')
			error_advice_exit(step_error)
		
		extracted_data = match.group(1)
		cond_print('done.\n')
		return extracted_data
	
	except KeyboardInterrupt:
		sys.exit('\n')

# Calculate new block size based on previous block size
def new_block_size(before, after, bytes):
	new_min = max(bytes / 2.0, 1.0)
	new_max = max(bytes * 2.0, 1.0)
	dif = after - before
	if dif < const_epsilon:
		return int(new_max)
	rate = bytes / dif
	if rate > new_max:
		return int(new_max)
	if rate < new_min:
		return int(new_min)
	return int(rate)

# Get optimum 1k exponent to represent a number of bytes
def optimum_k_exp(num_bytes):
	global const_1k
	if num_bytes == 0:
		return 0
	return long(math.log(num_bytes, const_1k))

# Get optimum representation of number of bytes
def format_bytes(num_bytes):
	global const_1k
	try:
		exp = optimum_k_exp(num_bytes)
		suffix = 'bkMGTPEZY'[exp]
		if exp == 0:
			return '%s%s' % (num_bytes, suffix)
		converted = float(num_bytes) / float(const_1k**exp)
		return '%.2f%s' % (converted, suffix)
	except IndexError:
		sys.exit('Error: internal error formatting number of bytes.')

# Calculate ETA and return it in string format as MM:SS
def calc_eta(start, now, total, current):
	dif = now - start
	if current == 0 or dif < const_epsilon:
		return '--:--'
	rate = float(current) / dif
	eta = long((total - current) / rate)
	(eta_mins, eta_secs) = divmod(eta, 60)
	if eta_mins > 99:
		return '--:--'
	return '%02d:%02d' % (eta_mins, eta_secs)

# Calculate speed and return it in string format
def calc_speed(start, now, bytes):
	dif = now - start
	if bytes == 0 or dif < const_epsilon:
		return 'N/A b'
	return format_bytes(float(bytes) / dif)

# Create the command line options parser and parse command line
cmdl_usage = 'usage: %prog [options] video_url'
cmdl_version = '2008.04.05'
cmdl_parser = optparse.OptionParser(usage=cmdl_usage, version=cmdl_version, conflict_handler='resolve')
cmdl_parser.add_option('-h', '--help', action='help', help='print this help text and exit')
cmdl_parser.add_option('-v', '--version', action='version', help='print program version and exit')
cmdl_parser.add_option('-u', '--username', dest='username', metavar='USERNAME', help='account username')
cmdl_parser.add_option('-p', '--password', dest='password', metavar='PASSWORD', help='account password')
cmdl_parser.add_option('-o', '--output', dest='outfile', metavar='FILE', help='output video file name')
cmdl_parser.add_option('-q', '--quiet', action='store_true', dest='quiet', help='activates quiet mode')
cmdl_parser.add_option('-s', '--simulate', action='store_true', dest='simulate', help='do not download video')
cmdl_parser.add_option('-t', '--title', action='store_true', dest='use_title', help='use title in file name')
cmdl_parser.add_option('-l', '--literal', action='store_true', dest='use_literal', help='use literal title in file name')
cmdl_parser.add_option('-n', '--netrc', action='store_true', dest='use_netrc', help='use .netrc authentication data')
cmdl_parser.add_option('-g', '--get-url', action='store_true', dest='get_url', help='print final video URL only')
cmdl_parser.add_option('-2', '--title-too', action='store_true', dest='get_title', help='used with -g, print title too')
(cmdl_opts, cmdl_args) = cmdl_parser.parse_args()

# Set socket timeout
socket.setdefaulttimeout(const_timeout)

# Get video URL
if len(cmdl_args) != 1:
	cmdl_parser.print_help()
	sys.exit('\n')
video_url_cmdl = cmdl_args[0]

# Verify video URL format and convert to "standard" format
video_url_mo = const_video_url_re.match(video_url_cmdl)
if video_url_mo is None:
	sys.exit('Error: URL does not seem to be a niconico video URL. If it is, report a bug.')
video_url_id = video_url_mo.group(2)
video_url = const_video_url_str % video_url_id

video_extension = '.flv'

# Check conflicting options
if cmdl_opts.outfile is not None and (cmdl_opts.simulate or cmdl_opts.get_url):
	sys.stderr.write('Warning: video file name given but will not be used.\n')

if cmdl_opts.outfile is not None and (cmdl_opts.use_title or cmdl_opts.use_literal):
	sys.exit('Error: using the video title conflicts with using a given file name.')

if cmdl_opts.use_title and cmdl_opts.use_literal:
	sys.exit('Error: cannot use title and literal title at the same time.')

if cmdl_opts.quiet and cmdl_opts.get_url:
	sys.exit('Error: cannot be quiet and print final URL at the same time.')

# Incorrect option formatting
if cmdl_opts.username is None and cmdl_opts.password is not None:
	sys.exit('Error: password give but username is missing.')

if cmdl_opts.use_netrc and (cmdl_opts.username is not None or cmdl_opts.password is not None):
	sys.exit('Error: cannot use netrc and username/password at the same time.')

if cmdl_opts.get_url is None and cmdl_opts.get_title is not None:
	sys.exit('Error: getting title requires getting URL.')

# Get account information if any
#account_username = None
#account_password = None
account_username = 'xxxxxx@gmail.com'
account_password = 'xxxxxx' 

if cmdl_opts.use_netrc:
	try:
		info = netrc.netrc().authenticators('nicovideo')
		if info is None:
			sys.exit('Error: no authenticators for machine nicovideo.')
		netrc_username = info[0]
		netrc_password = info[2]
	except IOError:
		sys.exit('Error: unable to read .netrc file.')
	except netrc.NetrcParseError:
		sys.exit('Error: unable to parse .netrc file.')

if cmdl_opts.password is not None:
	account_username = cmdl_opts.username
	account_password = cmdl_opts.password
else:
	if cmdl_opts.username is not None and cmdl_opts.use_netrc:
		if cmdl_opts.username != netrc_username:
			sys.exit('Error: conflicting username from .netrc and command line options.')
		account_username = cmdl_opts.username
		account_password = netrc_password
	elif cmdl_opts.username is not None:
		account_username = cmdl_opts.username
		account_password = getpass.getpass('Type Niconico password and press return: ')
	elif cmdl_opts.use_netrc:
		if len(netrc_username) == 0:
			sys.exit('Error: empty username in .netrc file.')
		account_username = netrc_username
		account_password = netrc_password
	elif account_username is None:
		account_username = raw_input("Type Niconico E-mail account: ")
		account_password = getpass.getpass('Type Niconico password and press return: ')

# Install cookie and proxy handlers
urllib2.install_opener(urllib2.build_opener(urllib2.ProxyHandler()))
urllib2.install_opener(urllib2.build_opener(urllib2.HTTPCookieProcessor()))

# Log in
if account_username is not None:
	url = const_login_url_str
	post = const_login_post_str % (video_url_id, account_username, account_password)
	download_step(False, 'Logging in', 'unable to log in', url, post)

# Retrieve video webpage
video_webpage = download_step(True, 'Retrieving video webpage', 'unable to retrieve video webpage', video_url)

# Extract video title if needed
if cmdl_opts.use_title or cmdl_opts.use_literal or cmdl_opts.get_title:
	video_title = extract_step('Extracting video title', 'unable to extract video title', const_video_title_re, video_webpage)

# Extract needed video URL parameters
video_url_info = const_video_url_info_str % video_url_id
video_info_data = download_step(True, 'Retrieving info data', 'unable to retrieve video webpage', video_url_info)
cond_print('Extracting URL "url" parameter... ')
video_url_url_param = cgi.parse_qs(video_info_data)
if (video_url_url_param.has_key("url")):
	video_url_url_param=video_url_url_param["url"][0]
else:
	error_advice_exit('cannot extract url parameter')
cond_print('done.\n')
video_url_real = (video_url_url_param)

# Extract video type and modify video_extension
video_type_mo = const_video_type_re.match(video_url_real)
if (video_type_mo):
	if video_type_mo.group(1) == "s":
		video_extension = ".swf"
	elif video_type_mo.group(1) == "m":
		video_extension = ".mp4"

# Get output file name 
if cmdl_opts.outfile is None:
	video_filename = '%s%s' % (video_url_id, video_extension)
else:
	video_filename = cmdl_opts.outfile

# Rebuild filename if needed
if cmdl_opts.use_title or cmdl_opts.use_literal:
	if cmdl_opts.use_title:
		prefix = title_string_norm(video_title)
	else:
		prefix = title_string_touch(video_title)
	video_filename = '%s-%s%s' % (prefix, video_url_id, video_extension)

# Check name
if not video_filename.lower().endswith(video_extension):
	sys.stderr.write('Warning: video file name does not end in %s\n' % video_extension)

# Retrieve video data
try:
	cond_print('Requesting video file... ')
	video_data = perform_request(video_url_real)
	cond_print('done.\n')
	cond_print('Video data found at %s\n' % video_data.geturl())

	if cmdl_opts.get_title:
		print video_title

	if cmdl_opts.get_url:
		print video_data.geturl()

	if cmdl_opts.simulate or cmdl_opts.get_url:
		sys.exit()

	try:
		video_file = open(video_filename, 'wb')
	except (IOError, OSError):
		sys.exit('Error: unable to open "%s" for writing.' % video_filename)
	try:
		video_len = long(video_data.info()['Content-length'])
		video_len_str = format_bytes(video_len)
	except KeyError:
		video_len = None
		video_len_str = 'N/A'

	byte_counter = 0
	block_size = const_initial_block_size
	start_time = time.time()
	while True:
		if video_len is not None:
			percent = float(byte_counter) / float(video_len) * 100.0
			percent_str = '%.1f' % percent
			eta_str = calc_eta(start_time, time.time(), video_len, byte_counter)
		else:
			percent_str = '---.-'
			eta_str = '--:--'
		counter = format_bytes(byte_counter)
		speed_str = calc_speed(start_time, time.time(), byte_counter)
		cond_print('\rRetrieving video data: %5s%% (%8s of %s) at %8s/s ETA %s ' % (percent_str, counter, video_len_str, speed_str, eta_str))

		before = time.time()
		video_block = video_data.read(block_size)
		after = time.time()
		dl_bytes = len(video_block)
		if dl_bytes == 0:
			break
		byte_counter += dl_bytes
		video_file.write(video_block)
		block_size = new_block_size(before, after, dl_bytes)

	if video_len is not None and byte_counter != video_len:
		error_advice_exit('server did not send the expected amount of data')

	video_file.close()
	cond_print('done.\n')
	cond_print('Video data saved to %s\n' % video_filename)

except (urllib2.URLError, ValueError, httplib.HTTPException, TypeError, socket.error):
	cond_print('failed.\n')
	error_advice_exit('unable to download video data')

except KeyboardInterrupt:
	sys.exit('\n')

# Finish
sys.exit()
