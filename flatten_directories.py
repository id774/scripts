#!/usr/bin/env python

import os
import shutil
from optparse import OptionParser

# OptParserを使用してオプションを追加します。
parser = OptionParser()
parser.add_option("-m", "--move", action="store_true", dest="move_mode", default=False,
                  help="move files instead of copying them")
parser.add_option("-d", "--delete", action="store_true", dest="delete_mode", default=False,
                  help="delete empty directories")
parser.add_option("-q", "--quiet", action="store_true", dest="quiet_mode", default=False,
                  help="suppress operation info")
parser.add_option("-x", "--execute", action="store_true", dest="execute_mode", default=False,
                  help="execute file operations")
(options, args) = parser.parse_args()

def handle_directory(path):
    # ディレクトリ内のファイルとサブディレクトリを取得します。
    entries = os.listdir(path)

    # 各エントリに対して操作を行います。
    for entry in entries:
        old_path = os.path.join(path, entry)

        # エントリがディレクトリの場合は、再帰的に処理します。
        if os.path.isdir(old_path):
            handle_directory(old_path)
        else:
            # ファイルの場合、新しいファイル名を作成し、移動またはコピーします。
            new_filename = f"{path.replace('/', '_')}_{entry}"
            if options.move_mode:
                if options.execute_mode:
                    shutil.move(old_path, new_filename)
                if not options.quiet_mode:
                    print(f"Moved {old_path} -> {new_filename}")
            else:
                if options.execute_mode:
                    shutil.copy(old_path, new_filename)
                if not options.quiet_mode:
                    print(f"Copied {old_path} -> {new_filename}")

    # ディレクトリが空で、かつ削除オプションが有効であれば削除します。
    if options.delete_mode and not os.listdir(path):
        if options.execute_mode:
            os.rmdir(path)
        if not options.quiet_mode:
            print(f"Deleted directory {path}")

# 現在のディレクトリ内のすべてのサブディレクトリを取得し、処理を開始します。
subdirectories = [d for d in os.listdir('.') if os.path.isdir(d)]
for subdir in subdirectories:
    handle_directory(subdir)

