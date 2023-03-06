import argparse
import os

def rename_files(dir_path, num_digits):
    # ディレクトリ内のファイルをすべて取得する
    files = os.listdir(dir_path)

    # 各ファイル名を処理する
    for file_name in files:
        # ファイル名の数字部分を取得する
        name_parts = os.path.splitext(file_name)
        name_base = name_parts[0]
        name_ext = name_parts[1]
        num_part = ''
        for c in reversed(name_base):
            if not c.isdigit():
                break
            num_part = c + num_part
        if not num_part:
            continue

        # 数字部分を指定した桁数に合わせる
        if len(num_part) > num_digits:
            new_num_part = num_part[-num_digits:]
        else:
            new_num_part = num_part.zfill(num_digits)

        new_name_base = name_base[:-len(num_part)] + new_num_part

        # 新しいファイル名を作成し、リネームする
        new_file_name = new_name_base + name_ext
        old_file_path = os.path.join(dir_path, file_name)
        new_file_path = os.path.join(dir_path, new_file_name)
        os.rename(old_file_path, new_file_path)

        # リネームしたことを表示する
        print(f"{file_name} -> {new_file_name}")

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser = argparse.ArgumentParser(description='ファイル名の数字部分を一律でゼロパディングしてリネームするプログラム')
    parser.add_argument('dir_path', help='directory path')
    parser.add_argument('num_digits', type=int, help='number of digits for padding')
    args = parser.parse_args()

    if not os.path.isdir(args.dir_path):
        print(f"{args.dir_path} is not a valid directory.")
        exit()

    rename_files(args.dir_path, args.num_digits)
