class Md5sum:
    def __init__(self):
        pass

    def get_md5(self, path):
        try: import hashlib
        except: import md5
        try: m = hashlib.md5()
        except: m = md5.new()
        for f in open(path, 'rb'):
            m.update(f)
        return m.hexdigest()

