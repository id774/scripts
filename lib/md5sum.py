class Md5sum:
    def __init__(self):
        pass

    def get_md5(self, path):
        import md5
        m = md5.new()
        for f in open(path, 'rb'):
            m.update(f)
        return m.hexdigest()

