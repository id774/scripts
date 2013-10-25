import imp

class PythonModuleVersion:
    def __init__(self):
        pass

    def get_module_version(self, module_name):
        try:
            imp.find_module(module_name)
            help(module_name)
        except ImportError:
            return module_name + ' ImportError'

