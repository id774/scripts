#!/usr/bin/env python
#
########################################################################
# Show python modules info and version
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 2/10,2014
#       Remove install process.
#  v1.1 6/29,2011
#       Add some packages.
#  v1.0 8/15,2008
#       Stable.
########################################################################

import imp

class PythonModuleInfo:
    def __init__(self):
        pass

    def get_module_info(self, module_name):
        try:
            imp.find_module(module_name)
            help(module_name)
        except ImportError:
            return module_name + ' ImportError'

def main():
    m = PythonModuleInfo()
    m.get_module_info('IPython')
    m.get_module_info('docutils')
    m.get_module_info('nose')
    m.get_module_info('simplejson')
    m.get_module_info('numpy')
    m.get_module_info('scipy')
    m.get_module_info('sklearn')
    m.get_module_info('matplotlib')
    m.get_module_info('pandas')
    m.get_module_info('patsy')
    m.get_module_info('statsmodels')
    m.get_module_info('seaborn')
    m.get_module_info('bokeh')
    m.get_module_info('twisted')
    m.get_module_info('flask')
    m.get_module_info('django')
    m.get_module_info('sqlalchemy')
    m.get_module_info('migrate')
    m.get_module_info('readline')
    m.get_module_info('pygments')
    m.get_module_info('babel')
    m.get_module_info('genshi')
    m.get_module_info('bottle')
    m.get_module_info('cherrypy')
    m.get_module_info('bs4')
    m.get_module_info('nltk')
    m.get_module_info('MeCab')
    m.get_module_info('CaboCha')

if __name__=='__main__':
    main()

