print("import_test import")
__all__ = ["subdir"]

y = "a"

from import_test.subdir import x as xsubdir

import import_test.othersubdir

from .blu import bluuu

from .yetanother import bla as imported_bla


from . import yetanother
# from import_test import yetanother
# import import_test.yetanother
