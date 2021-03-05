print("import_test import")
__all__ = ["subdir"]

y = "a"

from import_test.subdir import x as xsubdir

import import_test.othersubdir
