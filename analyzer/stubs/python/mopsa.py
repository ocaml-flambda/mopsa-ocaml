# Assertions
def assert_equal(x, y): pass
def assert_not_equal(x, y): pass
def massert(cond): pass
def assert_exists(cond): pass
def assert_safe(cond): pass
def assert_exception(exn): pass
def assert_exception_exists(exn): pass
def ignore_exception(exn): pass

# Ranges
def random_int(a, b): pass
def random_float(a, b): pass
def random_bool(): pass
def random_string(): pass

# Decorators
def stub(f): pass
def unsupported(f): pass
def builtin(f, name): pass

# Assertions used in the type analysis
def assert_list_of(l, ty): pass
def assert_set_of(l, ty): pass
def assert_dict_of(l, ty_k, ty_v): pass
