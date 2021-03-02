import mopsa

class ref:
    def __init__(self, r):
        self._r = r

    def __call__(self):
        if mopsa.random_bool():
            return self._r
        else:
            return None
