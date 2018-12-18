import mopsa

def flatten(lists):
    """
    Flattens a list of lists into a flat list
    """
    return [item for sublist in lists for item in sublist]


def flatten_dict(dicts, defaults):
    """
    Flattens a dict of lists, i.e., concatenates all lists for the same keys.
    """
    result = {}
    for key in defaults:
        result[key] = []
    for d in dicts:
        for key, value in d.items():
            a = [1,2,3,4,5,6]
            b = a[key]
            if key in result:
                result[key] = result[key] + [value] # FIXME: was: .extend(value)
            else:
                result[key] = value
    return result


def test_main():
    a = flatten([[1,2], [1,2], [True, False]])

    b = [{1:[2]}, {True: [True]}, {5: [1.2, 2]}]
    c = b[0][1]

    d = flatten_dict(b, [True, 1])
    mopsa.assert_list_of(a, int)
    mopsa.assert_list_of(b, dict)
    # mopsa.assert_list_of(c, float)
    mopsa.assert_dict_of(d, int, list)


# a := List[int]
# b := List[Dict[int, List[float]]]
# c := List[float]
# ENCORE FAUX sur d:
# d := Dict[int, List[float]]
# flatten := Callable[[List[List[int]]], List[int]]
# flatten_dict := Callable[[List[Dict[int, List[float]]], List[int]], Dict[int, List[float]]]
