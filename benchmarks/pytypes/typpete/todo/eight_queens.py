import mopsa

BOARD_SIZE = 8

class BailOut(Exception):
    pass

def validate(queens):
    left = right = col = queens[-1]
    for r in queens[:-1][::-1]:
        left, right = left-1, right+1
        if r in (left, col, right):
            raise BailOut

def add_queen(queens):
    for i in range(BOARD_SIZE):
        test_queens = queens + [i]
        try:
            validate(test_queens)
            if len(test_queens) == BOARD_SIZE:
                return test_queens
            else:
                # return add_queen(test_queens)
                return test_queens
        except BailOut:
            pass
    # cheating a bit
    return [1]
    # raise BailOut

def test_main():
    r_queens = add_queen([])
    print(r_queens)
    print("\n".join(". "*q + "Q " + ". "*(BOARD_SIZE-q-1) for q in r_queens))
    mopsa.assert_safe()
    mopsa.massert(isinstance(BOARD_SIZE, int))
    mopsa.assert_list_of(r_queens, int)

# To fix: original file does not work... the raise BailOut is never catched
# Also, there is a recursive function we don't handle yet

# BOARD_SIZE = 8

# # Original Typpete file is not valid in Python3, as BailOut should inherit from BaseException
# # class BailOut:
# #    pass
# class BailOut(Exception): pass


# def validate(q):
#     left = right = col = q[-1]
#     for r in list(reversed(q[:-1])):
#         left, right = left-1, right+1
#         if r in (left, col, right):
#             raise BailOut


# def add_queen(q):
#     for i in range(BOARD_SIZE):
#         test_queens = q + [i]
#         validate(test_queens)
#         if len(test_queens) == BOARD_SIZE:
#             return test_queens
#         else:
#             return add_queen(test_queens)
#     return []

# queens = add_queen([])
# print(queens)
# print("\n".join([". "*q + "Q " + ". "*(BOARD_SIZE-q-1) for q in queens]))


# # BOARD_SIZE := int
# # BailOut := Type[BailOut]
# # add_queen := Callable[[List[int]], List[int]]
# # queens := List[int]
# # validate := Callable[[List[int]], None]
