from collections.abc import Sequence


def middle_portion(xs: Sequence[bool]) -> list[bool]:
    """Return the shortest middle after removing true-only ends.

    This is the constructive counterpart of the Lean spec:

        xs = before + middle + after

    where `before` and `after` contain only `True`, and `middle` has minimal
    length among all such decompositions.

    The optimal choice is obtained by taking the longest possible true prefix
    and the longest possible true suffix that remains. This scans from both
    ends and returns a copy of the middle slice.
    """

    left = 0
    right = len(xs)

    while left < right and xs[left] is True:
        left += 1

    while left < right and xs[right - 1] is True:
        right -= 1

    return list(xs[left:right])


if __name__ == "__main__":
    assert middle_portion([True]) == []
    assert middle_portion([True, False, True]) == [False]
    assert middle_portion([True, True, False, False, True]) == [False, False]
    assert middle_portion([False, True, False]) == [False, True, False]
    assert middle_portion([True, True]) == []
