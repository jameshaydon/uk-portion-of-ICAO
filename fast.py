def uk_portion_of_icao(uk, icao, adexp):
    if len(icao) == 0 or len(icao) % 2 != 1:
        return None

    waypoint_count = (len(icao) + 1) // 2
    bounds = adexp_uk_bounds(uk, adexp)
    if bounds is None:
        return None

    first_uk = bounds[0]
    last_uk = bounds[1]

    positions = unique_full_reconciliation_positions(icao, waypoint_count, adexp)
    if positions is None:
        return None

    window = uk_window_from_reconciliation(positions, waypoint_count, first_uk, last_uk)
    if window is None:
        return None

    left = window[0]
    right = window[1]
    return icao_token_indexes_for_waypoint_window(left, right)


def adexp_uk_bounds(uk, adexp):
    found = False
    first = 0
    last = 0
    index = 0
    while index < len(adexp):
        waypoint = adexp[index]
        is_uk = False
        if waypoint in uk:
            is_uk = uk[waypoint]
        if is_uk:
            if not found:
                first = index
                found = True
            last = index
        index += 1

    if not found:
        return None
    bounds = [0] * 2
    bounds[0] = first
    bounds[1] = last
    return bounds


def unique_full_reconciliation_positions(icao, waypoint_count, adexp):
    earliest = earliest_full_reconciliation_positions(icao, waypoint_count, adexp)
    if earliest is None:
        return None

    latest = latest_full_reconciliation_positions(icao, waypoint_count, adexp)
    if latest is None:
        return None

    if not integer_lists_equal(earliest, latest):
        return None

    return earliest


def earliest_full_reconciliation_positions(icao, waypoint_count, adexp):
    if waypoint_count == 0 or len(adexp) == 0:
        return None

    if waypoint_count == 1:
        if len(adexp) == 1 and icao[0] == adexp[0]:
            return [0]
        return None

    if len(adexp) < waypoint_count:
        return None

    last_waypoint = waypoint_count - 1
    last_adexp = len(adexp) - 1
    if icao[0] != adexp[0]:
        return None
    if icao[last_waypoint * 2] != adexp[last_adexp]:
        return None

    positions = [0] * waypoint_count
    positions[0] = 0
    positions[last_waypoint] = last_adexp

    adexp_index = 1
    waypoint_index = 1
    while waypoint_index < last_waypoint:
        target = icao[waypoint_index * 2]
        while adexp_index < last_adexp and adexp[adexp_index] != target:
            adexp_index += 1
        if adexp_index == last_adexp:
            return None
        positions[waypoint_index] = adexp_index
        adexp_index += 1
        waypoint_index += 1

    return positions


def latest_full_reconciliation_positions(icao, waypoint_count, adexp):
    if waypoint_count == 0 or len(adexp) == 0:
        return None

    if waypoint_count == 1:
        if len(adexp) == 1 and icao[0] == adexp[0]:
            return [0]
        return None

    if len(adexp) < waypoint_count:
        return None

    last_waypoint = waypoint_count - 1
    last_adexp = len(adexp) - 1
    if icao[0] != adexp[0]:
        return None
    if icao[last_waypoint * 2] != adexp[last_adexp]:
        return None

    positions = [0] * waypoint_count
    positions[0] = 0
    positions[last_waypoint] = last_adexp

    adexp_index = last_adexp - 1
    waypoint_index = last_waypoint - 1
    while waypoint_index > 0:
        target = icao[waypoint_index * 2]
        while adexp_index > 0 and adexp[adexp_index] != target:
            adexp_index -= 1
        if adexp_index == 0:
            return None
        positions[waypoint_index] = adexp_index
        adexp_index -= 1
        waypoint_index -= 1

    return positions


def uk_window_from_reconciliation(positions, waypoint_count, first_uk, last_uk):
    left = -1
    right = -1

    index = 0
    while index < waypoint_count:
        if positions[index] <= first_uk:
            left = index
        if right == -1 and positions[index] >= last_uk:
            right = index
        index += 1

    if left == -1 or right == -1 or left > right:
        return None
    window = [0] * 2
    window[0] = left
    window[1] = right
    return window


def integer_lists_equal(left, right):
    if len(left) != len(right):
        return False

    index = 0
    while index < len(left):
        if left[index] != right[index]:
            return False
        index += 1

    return True


def icao_token_indexes_for_waypoint_window(waypoint_start, waypoint_finish):
    indexes = [0] * 2
    indexes[0] = waypoint_start * 2
    indexes[1] = waypoint_finish * 2
    return indexes
