import unittest
from itertools import product

from fast import (
    earliest_full_reconciliation_positions,
    latest_full_reconciliation_positions,
    uk_portion_of_icao,
    unique_full_reconciliation_positions,
)


def reference_reconciliation_positions(icao, adexp):
    if len(icao) == 0 or len(icao) % 2 != 1 or len(adexp) == 0:
        return []

    waypoints = []
    index = 0
    while index < len(icao):
        waypoints.append(icao[index])
        index += 2

    if len(waypoints) == 1:
        if len(adexp) == 1 and waypoints[0] == adexp[0]:
            return [[0]]
        return []

    if waypoints[0] != adexp[0] or waypoints[-1] != adexp[-1]:
        return []

    reconciliations = []

    def go(waypoint_index, adexp_start, positions):
        if waypoint_index == len(waypoints) - 1:
            reconciliations.append(positions + [len(adexp) - 1])
            return

        adexp_index = adexp_start
        while adexp_index < len(adexp) - 1:
            if adexp[adexp_index] == waypoints[waypoint_index]:
                go(waypoint_index + 1, adexp_index + 1, positions + [adexp_index])
            adexp_index += 1

    go(1, 1, [0])
    return reconciliations


def reference_uk_portion_of_icao(uk, icao, adexp):
    reconciliations = reference_reconciliation_positions(icao, adexp)
    if len(reconciliations) != 1:
        return None

    first_uk = -1
    last_uk = -1
    index = 0
    while index < len(adexp):
        if uk.get(adexp[index], False):
            if first_uk == -1:
                first_uk = index
            last_uk = index
        index += 1

    if first_uk == -1:
        return None

    positions = reconciliations[0]
    left = -1
    right = -1
    index = 0
    while index < len(positions):
        if positions[index] <= first_uk:
            left = index
        if right == -1 and positions[index] >= last_uk:
            right = index
        index += 1

    if left == -1 or right == -1 or left > right:
        return None

    return [left * 2, right * 2]


class UKPortionOfICAOTests(unittest.TestCase):
    def check_case(self, name, uk, icao, adexp, expected):
        actual = uk_portion_of_icao(uk, icao, adexp)
        self.assertEqual(actual, expected, name)

    def test_example_input_output_pairs(self):
        cases = [
            (
                "worked example with duplicate non-UK Q",
                {"T": True, "A": True, "O": True, "E": True, "X": True, "P": True, "W": True},
                ["F", "4", "Q", "2", "T", "8", "O", "5", "P", "1", "Y", "9", "U"],
                ["F", "S", "Q", "C", "T", "A", "O", "E", "X", "P", "W", "B", "Q", "Y", "U"],
                [4, 10],
            ),
            (
                "UK waypoint appears only in ADEXP between ICAO anchors",
                {"S": True},
                ["F", "4", "U"],
                ["F", "S", "U"],
                [0, 2],
            ),
            (
                "UK waypoint is exactly the first ICAO waypoint",
                {"F": True},
                ["F", "4", "U"],
                ["F", "S", "U"],
                [0, 0],
            ),
            (
                "UK waypoint is exactly the final ICAO waypoint",
                {"U": True},
                ["F", "4", "U"],
                ["F", "S", "U"],
                [2, 2],
            ),
            (
                "all ADEXP waypoints are UK so the whole ICAO plan is returned",
                {"F": True, "S": True, "U": True},
                ["F", "4", "U"],
                ["F", "S", "U"],
                [0, 2],
            ),
            (
                "single waypoint plan in UK",
                {"F": True},
                ["F"],
                ["F"],
                [0, 0],
            ),
            (
                "single waypoint plan outside UK",
                {"F": False},
                ["F"],
                ["F"],
                None,
            ),
            (
                "single waypoint plan rejects different ADEXP waypoint",
                {"F": True},
                ["F"],
                ["G"],
                None,
            ),
            (
                "single ICAO waypoint cannot reconcile with longer ADEXP stream",
                {"F": True},
                ["F"],
                ["F", "G"],
                None,
            ),
            (
                "unknown waypoints default to outside UK",
                {},
                ["F", "4", "U"],
                ["F", "S", "U"],
                None,
            ),
            (
                "false map entries are outside UK",
                {"S": False},
                ["F", "4", "U"],
                ["F", "S", "U"],
                None,
            ),
            (
                "middle ICAO waypoint is the only UK waypoint",
                {"B": True},
                ["A", "1", "B", "2", "C"],
                ["A", "X", "B", "Y", "C"],
                [2, 2],
            ),
            (
                "first ICAO leg has an implicit UK waypoint",
                {"X": True},
                ["A", "1", "B", "2", "C"],
                ["A", "X", "B", "Y", "C"],
                [0, 2],
            ),
            (
                "second ICAO leg has an implicit UK waypoint",
                {"Y": True},
                ["A", "1", "B", "2", "C"],
                ["A", "X", "B", "Y", "C"],
                [2, 4],
            ),
            (
                "UK span starts inside first leg and ends at final waypoint",
                {"X": True, "C": True},
                ["A", "1", "B", "2", "C"],
                ["A", "X", "B", "Y", "C"],
                [0, 4],
            ),
            (
                "UK span starts at first waypoint and ends inside final leg",
                {"A": True, "Y": True},
                ["A", "1", "B", "2", "C"],
                ["A", "X", "B", "Y", "C"],
                [0, 4],
            ),
            (
                "UK ADEXP stretch spans several ICAO legs",
                {"X": True, "Y": True, "C": True},
                ["A", "1", "B", "2", "C", "3", "D"],
                ["A", "X", "B", "Y", "C", "Z", "D"],
                [0, 4],
            ),
            (
                "separate UK islands are covered by one contiguous ICAO window",
                {"X": True, "Y": True},
                ["A", "1", "B", "2", "C", "3", "D"],
                ["A", "X", "B", "Z", "C", "Y", "D"],
                [0, 6],
            ),
            (
                "route token text can equal a waypoint identifier",
                {"C": True},
                ["A", "B", "C"],
                ["A", "X", "C"],
                [2, 2],
            ),
            (
                "direct adjacent waypoints reconcile without implicit ADEXP points",
                {"B": True},
                ["A", "1", "B"],
                ["A", "B"],
                [2, 2],
            ),
            (
                "duplicate waypoint after the forced middle waypoint is non-UK",
                {"C": True},
                ["A", "1", "B", "2", "C", "3", "D"],
                ["A", "B", "C", "B", "D"],
                [4, 4],
            ),
            (
                "duplicate waypoint before the forced middle waypoint is non-UK",
                {"C": True},
                ["A", "1", "B", "2", "C", "3", "D"],
                ["A", "B", "B", "C", "D"],
                None,
            ),
            (
                "repeated ICAO waypoint can reconcile uniquely",
                {"B": True},
                ["A", "1", "B", "2", "A"],
                ["A", "X", "B", "Y", "A"],
                [2, 2],
            ),
            (
                "ambiguous duplicate middle waypoint is rejected",
                {"B": True},
                ["A", "1", "B", "2", "C"],
                ["A", "B", "B", "C"],
                None,
            ),
            (
                "ambiguous duplicate outside-UK waypoint is still rejected",
                {"C": True},
                ["A", "1", "B", "2", "C"],
                ["A", "B", "B", "C"],
                None,
            ),
            (
                "duplicate UK identifier means every occurrence is UK",
                {"B": True},
                ["A", "1", "B", "2", "C", "3", "D"],
                ["A", "B", "C", "B", "D"],
                [2, 6],
            ),
            (
                "duplicate endpoint inside ADEXP does not make a two-waypoint plan ambiguous",
                {"X": True},
                ["A", "1", "A"],
                ["A", "X", "A"],
                [0, 2],
            ),
            (
                "duplicate start waypoint inside first leg makes UK id cover the leg",
                {"A": True},
                ["A", "1", "B"],
                ["A", "A", "B"],
                [0, 2],
            ),
            (
                "duplicate final waypoint inside final leg makes UK id cover the leg",
                {"B": True},
                ["A", "1", "B"],
                ["A", "B", "B"],
                [0, 2],
            ),
            (
                "duplicate non-ICAO waypoint does not make reconciliation ambiguous",
                {"X": True},
                ["A", "1", "B"],
                ["A", "X", "X", "B"],
                [0, 2],
            ),
            (
                "missing middle waypoint after a tempting out-of-order match is rejected",
                {"C": True},
                ["A", "1", "B", "2", "C"],
                ["A", "C", "B"],
                None,
            ),
            (
                "reversed middle waypoints cannot reconcile",
                {"B": True},
                ["A", "1", "B", "2", "C", "3", "D"],
                ["A", "C", "B", "D"],
                None,
            ),
            (
                "cannot reconcile when first waypoint differs",
                {"S": True},
                ["F", "4", "U"],
                ["X", "S", "U"],
                None,
            ),
            (
                "cannot reconcile when final waypoint differs",
                {"S": True},
                ["F", "4", "U"],
                ["F", "S", "X"],
                None,
            ),
            (
                "cannot reconcile when middle waypoint is missing",
                {"S": True},
                ["F", "4", "Q", "2", "U"],
                ["F", "S", "U"],
                None,
            ),
            (
                "cannot reconcile when ADEXP has trailing waypoint after ICAO final",
                {"S": True},
                ["F", "4", "U"],
                ["F", "S", "U", "X"],
                None,
            ),
            (
                "cannot reconcile when ADEXP has leading waypoint before ICAO first",
                {"S": True},
                ["F", "4", "U"],
                ["X", "F", "S", "U"],
                None,
            ),
            (
                "empty ICAO is malformed",
                {"F": True},
                [],
                ["F"],
                None,
            ),
            (
                "even-length ICAO token stream is malformed",
                {"B": True},
                ["A", "1", "B", "2"],
                ["A", "B"],
                None,
            ),
            (
                "empty ADEXP cannot reconcile",
                {"F": True},
                ["F"],
                [],
                None,
            ),
        ]

        for case in cases:
            self.check_case(case[0], case[1], case[2], case[3], case[4])

    def test_matches_reference_oracle_for_small_inputs(self):
        symbols = ["A", "B", "C"]
        uk_maps = []
        for flags in product([False, True], repeat=len(symbols)):
            uk = {}
            index = 0
            while index < len(symbols):
                if flags[index]:
                    uk[symbols[index]] = True
                index += 1
            uk_maps.append(uk)

        checked = 0
        for waypoint_count in range(1, 5):
            for waypoint_values in product(symbols, repeat=waypoint_count):
                icao = []
                index = 0
                while index < waypoint_count:
                    icao.append(waypoint_values[index])
                    if index + 1 < waypoint_count:
                        icao.append("R" + str(index))
                    index += 1

                for adexp_length in range(0, 6):
                    for adexp_values in product(symbols, repeat=adexp_length):
                        adexp = list(adexp_values)
                        for uk in uk_maps:
                            expected = reference_uk_portion_of_icao(uk, icao, adexp)
                            actual = uk_portion_of_icao(uk, icao, adexp)
                            checked += 1
                            if actual != expected:
                                self.fail(
                                    "mismatch for uk={!r}, icao={!r}, adexp={!r}: "
                                    "expected {!r}, got {!r}".format(
                                        uk, icao, adexp, expected, actual
                                    )
                                )

        self.assertEqual(checked, 349440)

    def test_malformed_icao_streams_are_rejected_before_reconciliation(self):
        cases = [
            [],
            ["A", "R"],
            ["A", "R", "B", "S"],
            ["A", "R", "B", "S", "C", "T"],
        ]

        for icao in cases:
            self.check_case(
                "malformed stream {!r}".format(icao),
                {"A": True, "B": True, "C": True},
                icao,
                ["A", "B", "C"],
                None,
            )

    def test_reconciliation_helpers_reject_short_adexp_duplicate_endpoints(self):
        icao = ["A", "DCT", "A"]
        adexp = ["A"]

        self.assertIsNone(
            earliest_full_reconciliation_positions(icao, 2, adexp)
        )
        self.assertIsNone(
            latest_full_reconciliation_positions(icao, 2, adexp)
        )
        self.assertIsNone(
            unique_full_reconciliation_positions(icao, 2, adexp)
        )


if __name__ == "__main__":
    unittest.main()
