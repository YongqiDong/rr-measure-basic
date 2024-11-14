import os
import sys
import json
import numpy as np
from matplotlib import pyplot as plt
from pathlib import Path
from pprint import pprint
from dataclasses import dataclass
from collections import defaultdict
from datetime import datetime


LINEWIDTH=10
markersize=20
figsize=(14, 10)
fontsize=32
plt.rcParams.update({'font.size': fontsize})


EMULATION = "emulation"
STARLINK = "starlink"
TERRESTRIAL = "terrestrial"

SCENARIO_SET = [EMULATION, STARLINK, TERRESTRIAL]

TITLE = {
    EMULATION: "Emulation",
    STARLINK: "Starlink",
    TERRESTRIAL: "Terrestrial",
}

DOWNLINK = "../../data"

L2A = "abrL2A"
Dynamic = "abrDynamic"
LoLP = "abrLoLP"
CMAB = "abrCMAB"

FIGURE_DIR = "figures"


@dataclass
class AlgorithmStat:
    L2A: list[float]
    Dynamic: list[float]
    LoLP: list[float]


def get_per_round_bitrate_switch_from_bitrates(dirpath: Path) -> int:
    bitrates = json.load(open(str(dirpath.joinpath("bitrates.json"))))
    lst = list(bitrates.values())
    lst = lst[len(lst)//5:]
    return sum(x != y for x, y in zip(lst, lst[1:]))


def bitrate_switch_by_target_latency(prefix: str):
    for g in SCENARIO_SET:
        fig = plt.figure(figsize = figsize)

        result = defaultdict(list)

        ax = fig.add_subplot(1, 1, 1)
        for targetLatency in range(3, 7, 1):
            arrL2A = []
            arrDynamic = []
            arrLoLP = []
            arrCMAB = []

            path = Path(prefix).joinpath(g).joinpath("target-{}s".format(str(targetLatency)))
            for dirpath, dirnames, files in os.walk(path):
                if len(files) != 0:
                    if "variable" not in dirpath:
                        continue
                    if L2A in dirpath:
                        arrL2A.append(get_per_round_bitrate_switch_from_bitrates(Path(dirpath)))
                    elif Dynamic in dirpath:
                        arrDynamic.append(get_per_round_bitrate_switch_from_bitrates(Path(dirpath)))
                    elif LoLP in dirpath:
                        arrLoLP.append(get_per_round_bitrate_switch_from_bitrates(Path(dirpath)))
                    elif CMAB in dirpath:
                        arrCMAB.append(get_per_round_bitrate_switch_from_bitrates(Path(dirpath)))

            result[L2A].append(np.average(arrL2A))
            result[Dynamic].append(np.average(arrDynamic))
            result[LoLP].append(np.average(arrLoLP))
            result[CMAB].append(np.average(arrCMAB))

        ax.plot(result[L2A], marker='o', markersize=markersize, color='tab:blue', linewidth=LINEWIDTH, label="L2A-LL")
        if g == TERRESTRIAL:
            ax.plot(result[Dynamic], marker='o', linestyle="dotted", markersize=markersize, color='tab:orange', linewidth=LINEWIDTH, label="Dynamic")
        else:
            ax.plot(result[Dynamic], marker='o', markersize=markersize, color='tab:orange', linewidth=LINEWIDTH, label="Dynamic")
        ax.plot(result[LoLP], marker='o', markersize=markersize, color='tab:green', linewidth=LINEWIDTH, label="LoL+")
        ax.plot(result[CMAB], marker='o', markersize=markersize,color='tab:brown', linewidth=LINEWIDTH, label="CMAB")

        plt.xticks(np.arange(4), np.arange(3, 7))
        plt.xlabel("Latency Target (seconds)")
        plt.ylabel("Number of Bitrate Switches")
        plt.ylim(0, 160)
        plt.legend(loc="best")
        plt.tight_layout()
        plt.savefig("../../results/streaming/bitrate_switch_" + TITLE[g] + ".png")
        plt.clf()
        plt.close()


if __name__ == "__main__":
    bitrate_switch_by_target_latency(DOWNLINK)
