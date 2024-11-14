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


def get_per_round_stall_duration(dirpath: Path, alg: str) -> float:
    buffer = json.load(open(str(dirpath.joinpath("buffer.json"))))
    result = list()
    count = 0

    for _, v in buffer.items():
        count += 1
        if v == 0 and count > len(buffer) // 5:
            result.append(1)
    return np.sum(result) / (len(buffer)//5) * 100


def stall_duration_by_target_latency(prefix: str):
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
                        arrL2A.append(get_per_round_stall_duration(Path(dirpath), L2A))
                    elif Dynamic in dirpath:
                        arrDynamic.append(get_per_round_stall_duration(Path(dirpath), Dynamic))
                    elif LoLP in dirpath:
                        arrLoLP.append(get_per_round_stall_duration(Path(dirpath), LoLP))
                    elif CMAB in dirpath:
                        arrCMAB.append(get_per_round_stall_duration(Path(dirpath), CMAB))

            result[L2A].append(np.average(arrL2A))
            result[Dynamic].append(np.average(arrDynamic))
            result[LoLP].append(np.average(arrLoLP))
            result[CMAB].append(np.average(arrCMAB))

        ax.plot(result[L2A], marker='o', markersize=markersize, color='tab:blue', linewidth=LINEWIDTH, label="L2A-LL")
        ax.plot(result[Dynamic], marker='o', markersize=markersize, color='tab:orange', linewidth=LINEWIDTH, label="Dynamic")
        ax.plot(result[LoLP], marker='o', markersize=markersize, color='tab:green', linewidth=LINEWIDTH, label="LoL+")
        ax.plot(result[CMAB], marker='o', markersize=markersize, color='tab:brown', linewidth=LINEWIDTH, label="CMAB")

        plt.xticks(np.arange(4), np.arange(3, 7))
        plt.xlabel("Latency Target (seconds)")
        plt.ylabel("Rebuffering Time Ratio (%)")
        plt.ylim(0, 7)
        
        plt.legend(loc="best")
        plt.tight_layout()
        plt.savefig("../../results/streaming/average_rebuffering_time_percentage_" + TITLE[g] + ".png")
        plt.clf()
        plt.close()


if __name__ == "__main__":
    stall_duration_by_target_latency(DOWNLINK)
