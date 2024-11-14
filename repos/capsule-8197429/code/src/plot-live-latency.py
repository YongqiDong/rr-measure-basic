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
EXCEPTED = "excepted"

FIGURE_DIR = "figures"


@dataclass
class AlgorithmStat:
    L2A: list[float]
    Dynamic: list[float]
    LoLP: list[float]



def get_per_round_average_live_latency(dirpath: Path, alg: str) -> float:
    latency = json.load(open(str(dirpath.joinpath("playback_latency.json"))))
    result = list()
    for _, v in latency.items():
        result.append(v)
    result = result[len(result)//5:]
    return np.average(result)


def average_live_latency_by_target_latency(prefix: str):
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
                        arrL2A.append(get_per_round_average_live_latency(Path(dirpath), L2A))
                    elif Dynamic in dirpath:
                        arrDynamic.append(get_per_round_average_live_latency(Path(dirpath), Dynamic))
                    elif LoLP in dirpath:
                        arrLoLP.append(get_per_round_average_live_latency(Path(dirpath), LoLP))
                    elif CMAB in dirpath:
                        arrCMAB.append(get_per_round_average_live_latency(Path(dirpath), CMAB))

            result[L2A].append(np.average(arrL2A))
            result[Dynamic].append(np.average(arrDynamic))
            result[LoLP].append(np.average(arrLoLP))
            result[CMAB].append(np.average(arrCMAB))
            result[EXCEPTED].append(targetLatency)


        ax.plot(result[L2A], marker='o', markersize=markersize, color='tab:blue', linewidth=LINEWIDTH, label="L2A-LL")
        ax.plot(result[Dynamic], marker='o', markersize=markersize, color='tab:orange', linewidth=LINEWIDTH, label="Dynamic")
        ax.plot(result[LoLP], marker='o', markersize=markersize, color='tab:green', linewidth=LINEWIDTH, label="LoL+")
        ax.plot(result[CMAB], marker='o', markersize=markersize, color='tab:brown', linewidth=LINEWIDTH, label="CMAB")
        ax.plot(result[EXCEPTED], marker='o', markersize=markersize, color='tab:red', linewidth=LINEWIDTH, label="Expected")

        plt.xticks(np.arange(4), np.arange(3, 7))
        plt.xlabel("Latency Target (seconds)")
        plt.ylabel("Average Live Latency (seconds)")

        plt.ylim(3, 7)
        plt.legend(loc="best")
        plt.tight_layout()
        plt.savefig("../../results/streaming/average_live_latency_" + TITLE[g] + ".png")
        plt.clf()
        plt.close()


if __name__ == "__main__":
    average_live_latency_by_target_latency(DOWNLINK)
