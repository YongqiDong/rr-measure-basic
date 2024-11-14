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

def get_per_round_average_qoe(dirpath: Path, alg: str) -> float:
    qoe = json.load(open(str(dirpath.joinpath("qoe.json"))))
    qoe = qoe[len(qoe)//5:]
    return np.average(qoe)

def get_per_round_qoe(dirpath: Path, alg: str):
    qoe = json.load(open(str(dirpath.joinpath("qoe.json"))))
    qoe = qoe[len(qoe)//5:]
    return qoe

color = {
    "emulation": "blue",
    "starlink": "green",
    "terrestrial": "red",
}

def average_qoe_by_target_latency(prefix: str):
    fig = plt.figure(figsize = figsize)
    result = defaultdict(list)
    ax = fig.add_subplot(1, 1, 1)
    
    for g in SCENARIO_SET:
        result = []
        for targetLatency in range(3, 7, 1):
            arrCMAB = []
            path = Path(prefix).joinpath(g).joinpath("target-{}s".format(str(targetLatency)))
            for dirpath, dirnames, files in os.walk(path):
                if len(files) != 0:
                    if "variable" not in dirpath:
                        continue
                    if CMAB in dirpath:
                        arrCMAB.append(get_per_round_average_qoe(Path(dirpath), CMAB))
            result.append(np.average(arrCMAB))
      
        ax.plot(result, marker='o', markersize=markersize, linewidth=LINEWIDTH, label=TITLE[g])

    plt.xticks(np.arange(4), np.arange(3, 7))
    plt.xlabel("Latency Target (seconds)")
    plt.ylabel("Average Reward")
    plt.ylim(1, 5)

    plt.legend(loc="best")
    plt.tight_layout()
    plt.savefig("../../results/streaming/average_reward" + ".png")
    plt.clf()
    plt.close()


if __name__ == "__main__":
    average_qoe_by_target_latency(DOWNLINK)
