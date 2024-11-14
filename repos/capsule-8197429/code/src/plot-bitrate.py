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

linestyle = "solid"
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


def get_per_round_average_bitrate(dirpath: Path, alg:str) -> float:
    bitrate = json.load(open(str(dirpath.joinpath("bitrate_by_second.json"))))
    bitrate = bitrate[len(bitrate)//5:]
    return np.average(bitrate)


def average_bitrate_by_target_latency(prefix: str):
    for g in SCENARIO_SET:
        fig = plt.figure(figsize = figsize)

        result_avg = defaultdict(list)

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
                        arrL2A.append(get_per_round_average_bitrate(Path(dirpath), L2A))
                    elif Dynamic in dirpath:
                        arrDynamic.append(get_per_round_average_bitrate(Path(dirpath), Dynamic))
                    elif LoLP in dirpath:
                        arrLoLP.append(get_per_round_average_bitrate(Path(dirpath), LoLP))
                    elif CMAB in dirpath:
                        arrCMAB.append(get_per_round_average_bitrate(Path(dirpath), CMAB))

            result_avg[L2A].append(np.average(arrL2A))
            result_avg[Dynamic].append(np.average(arrDynamic))
            result_avg[LoLP].append(np.average(arrLoLP))
            result_avg[CMAB].append(np.average(arrCMAB))

        ax.plot(result_avg[L2A], marker='o', linestyle=linestyle, markersize=markersize, color='tab:blue', linewidth=LINEWIDTH, label="L2A-LL")
        if g == TERRESTRIAL:
            ax.plot(result_avg[Dynamic], marker='o', linestyle="dotted",  markersize=markersize, color='tab:orange', linewidth=LINEWIDTH, label="Dynamic")
        else:
            ax.plot(result_avg[Dynamic], marker='o', linestyle=linestyle,  markersize=markersize, color='tab:orange', linewidth=LINEWIDTH, label="Dynamic")
        ax.plot(result_avg[LoLP], marker='o', linestyle=linestyle, markersize=markersize, color='tab:green', linewidth=LINEWIDTH, label="LoL+")
        ax.plot(result_avg[CMAB], marker='o', linestyle=linestyle, markersize=markersize, color='tab:brown', linewidth=LINEWIDTH, label="CMAB")


        plt.xticks(np.arange(4), np.arange(3, 7))
        plt.xlabel("Latency Target (seconds)")
        plt.ylabel("Average Bitrate (Kbps)")
        plt.ylim(0, 6500)

        plt.legend(loc="best")
        plt.tight_layout()
        plt.savefig("../../results/streaming/average_bitrate_" + TITLE[g] + ".png")
        plt.clf()
        plt.close()


def std_bitrate_by_target_latency(prefix: str):
    for g in SCENARIO_SET:
        fig = plt.figure(figsize = figsize)

        result_std = defaultdict(list)

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
                        arrL2A.append(get_per_round_average_bitrate(Path(dirpath), L2A))
                    elif Dynamic in dirpath:
                        arrDynamic.append(get_per_round_average_bitrate(Path(dirpath), Dynamic))
                    elif LoLP in dirpath:
                        arrLoLP.append(get_per_round_average_bitrate(Path(dirpath), LoLP))
                    elif CMAB in dirpath:
                        arrCMAB.append(get_per_round_average_bitrate(Path(dirpath), CMAB))

            result_std[L2A].append(np.std(arrL2A))
            result_std[Dynamic].append(np.std(arrDynamic))
            result_std[LoLP].append(np.std(arrLoLP))
            result_std[CMAB].append(np.std(arrCMAB))

       
        ax.plot(result_std[L2A], marker='o', linestyle=linestyle, markersize=markersize, color='tab:blue', linewidth=LINEWIDTH, label="L2A-LL")
        ax.plot(result_std[Dynamic], marker='o', linestyle=linestyle, markersize=markersize, color='tab:orange', linewidth=LINEWIDTH, label="Dynamic")
        ax.plot(result_std[LoLP], marker='o', linestyle=linestyle, markersize=markersize, color='tab:green', linewidth=LINEWIDTH, label="LoL+")
        ax.plot(result_std[CMAB], marker='o', linestyle=linestyle, markersize=markersize, color='tab:brown', linewidth=LINEWIDTH, label="CMAB")


        plt.xticks(np.arange(4), np.arange(3, 7))
        plt.xlabel("Latency Target (seconds)")
        plt.ylabel("Bitrate Standard Deviation (Kbps)")
        plt.ylim(0, 1500)
        
        plt.legend(loc="best")
        plt.tight_layout()
        plt.savefig("../../results/streaming/std_bitrate_" + TITLE[g] + ".png")
        plt.clf()
        plt.close()


if __name__ == "__main__":
    average_bitrate_by_target_latency(DOWNLINK)
    std_bitrate_by_target_latency(DOWNLINK)
