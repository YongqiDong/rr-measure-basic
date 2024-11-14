import matplotlib.pyplot as plt
from collections import defaultdict
from datetime import datetime


fontsize=15
plt.rcParams.update({'font.size': fontsize})


def plot_2():
    isl = "../../../data/latency/isl-ping-gw-lagos-230911-172603-0.1s-60min.txt"
    no_isl = "../../../data/latency/no-isl-ping-gw-seattle-230911-170535-0.1s-60min.txt"

    rtt_isl = []
    rtt_no_isl = []

    with open(isl, "r") as f:
        for line in f.readlines():
            t, seq, rtt = line.strip("\n").split("\t")
            if float(rtt) == -1:
                continue
            rtt_isl.append(float(rtt))

    with open(no_isl, "r") as f:
        for line in f.readlines():
            t, seq, rtt = line.strip("\n").split("\t")
            if float(rtt) == -1:
                continue
            rtt_no_isl.append(float(rtt))

    fig = plt.figure(figsize =(5, 4))
    ax = fig.add_subplot(111)

    ax.ecdf(rtt_isl, label="With ISL", color="red")
    ax.ecdf(rtt_no_isl, label="Without ISL", color="blue")

    plt.xlim(0, max(max(rtt_isl), max(rtt_no_isl)))
    plt.ylabel("CDF")
    plt.xlabel("RTT (ms)")
    plt.tight_layout()
    plt.legend(labelcolor='linecolor', loc="best")
    plt.savefig("../../../results/latency/ping-cdf.png")
    plt.clf()
    plt.close()

if __name__ == "__main__":
    plot_2()
