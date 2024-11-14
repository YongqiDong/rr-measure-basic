import json
import matplotlib.pyplot as plt
import numpy as np
import math

fontsize=23
plt.rcParams.update({'font.size': fontsize})

prefix = "together"
figsize=(12, 6)
legend_location="upper right"
marker = "."


def plot():
    iperf3_filename = "../../../data/throughput/downlink/no-isl-iperf3-2m-2023-09-21-06-10-00.json"
    irtt_filename = "../../../data/throughput/downlink/no-isl-irtt-10ms-2m-2023-09-21-06-10-00.json"
    
    isl_iperf3_filename = "../../../data/throughput/downlink/isl-iperf3-0.1s-2m-2023-09-21-06-10-00.json"
    isl_irtt_filename = "../../../data/throughput/downlink/isl-irtt-10ms-2m-2023-09-21-06-10-00.json"

    count = 0
    with open(irtt_filename, "r") as f:
        data = json.loads(f.read())
        rtt = []
        receive = []
        send = []

        for round in data["round_trips"]:
            count += 1
            if count % 2 != 0:
                continue
            if round["lost"] == "true":
                rtt.append(-1)
                receive.append(-100)
                send.append(-100)
            elif round["lost"] == "true_up":
                rtt.append(-100)
                send.append(-1)
                receive.append(-100)
            elif round["lost"] == "true_down":
                rtt.append(-100)
                receive.append(-1)
                send.append(-100)
            else:
                rtt.append(float(round["delay"]["rtt"])/1000000)
                receive.append(float(round["delay"]["receive"])/1000000)
                send.append(float(round["delay"]["send"])/1000000)

    with open(iperf3_filename, "r") as f:
        data = json.loads(f.read())
        mbps = []
        for round in data["intervals"]:
            sum = round["sum"]
            mbps.append(float(sum["bits_per_second"])/1000000)

    count = 0
    with open(isl_irtt_filename, "r") as f:
        data = json.loads(f.read())
        isl_rtt = []
        isl_receive = []
        isl_send = []

        for round in data["round_trips"]:
            count += 1
            if count % 2 != 0:
                continue
            if round["lost"] == "true":
                isl_rtt.append(-100)
                isl_receive.append(-100)
                isl_send.append(-100)
            elif round["lost"] == "true_up":
                isl_rtt.append(-100)
                isl_send.append(-100)
                isl_receive.append(-100)
            elif round["lost"] == "true_down":
                isl_rtt.append(-100)
                isl_receive.append(-100)
                isl_send.append(-100)
            else:
                isl_rtt.append(float(round["delay"]["rtt"])/1000000)
                isl_receive.append(float(round["delay"]["receive"])/1000000)
                isl_send.append(float(round["delay"]["send"])/1000000)

    with open(isl_iperf3_filename, "r") as f:
        data = json.loads(f.read())
        isl_mbps = []
        for round in data["intervals"]:
            sum = round["sum"]
            isl_mbps.append(float(sum["bits_per_second"])/1000000)

    length = len(rtt)

    fig = plt.figure(figsize=figsize)
    ax1 = fig.add_subplot(111)
    ax1.plot(isl_rtt[:len(isl_rtt)], marker, color="red", label="With ISL")
    ax1.plot(rtt[:len(rtt)], marker, color="blue", label="Without ISL")
    ax1.set_xlabel("Time (second)")
    ax1.set_ylabel("RTT (ms)")
    ax1.set_xlim(0, length)
    ax1.set_ylim(0, max(isl_rtt) * 1.1)
    plt.legend(loc=legend_location, labelcolor='linecolor')
    plt.tight_layout()
    
    labels = [item.get_text() for item in ax1.get_xticklabels()]
    for x in range(len(labels)):
        labels[x] = math.floor(int(labels[x]) / 100) * 2

    ax1.set_xticklabels(labels)

    plt.savefig("../../../results/throughput/{}-irtt.png".format(prefix))
    plt.close()

    fig = plt.figure(figsize=figsize)
    ax2 = fig.add_subplot(111)
    ax2.plot(isl_send[:len(isl_send)], marker, color="red", label="With ISL")
    ax2.plot(send[:len(send)], marker, color="blue", label="Without ISL")
    ax2.set_xlim(0, length)
    ax2.set_ylim(0, max(send))
    ax2.set_xlabel("Time (second)")
    ax2.set_ylabel("Latency (ms)")
    ax2.set_xticklabels(labels)
    plt.legend(loc=legend_location, labelcolor='linecolor')
    plt.tight_layout()
    plt.savefig("../../../results/throughput/{}-uplink-irtt.png".format(prefix))
    plt.close()


    fig = plt.figure(figsize=figsize)
    ax3 = fig.add_subplot(111)
    ax3.plot(isl_receive[:len(isl_receive)], marker, color="red", label="With ISL")
    ax3.plot(receive[:len(receive)], marker, color="blue", label="Without ISL")
    ax3.set_xlim(0, length)
    ax3.set_ylim(0, 200 * 1.1)
    ax3.set_xlabel("Time (second)")
    ax3.set_ylabel("Latency (ms)")
    ax3.set_xticklabels(labels)
    plt.legend(loc=legend_location, labelcolor='linecolor')
    plt.tight_layout()
    plt.savefig("../../../results/throughput/{}-downlink-irtt.png".format(prefix))
    plt.close()


    fig = plt.figure(figsize=figsize)
    ax4 = fig.add_subplot(111)
    ax4.plot(isl_mbps[:len(isl_mbps)], drawstyle="steps-post", color="red", label="With ISL")
    ax4.plot(mbps[:len(mbps)], drawstyle="steps-post", color="blue", label="Without ISL")
    ax4.set_xlabel("Time (second)")
    ax4.set_ylabel("Throughput (Mbps)")
    ax4.set_xlim(0, length/5)
    ax4.set_ylim(0, max(isl_mbps) * 1.1)
    ax4.set_xticklabels(labels)
    plt.legend(loc=legend_location, labelcolor='linecolor')
    plt.tight_layout()
    plt.savefig("../../../results/throughput/{}-downlink-iperf3.png".format(prefix))
    plt.close()

    fig = plt.figure(figsize =(5, 4))
    ax = fig.add_subplot(111)

    ax.ecdf(isl_mbps, label="With ISL", color="red")
    ax.ecdf(mbps, label="Without ISL", color="blue")

    plt.rcParams.update({'font.size': 20})

    plt.ylim(0, 1.1)
    plt.ylabel("CDF")
    plt.xlabel("Throughput (Mbps)")
    plt.tight_layout()
    plt.legend(labelcolor='linecolor', loc="lower right")
    plt.savefig("../../../results/throughput/downlink-throughput-cdf.png")
    plt.clf()
    plt.close()


if __name__ == "__main__":
    plot()
