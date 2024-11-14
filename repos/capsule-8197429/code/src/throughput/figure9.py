import json
import matplotlib.pyplot as plt
import numpy as np


fontsize=20
plt.rcParams.update({'font.size': fontsize})

prefix = "together"
figsize=(12, 6)
legend_location="upper right"
marker = "."


def plot():
    iperf3_filename = "../../../data/throughput/uplink/no-isl-iperf3-5m-2023-09-21-08-00-00.json"
    irtt_filename = "../../../data/throughput/uplink/no-isl-irtt-10ms-5m-2023-09-21-08-00-00.json"
    
    isl_iperf3_filename = "../../../data/throughput/uplink/isl-iperf3-0.1s-5m-2023-09-21-08-00-00.json"
    isl_irtt_filename = "../../../data/throughput/uplink/isl-irtt-10ms-5m-2023-09-21-08-00-00.json"

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
                isl_rtt.append(-1)
                isl_receive.append(-100)
                isl_send.append(-100)
            elif round["lost"] == "true_up":
                isl_rtt.append(-100)
                isl_send.append(-1)
                isl_receive.append(-100)
            elif round["lost"] == "true_down":
                isl_rtt.append(-100)
                isl_receive.append(-1)
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

    fig = plt.figure(figsize =(5, 4))
    ax = fig.add_subplot(111)

    ax.ecdf(isl_mbps, label="With ISL", color="red")
    ax.ecdf(mbps, label="Without ISL", color="blue")

    plt.ylim(0, 1.1)
    plt.ylabel("CDF", fontsize=fontsize)
    plt.xlabel("Throughput (Mbps)", fontsize=fontsize)
    plt.tight_layout()
    plt.legend(labelcolor='linecolor', loc="lower right")
    plt.savefig("../../../results/throughput/uplink-throughput-cdf.png")
    plt.clf()
    plt.close()


if __name__ == "__main__":
    plot()
