import matplotlib.pyplot as plt
from collections import defaultdict
from datetime import datetime


fontsize=20
plt.rcParams.update({'font.size': fontsize})


def plot_3():
    isl = "../../../data/latency/isl-ping-gw-lagos-230911-172603-0.1s-60min.txt"
    no_isl = "../../../data/latency/no-isl-ping-gw-seattle-230911-170535-0.1s-60min.txt"

    def plot(input, output):
        latency_at_second = {}
        latency_dict = defaultdict(list)
        with open(input, "r") as f:
            count = 0
            seq_list = []
            rtt_list = []
            timestamp = []
            for line in f.readlines():
                t, seq, rtt = line.strip("\n").split("\t")
                if float(rtt) == -1:
                    continue
                dt_object = datetime.fromtimestamp(float(t))
                timestamp.append(t)
                latency_dict[dt_object.second].append(float(rtt))
                seq_list.append(seq)
                rtt_list.append(rtt)

            for k, v in latency_dict.items():
                latency_at_second[k] = sum(v) / len(v)

            result = dict(sorted(latency_at_second.items()))
            xx = [x for x in result.keys()]
            values = [x for x in result.values()]

            fig = plt.figure(figsize =(5, 4))
            ax = fig.add_subplot(111)
            ax.bar(xx, values)
            # 12-27-42-57
            ax.axvline(x = 12, color = 'r', linestyle = '--', linewidth=1)
            ax.axvline(x = 27, color = 'r', linestyle = '--', linewidth=1)
            ax.axvline(x = 42, color = 'r', linestyle = '--', linewidth=1)
            ax.axvline(x = 57, color = 'r', linestyle = '--', linewidth=1)
            # ax.legend(loc="best", fontsize=legend_fontsize)
            ax.set_ylim(min(values) * 0.95, max(values)*1.05)
            plt.xlabel("Seconds", fontsize=fontsize)
            plt.ylabel("RTT (ms)", fontsize=fontsize)
            plt.tight_layout()
            plt.savefig("../../../results/latency/{}.png".format(output))
            plt.clf()
            plt.close()
    
    plot(isl, "isl")
    plot(no_isl, "no-isl")


if __name__ == "__main__":
    plot_3()
