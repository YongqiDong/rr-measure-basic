"Comparison between SGRU classifier and GRU classifier on Dataset 1"

import matplotlib.pyplot as plt
import numpy as np

names = ('GRU', 'SGRU')
subjects = ('ACC', 'Se', 'PPV')
scores = ((92.76,	78.86,	96.45), (97.33,	88.89,	97.51))

bar_width = 0.20
index = np.arange(len(scores[0]))
rects1 = plt.bar(index, scores[0], bar_width, color='r', label=names[0])
rects2 = plt.bar(index + bar_width, scores[1], bar_width, color='g', label=names[1])
plt.xticks(index + bar_width, subjects)

plt.ylabel('Score (%)')
plt.title('Dataset 1')
plt.grid(ls='--')
plt.legend(loc='upper center', bbox_to_anchor=(0.5, -0.05), fancybox=True, ncol=5)

def add_labels(rects):
    for rect in rects:
        height = rect.get_height()
        plt.text(rect.get_x() + rect.get_width() / 2, height, height, ha='center', va='bottom')
        rect.set_edgecolor('white')

add_labels(rects1)
add_labels(rects2)
plt.savefig('../results/zhu_1.png', dpi=650)
plt.show()