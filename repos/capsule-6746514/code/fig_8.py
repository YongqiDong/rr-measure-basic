"Comparison between SGRU classifier and GRU classifier on Dataset 2"
import matplotlib.pyplot as plt
import numpy as np

names = ('GRU', 'SGRU')
subjects = ('ACC', 'Se', 'PPV')
scores = ((94.19,	81.13,	96.70), (97.43,	89.86,	97.93))

bar_width = 0.20
index = np.arange(len(scores[0]))
rects1 = plt.bar(index, scores[0], bar_width, color='r', label=names[0])
rects2 = plt.bar(index + bar_width, scores[1], bar_width, color='g', label=names[1])
plt.xticks(index + bar_width, subjects)

plt.ylabel('Score (%)')
plt.title('Dataset 2')
plt.grid(ls='--')
plt.legend(loc='upper center', bbox_to_anchor=(0.5, -0.05), fancybox=True, ncol=5)

def add_labels(rects):
    for rect in rects:
        height = rect.get_height()
        plt.text(rect.get_x() + rect.get_width() / 2, height, height, ha='center', va='bottom')
        rect.set_edgecolor('white')

add_labels(rects1)
add_labels(rects2)
plt.savefig('../results/zhu_2.png', dpi=650)
plt.show()