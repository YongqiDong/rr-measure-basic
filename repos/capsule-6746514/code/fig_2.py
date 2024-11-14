"Visualization of raw data(Figure 2)"
import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as plt

data = pd.read_csv('../data/Epileptic Seizure Recognition.csv')
M = data.values
X_data = M[:, 1:-1]
y_data = M[:, -1].astype(int)
print('start:', X_data.shape)

def autolabel(rects):
    for rect in rects:
        height = rect.get_height()
        plt.text(rect.get_x() + rect.get_width() / 2. - 0.2, 1.02 * height, '%s' % int(height))

c = []
raw_data = X_data
for j in range(len(raw_data)):
    new_feature = int(sum(raw_data[j])/len(raw_data[j]))
    c.append(new_feature)

C1 = np.argwhere(y_data == 1).flatten()
C2 = np.argwhere(y_data == 2).flatten()
C3 = np.argwhere(y_data == 3).flatten()
C4 = np.argwhere(y_data == 4).flatten()
C5 = np.argwhere(y_data == 5).flatten()

print('number of each class:', len(C5), len(C1), len(C2), len(C3), len(C4))

l1 = [len(C1), len(C2), len(C3), len(C4), len(C5)]
total_width, n = 0.8, 1
width = total_width / n
x1 = ['Class 1', 'Class 2', 'Class 3', 'Class 4', 'Class 5']
a = plt.bar(x1, l1, width=width)
autolabel(a)
plt.xlabel('Class label')
plt.ylabel('Count')
plt.grid(ls='--')
plt.savefig(os.path.join('../results/number_class.png'), dpi=600)
plt.show()

plt.subplot(511)
plt.plot(raw_data[C1[0]], 'r--')
plt.title('Class 1')
plt.grid(ls='--')
plt.subplot(512)
plt.plot(raw_data[C2[0]], 'b--')
plt.title('Class 2')
plt.grid(ls='--')
plt.subplot(513)
plt.plot(raw_data[C3[0]], 'b--')
plt.title('Class 3')
plt.ylabel('Amplitude(uV)')
plt.grid(ls='--')
plt.subplot(514)
plt.plot(raw_data[C4[0]], 'b--')
plt.title('Class 4')
plt.grid(ls='--')
plt.subplot(515)
plt.plot(raw_data[C5[0]], 'b--')
plt.title('Class 5')
plt.xlabel('Time')
plt.grid(ls='--')
plt.tight_layout()
plt.savefig(os.path.join('../results/Amplitude.png'), dpi=650)
plt.show()

