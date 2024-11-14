"The realization of PCC-WFE Technique and Figure 3"
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from sklearn.preprocessing import MinMaxScaler
from numpy import delete
import os

def pc(x, y):
    a = 11500*sum(x*y)-sum(x)*sum(y)
    b = 11500*sum(x**2)-sum(x)**2
    c = 11500*sum(y**2)-sum(y)**2
    P = a/((b*c)**0.5)
    return P

def load_data(data_file):
    data = pd.read_csv(data_file)
    Sam = data.values
    X_data = Sam[:, 1:-1]
    y_data = Sam[:, -1]
    raw_data = X_data

    P = []
    for j in range(0, 178):
        X = raw_data[:, j]
        p = pc(X, y_data)
        P.append(p)

    C = []
    L = []
    for i in range(len(P)):
        if P[i] > 0:
            C.append(P[i])
            L.append(i)

    W = []
    for j in range(len(C)):
        w = C[j] / sum(C)
        W.append(w)

    New_fea = []
    for i in range(len(L)):
        new = X_data[:, L[i]] * W[i]
        New_fea.append(new)
    new_feature = sum(New_fea)

    #Figure 3
    #xx = []
    #for i in range(1, 11501):
    #    xx.append(i)
    #plt.scatter(xx, new_feature, c='g', marker='v', s=15)
    #plt.ylim([-600, 600])
    #plt.xlabel('Sample')
    #plt.ylabel('Amplitude (uV)')
    #plt.title('New Features', fontsize=15, verticalalignment='bottom')
    #plt.grid(ls='--')
    #plt.savefig(os.path.join('../results/new_features.png'), dpi=650)
    #plt.show()

    y_data = y_data.astype(int)
    new_data_1 = np.insert(raw_data, 178, values=new_feature, axis=1)
    new_Sam = np.insert(new_data_1, 179, values=y_data, axis=1)
    Sam_data = delete(Sam, 0, axis=1)

    scaler = MinMaxScaler(feature_range=(-1, 1))
    Sam_data[:, 0:-1] = scaler.fit_transform(X_data)
    new_Sam[:, 0:-1] = scaler.fit_transform(new_Sam[:, 0:-1])
    return Sam_data, new_Sam

if __name__=='__main__':
    Raw_data, new_data = load_data('../data/Epileptic Seizure Recognition.csv')
    print(Raw_data.shape)
    print('************')
    print(new_data.shape)
