from sklearn.decomposition import PCA
import pandas as pd
import numpy as np
from sklearn.preprocessing import MinMaxScaler

def load_data(data_file):
    data = pd.read_csv(data_file)
    Sam = data.values
    X_data = Sam[:, 1:-1]
    y_data = Sam[:, -1]
    pca = PCA(n_components=0.9)
    data = pca.fit_transform(X_data)
    scaler = MinMaxScaler(feature_range=(-1, 1))
    N_data = scaler.fit_transform(data)
    raw_data = np.insert(N_data, N_data.shape[1], values=y_data, axis=1)
    return raw_data

if __name__ == '__main__':
    newdata = load_data('../data/Epileptic Seizure Recognition.csv')
    print('************')
    print(newdata.shape)