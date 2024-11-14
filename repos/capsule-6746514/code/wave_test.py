"Classifier performs 10-fold cross validation on Wave"
from sklearn.svm import SVC
from sklearn.ensemble import RandomForestClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn import metrics
from xgboost import XGBClassifier
from warnings import simplefilter
from keras.layers import Input, Dropout, GRU, Dense, Flatten, add
from keras import backend as K
from keras.models import Model
from keras.optimizers import Adam
from perform import Performance

simplefilter(action='ignore', category=FutureWarning)

def evaluate(actual, predicted):
  acc = metrics.accuracy_score(actual, predicted)
  rec = metrics.recall_score(actual, predicted, average='macro')
  pp = metrics.precision_score(actual, predicted, average='macro')
  return acc, rec, pp

def xgboost_model():
    model = XGBClassifier(max_depth=5, learning_rate=0.1, n_estimators=10, silent=True)
    return model

def svm_model():
    model = SVC(C=1.0, kernel='rbf')
    return model

def dt_model():
    model = DecisionTreeClassifier(max_depth=5)
    return model

def rf_model():
    model = RandomForestClassifier(n_estimators=10, max_depth=5)
    return model

def mish(x):
    return x*K.tanh(K.softplus(x))

def sgru_model(number_features, output_node=1, af=mish):
  input_size = (number_features, 1)
  inputs = Input(shape=input_size)
  g1 = GRU(32, activation=af, dropout=0.2, return_sequences=True)(inputs)
  g2 = GRU(32, activation=af, dropout=0.2, return_sequences=True)(g1)
  g3 = GRU(32, activation=af, dropout=0.2, return_sequences=True)(g2)
  a1 = add([g1, g3])
  g4 = GRU(32, dropout=0.2, activation=af, return_sequences=True)(a1)
  a2 = add([g2, g4])
  f1 = Flatten()(a2)
  d1 = Dense(128, activation=af)(f1)
  d2 = Dropout(0.2)(d1)
  outputs = Dense(output_node, activation='sigmoid')(d2)
  model = Model(input=inputs, output=outputs)
  return model

def gru_model(number_features, output_node=1, af=mish):
  input_size = (number_features, 1)
  inputs = Input(shape=input_size)
  g1 = GRU(32, activation=af, dropout=0.2, kernel_initializer='he_normal', return_sequences=True)(inputs)
  g2 = GRU(32, activation=af, dropout=0.2, kernel_initializer='he_normal', return_sequences=True)(g1)
  g3 = GRU(32, activation=af, dropout=0.2, kernel_initializer='he_normal', return_sequences=True)(g2)
  g4 = GRU(32, activation=af, dropout=0.2)(g3)
  d1 = Dense(128, activation=af, kernel_initializer='he_normal')(g4)
  d2 = Dropout(0.2)(d1)
  outputs = Dense(output_node, activation='sigmoid', kernel_initializer='he_normal')(d2)
  model = Model(input=inputs, output=outputs)
  return model

if __name__=='__main__':
    from sklearn.model_selection import KFold
    import pandas as pd
    import pywt
    from sklearn.preprocessing import MinMaxScaler
    import numpy as np

    data = pd.read_csv('../data/Epileptic Seizure Recognition.csv')
    Sam = data.values
    X_data = Sam[:, 1:-1]
    y_data = Sam[:, -1]

    coeffs = pywt.wavedec(X_data, 'db1', level=2)
    scaler = MinMaxScaler(feature_range=(-1, 1))
    N_data = scaler.fit_transform(coeffs[2])
    New_data = np.insert(N_data, N_data.shape[1], values=y_data, axis=1)

    New_sam = KFold(n_splits=10)
    ACC_0 = []
    Se_0 = []
    Pp_0 = []
    #XGBoost
    for train_index, test_index in New_sam.split(New_data):
        Sam_train, Sam_test = New_data[train_index], New_data[test_index]
        X_train = Sam_train[:, 0:-1]
        Y_train = Sam_train[:, -1].astype(int)
        X_test = Sam_test[:, 0:-1]
        Y_test = Sam_test[:, -1].astype(int)
        Y_train[Y_train > 1] = 0
        Y_test[Y_test > 1] = 0

        y_train = Y_train
        y_test = Y_test
        x_train = X_train
        x_test = X_test

        model_0 = xgboost_model()
        model_0.fit(x_train, y_train)

        predict_0 = model_0.predict(x_test)
        acc_0, rec_0, pp_0 = evaluate(y_test, predict_0)
        ACC_0.append(acc_0)
        Se_0.append(rec_0)
        Pp_0.append(pp_0)

    ACC_1 = []
    Se_1 = []
    Pp_1 = []
    #SVM
    for train_index, test_index in New_sam.split(New_data):
        Sam_train, Sam_test = New_data[train_index], New_data[test_index]
        X_train = Sam_train[:, 0:-1]
        Y_train = Sam_train[:, -1].astype(int)
        X_test = Sam_test[:, 0:-1]
        Y_test = Sam_test[:, -1].astype(int)
        Y_train[Y_train > 1] = 0
        Y_test[Y_test > 1] = 0

        y_train = Y_train
        y_test = Y_test
        x_train = X_train
        x_test = X_test

        model_1 = svm_model()
        model_1.fit(x_train, y_train)

        predict_1 = model_1.predict(x_test)
        acc_1, rec_1, pp_1 = evaluate(y_test, predict_1)
        ACC_1.append(acc_1)
        Se_1.append(rec_1)
        Pp_1.append(pp_1)

    ACC_2 = []
    Se_2 = []
    Pp_2 = []
    #dt_model()
    for train_index, test_index in New_sam.split(New_data):
        Sam_train, Sam_test = New_data[train_index], New_data[test_index]
        X_train = Sam_train[:, 0:-1]
        Y_train = Sam_train[:, -1].astype(int)
        X_test = Sam_test[:, 0:-1]
        Y_test = Sam_test[:, -1].astype(int)
        Y_train[Y_train > 1] = 0
        Y_test[Y_test > 1] = 0

        y_train = Y_train
        y_test = Y_test
        x_train = X_train
        x_test = X_test

        print('X_test:', x_test.shape)
        print('Y_test:', y_test.shape)

        model_2 = dt_model()
        history_2 = model_2.fit(x_train, y_train)

        predict_2 = model_2.predict(x_test)
        acc_2, rec_2, pp_2 = evaluate(y_test, predict_2)
        ACC_2.append(acc_2)
        Se_2.append(rec_2)
        Pp_2.append(pp_2)

    ACC_3 = []
    Se_3 = []
    Pp_3 = []
    #rf_model()
    for train_index, test_index in New_sam.split(New_data):
        Sam_train, Sam_test = New_data[train_index], New_data[test_index]
        X_train = Sam_train[:, 0:-1]
        Y_train = Sam_train[:, -1].astype(int)
        X_test = Sam_test[:, 0:-1]
        Y_test = Sam_test[:, -1].astype(int)
        Y_train[Y_train > 1] = 0
        Y_test[Y_test > 1] = 0

        y_train = Y_train
        y_test = Y_test
        x_train = X_train
        x_test = X_test

        model_3 = rf_model()
        history_3 = model_3.fit(x_train, y_train)

        predict_3 = model_3.predict(x_test)
        acc_3, rec_3, pp_3 = evaluate(y_test, predict_3)
        ACC_3.append(acc_3)
        Se_3.append(rec_3)
        Pp_3.append(pp_3)

    #SGRU model
    ACC_4 = []
    Se_4 = []
    Pp_4 = []
    for train_index, test_index in New_sam.split(New_data):
        Sam_train, Sam_test = New_data[train_index], New_data[test_index]
        X_train = Sam_train[:, 0:-1]
        Y_train = Sam_train[:, -1].astype(int)
        X_test = Sam_test[:, 0:-1]
        Y_test = Sam_test[:, -1].astype(int)
        Y_train[Y_train > 1] = 0
        Y_test[Y_test > 1] = 0

        print(X_train.shape)
        print(Y_train.shape)
        print(X_test.shape)
        print(Y_test.shape)

        y_train = Y_train
        y_test = Y_test

        x_train = X_train.reshape((X_train.shape[0], X_train.shape[1], 1))
        x_test = X_test.reshape((X_test.shape[0], X_test.shape[1], 1))

        print('X_train:', x_train.shape, 'X_test:', x_test.shape)
        print('Y_train:', y_train.shape, 'Y_test:', y_test.shape)

        opt = Adam(lr=0.001)
        model_0 = sgru_model(number_features=89, output_node=1, af=mish)
        model_0.compile(loss='binary_crossentropy', optimizer=opt, metrics=['acc'])
        history_0 = model_0.fit(x_train, y_train, epochs=100, batch_size=32, validation_data=(x_test, y_test),
                                verbose=2)

        y_hat = model_0.predict(x_test, batch_size=32)

        labels = y_test
        scores = y_hat[:, 0]
        p = Performance(labels, scores)
        acc = p.accuracy()
        pre = p.presision()
        rec = p.recall()

        ACC_4.append(acc)
        Se_4.append(rec)
        Pp_4.append(pre)

    #GRU model
    ACC_5 = []
    Se_5 = []
    Pp_5 = []
    for train_index, test_index in New_sam.split(New_data):
        Sam_train, Sam_test = New_data[train_index], New_data[test_index]
        X_train = Sam_train[:, 0:-1]
        Y_train = Sam_train[:, -1].astype(int)
        X_test = Sam_test[:, 0:-1]
        Y_test = Sam_test[:, -1].astype(int)
        Y_train[Y_train > 1] = 0
        Y_test[Y_test > 1] = 0

        print(X_train.shape)
        print(Y_train.shape)
        print(X_test.shape)
        print(Y_test.shape)

        y_train = Y_train
        y_test = Y_test

        x_train = X_train.reshape((X_train.shape[0], X_train.shape[1], 1))
        x_test = X_test.reshape((X_test.shape[0], X_test.shape[1], 1))

        print('X_train:', x_train.shape, 'X_test:', x_test.shape)
        print('Y_train:', y_train.shape, 'Y_test:', y_test.shape)

        opt = Adam(lr=0.001)
        model_1 = gru_model(number_features=89, output_node=1, af=mish)
        model_1.compile(loss='binary_crossentropy', optimizer=opt, metrics=['acc'])
        history_1 = model_1.fit(x_train, y_train, epochs=100, batch_size=32, validation_data=(x_test, y_test), verbose=2)

        y_hat = model_1.predict(x_test, batch_size=32)

        labels = y_test
        scores = y_hat[:, 0]
        p = Performance(labels, scores)
        acc = p.accuracy()
        pre = p.presision()
        rec = p.recall()

        ACC_5.append(acc)
        Se_5.append(rec)
        Pp_5.append(pre)

    print('XGboost:', sum(ACC_0)/len(ACC_0), sum(Se_0)/len(Se_0), sum(Pp_0)/len(Pp_0))
    print('SVM:', sum(ACC_1)/len(ACC_1), sum(Se_1)/len(Se_1), sum(Pp_1)/len(Pp_1))
    print('DT:', sum(ACC_2)/len(ACC_2), sum(Se_2)/len(Se_2), sum(Pp_2)/len(Pp_2))
    print('RF:', sum(ACC_3)/len(ACC_3), sum(Se_3)/len(Se_3), sum(Pp_3)/len(Pp_3))
    print('SGRU:', sum(ACC_4)/len(ACC_4), sum(Se_4)/len(Se_4), sum(Pp_4)/len(Pp_4))
    print('GRU:', sum(ACC_5)/len(ACC_5), sum(Se_5)/len(Se_5), sum(Pp_5)/len(Pp_5))


