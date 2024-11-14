"Traditional classifier performs 10-fold cross validation on Dataset 1"
from sklearn.svm import SVC
from sklearn.ensemble import RandomForestClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn import metrics
from xgboost import XGBClassifier
from warnings import simplefilter
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
    model = SVC(kernel='rbf')
    return model

def dt_model():
    model = DecisionTreeClassifier(max_depth=5)
    return model

def rf_model():
    model = RandomForestClassifier(n_estimators=10, max_depth=5)
    return model

if __name__=='__main__':
    from sklearn.model_selection import KFold
    from PCC_WFE import load_data
    data_path = '../data/Epileptic Seizure Recognition.csv'
    Raw_data, new_data = load_data(data_path)
    print(Raw_data.shape)

    New_sam = KFold(n_splits=10)

    ACC_0 = []
    Se_0 = []
    Pp_0 = []
    #XGBoost
    for train_index, test_index in New_sam.split(Raw_data):
        Sam_train, Sam_test = Raw_data[train_index], Raw_data[test_index]
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
    for train_index, test_index in New_sam.split(Raw_data):
        Sam_train, Sam_test = Raw_data[train_index], Raw_data[test_index]
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
    for train_index, test_index in New_sam.split(Raw_data):
        Sam_train, Sam_test = Raw_data[train_index], Raw_data[test_index]
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
    for train_index, test_index in New_sam.split(Raw_data):
        Sam_train, Sam_test = Raw_data[train_index], Raw_data[test_index]
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

        model_3 = rf_model()
        model_3.fit(x_train, y_train)

        predict_3 = model_3.predict(x_test)
        acc_3, rec_3, pp_3 = evaluate(y_test, predict_3)
        ACC_3.append(acc_3)
        Se_3.append(rec_3)
        Pp_3.append(pp_3)

    print('Acc-XGboost:', ACC_0)
    print('Acc-SVM:', ACC_1)
    print('Acc-DT:', ACC_2)
    print('Acc-RF:', ACC_3)

    print('Se-XGboost:', Se_0)
    print('Se-SVM:', Se_1)
    print('Se-DT:', Se_2)
    print('Se-RF:', Se_3)

    print('PP-XGboost:', Pp_0)
    print('PP-SVM:', Pp_1)
    print('PP-DT:', Pp_2)
    print('PP-RF:', Pp_3)

    print('XGboost:', sum(ACC_0)/len(ACC_0), sum(Se_0)/len(Se_0), sum(Pp_0)/len(Pp_0))
    print('SVM:', sum(ACC_1)/len(ACC_1), sum(Se_1)/len(Se_1), sum(Pp_1)/len(Pp_1))
    print('DT:', sum(ACC_2)/len(ACC_2), sum(Se_2)/len(Se_2), sum(Pp_2)/len(Pp_2))
    print('RF:', sum(ACC_3)/len(ACC_3), sum(Se_3)/len(Se_3), sum(Pp_3)/len(Pp_3))

