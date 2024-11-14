import numpy as np

np.random.seed(3)
import tensorflow as tf

tf.random.set_seed(3)
np.set_printoptions(precision=4)
from numpy import mean, sqrt, square, abs, median
import h5py as hp
import pywt
from sklearn import preprocessing
from sklearn.preprocessing import scale
from sklearn.model_selection import StratifiedKFold
from keras import optimizers
from keras.models import Model, load_model
from keras.layers import Input
from sklearn.metrics import confusion_matrix, f1_score, recall_score, precision_score
from CPSC_model import Net
from keras.callbacks import LearningRateScheduler, Callback
import matplotlib.pyplot as plt
from keras_utils import Capsule, AttentionWithContext, Self_Attention
from keras import backend as K
# import warnings
# warnings.filterwarnings("ignore")
import os


def del_files(path_file):
    ls = os.listdir(path_file)
    for i in ls:
        f_path = os.path.join(path_file, i)
        if os.path.isdir(f_path):
            del_files(f_path)
        else:
            os.remove(f_path)


def rms(data, axis=None):
    return sqrt(mean(square(data), axis))


def mean_absolute_deviation(data, axis=None):
    return mean(abs(data - mean(data, axis)), axis)


def median_absolute_deviation(data, axis=None):
    return median(abs(data - median(data, axis)), axis)


def Mean(data, axis=None):
    return mean(data, axis)


def load_mat(path_data, name_data, dtype='float32'):
    data = hp.File(path_data)
    arrays_d = {}
    for k, v in data.items():
        arrays_d[k] = np.array(v)
    dataArr = np.array(arrays_d[name_data], dtype=dtype)
    return dataArr


# 设定路径及文件名并载入
Path = '../data/'  # 自定义路径要正确
DataFile = 'Last_20_DataFhr.mat'
LabelFile = 'Last_20_DataLabel.mat'

print("Loading data and labels...")
Data = load_mat(Path + DataFile, 'Fhr')
Label = load_mat(Path + LabelFile, 'Label')

Label = np.argmax(Label, axis=1)
print("label:", Label.shape)

Data = Data.T
print("Data:", Data.shape)


# change different db
def DWT_Features(Data):
    Feature1 = []
    for i in range(len(Data)):
        mm = Data[i] - Mean(Data[i])
        coeffs = pywt.wavedec(mm, 'db6', level=5)
        Feature1.append(coeffs[0])
    Feature1 = np.array(Feature1)
    return Feature1


def Engineering_Feature(Data):
    Feature2 = []
    for i in range(len(Data)):
        first = mean_absolute_deviation(Data[i])
        second = median_absolute_deviation(Data[i])
        thrid = Mean(Data[i])
        four = rms(Data[i])
        m = [first, second, thrid, four]
        Feature2.append(m)
    Feature2 = np.array(Feature2)
    return Feature2


def Processing(Data):
    SIZE1 = 1
    SIZE2 = 1200
    train_x = Data.reshape([-1, SIZE1, SIZE2])
    train_x = train_x.transpose([0, 2, 1])
    print('Scaling data ...-----------------\n')
    for j in range(train_x.shape[0]):
        train_x[j, :, :] = scale(train_x[j, :, :], axis=0)
    return train_x


def smooth_labels(labels, factor=0.1):
    # smooth the labels
    labels *= (1 - factor)
    labels += (factor / 2.0)
    # returned the smoothed labels
    return labels


# 写一个LossHistory类，保存loss和acc
class LossHistory(Callback):
    def on_train_begin(self, logs={}):
        self.losses = {'batch': [], 'epoch': []}
        self.accuracy = {'batch': [], 'epoch': []}
        self.val_loss = {'batch': [], 'epoch': []}
        self.val_acc = {'batch': [], 'epoch': []}

    def on_batch_end(self, batch, logs={}):
        self.losses['batch'].append(logs.get('loss'))
        self.accuracy['batch'].append(logs.get('acc'))
        self.val_loss['batch'].append(logs.get('val_loss'))
        self.val_acc['batch'].append(logs.get('val_acc'))

    def on_epoch_end(self, batch, logs={}):
        self.losses['epoch'].append(logs.get('loss'))
        self.accuracy['epoch'].append(logs.get('acc'))
        self.val_loss['epoch'].append(logs.get('val_loss'))
        self.val_acc['epoch'].append(logs.get('val_acc'))

    def loss_plot(self, loss_type):
        iters = range(len(self.losses[loss_type]))
        plt.figure()
        # subplot loss
        # acc
        plt.plot(iters, self.val_acc[loss_type], 'b', label='val acc')
        plt.plot(iters, self.accuracy[loss_type], 'r', label='train acc')
        plt.xlabel('Epochs')
        plt.ylabel('Accuracy')
        plt.title('Accuracy  on Training and Validation Data')
        plt.grid(True)
        plt.legend(loc="lower right")
        plt.show()

        if loss_type == 'epoch':
            # val_acc
            plt.figure()
            plt.plot(iters, self.losses[loss_type], 'g', label='train loss')
            plt.plot(iters, self.val_loss[loss_type], 'k', label='val loss')
            plt.xlabel('Epochs')
            plt.ylabel('Loss')
            plt.title('Loss on Training and Validation Data')
            plt.grid(True)
            plt.legend(loc="upper right")
            plt.show()


import itertools


def plot_sonfusion_matrix(cm, classes, normalize=False, title='Confusion matrix', cmap=plt.cm.Blues):
    plt.title(title, fontsize=8)
    tick_marks = np.arange(len(classes))
    plt.xticks(tick_marks, classes)
    plt.yticks(tick_marks, classes)

    if normalize:
        cm = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]

    thresh = cm.max() / 2.0
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        plt.text(j, i, cm[i, j], horizontalalignment='center', color='white' if cm[i, j] > thresh else 'black')

    plt.tight_layout()
    plt.ylabel('True label', fontsize=8)
    plt.xlabel('Predict label', fontsize=8)
    plt.imshow(cm, interpolation='nearest', cmap=cmap)
    plt.colorbar(shrink=.75).ax.tick_params(labelsize=8)
    plt.savefig('s-m.png')
    plt.show()


def lr_schedule(epoch):
    # 训练网络时学习率衰减方案

    lr = 0.1
    if epoch >= 15 and epoch < 90:
        lr = 0.01
    if epoch >= 90:
        lr = 0.001
    print('Learning rate: ', lr)
    return lr


def lr_schedule2(epoch):
    # 训练网络时学习率衰减方案

    lr = 0.1
    if epoch >= 34 and epoch < 90:
        lr = 0.03
    if epoch >= 90:
        lr = 0.01
    print('Learning rate: ', lr)
    return lr


class Metrics(Callback):
    def __init__(self, filepath):
        self.file_path = filepath

    def on_train_begin(self, logs=None):
        self.val_f1s = []
        self.best_val_f1 = 0
        self.val_recalls = []
        self.val_precisions = []

    def on_epoch_end(self, epoch, logs=None):
        # val_predict = list(map(boolMap, self.model.predict([self.validation_data[0], self.validation_data[1]])))
        val_predict = (np.asarray(self.model.predict([self.validation_data[0], self.validation_data[1]]))).round()
        val_targ = self.validation_data[2]
        _val_f1 = f1_score(val_targ, val_predict)
        _val_recall = recall_score(val_targ, val_predict)
        _val_precision = precision_score(val_targ, val_predict)
        self.val_f1s.append(_val_f1)
        self.val_recalls.append(_val_recall)
        self.val_precisions.append(_val_precision)
        print(_val_f1, _val_precision, _val_recall)
        print("max f1")
        print(max(self.val_f1s))
        if _val_f1 > self.best_val_f1:
            # self.model.save_weights(self.file_path, overwrite=True)
            self.model.save(self.file_path, overwrite=True)
            self.best_val_f1 = _val_f1
            print("best f1: {}".format(self.best_val_f1))
        else:
            print("val f1: {}, but not the best f1".format(_val_f1))
        return


SIZE1 = 1
SIZE2 = 1200
batch_size = 32
epochs = 250
momentum = 0.9
keep_prob = 0
class_num = 1

# 设定训练参数，搭建模型进行训练 -------------------------------------------
kfold = StratifiedKFold(n_splits=10, shuffle=True, random_state=3)

cvsacc1 = []
cvssen1 = []
cvsspe1 = []

index = 1

for train, test in kfold.split(Data, Label):
    print(index)

    print("DATA[train]", Data[train].shape)
    print("DATA[test]", Data[test].shape)
    # 训练集
    dwt_train = DWT_Features(Data[train])
    eng_train = Engineering_Feature(Data[train])

    # 测试集
    dwt_test = DWT_Features(Data[test])
    eng_test = Engineering_Feature(Data[test])

    # 训练集
    train_f_pro = Processing(Data[train])
    # 测试集
    test_f_pro = Processing(Data[test])

    dwt_trainf = scale(dwt_train, axis=1)
    dwt_testf = scale(dwt_test, axis=1)

    s_scale = preprocessing.StandardScaler()
    eng_trainf = s_scale.fit_transform(eng_train)
    eng_testf = s_scale.transform(eng_test)

    train_1 = Label[train].astype('float').reshape(-1, 1)
    train_1 = smooth_labels(train_1, factor=0.2)
    train_label = train_1

    test_1 = Label[test].reshape(-1, 1)
    test_label = test_1

    model_name = 'net_lead_' + str(index) + '.hdf5'
    MODEL_PATH = '../code/Best/'

    # evaluate the model
    model = load_model(MODEL_PATH + model_name,
                       custom_objects={'Capsule': Capsule, "AttentionWithContext": AttentionWithContext,
                                       "Self_Attention": Self_Attention})

    pred_vt = model.predict([test_f_pro, dwt_testf], batch_size=batch_size, verbose=0)

    pred_v = np.round(pred_vt[:, :])
    true_v = Label[test]

    # 评估模型的性能 ---------------------------------------------------------------------------------------------
    Conf_Mat = confusion_matrix(true_v, pred_v)  # 利用专用函数得到混淆矩阵

    # plt.figure(figsize=(3, 3))
    # plot_sonfusion_matrix(Conf_Mat, classes=range(2))
    #
    # plt.figure(figsize=(3, 3))
    # plt.tight_layout()
    # plt.title('Precision/Recall Curve', fontsize=8)
    # plt.xlabel('Recall', fontsize=8)
    # plt.ylabel('Precision',fontsize=8)
    # precision, recall, thresholds = precision_recall_curve(true_v,pred_v)
    # plt.plot(precision, recall)
    # plt.savefig('p-r.png')
    # plt.show()

    Acc1 = (Conf_Mat[0][0] + Conf_Mat[1][1]) / (np.sum(Conf_Mat[0]) + np.sum(Conf_Mat[1]))
    specificity1 = Conf_Mat[0][0] / np.sum(Conf_Mat[0])
    sensitivity1 = Conf_Mat[1][1] / np.sum(Conf_Mat[1])

    print('\nAccuracy=%.2f%%' % (Acc1 * 100))
    print('Sensitivity=%.2f%%' % (sensitivity1 * 100))
    print('Specificity=%.2f%%' % (specificity1 * 100))
    print('\nConfusion Matrix:\n')

    print(Conf_Mat)
    print("======================================")
    cvsacc1.append(Acc1 * 100)
    cvssen1.append(sensitivity1 * 100)
    cvsspe1.append(specificity1 * 100)

    index = index + 1

    K.clear_session()

print("ACC1: %.2f%% (+/- %.2f%%)" % (np.mean(cvsacc1), np.std(cvsacc1)))
print("Sen1: %.2f%% (+/- %.2f%%)" % (np.mean(cvssen1), np.std(cvssen1)))
print("Spe1: %.2f%% (+/- %.2f%%)" % (np.mean(cvsspe1), np.std(cvsspe1)))
z = []
for i in range(len(cvssen1)):
    ss = cvssen1[i] * cvsspe1[i]
    ss = np.sqrt(ss)
    z.append(ss)
print("QI: %.2f%% (+/- %.2f%%)" % (np.mean(z), np.std(z)))

























