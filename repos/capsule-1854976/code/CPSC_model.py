# -*- coding: utf-8 -*-
"""
Created on  2021

@author: Liu

# milt_CPSC_model.py:深度学习网络模型

"""

import warnings
from keras.layers import Conv1D, BatchNormalization, Activation,  Dense, MaxPooling1D
from keras.layers import Dropout, Concatenate
from keras import regularizers
from keras.layers import LSTM, Bidirectional,Add
from keras_utils import AttentionWithContext
warnings.filterwarnings("ignore")



class Net(object):
    """
        结合CNN和RNN（双向LSTM）的深度学习网络模型
    """

    def __init__(self):
        pass



    @staticmethod
    def hunhe(inp, inp2, keep_prob, num_classes, C=0.001, initial='he_normal'):
        # 原始 核4

        #16
        net = Conv1D(4, 16, padding='same', kernel_initializer=initial, kernel_regularizer=regularizers.l2(C))(inp)
        net = BatchNormalization()(net)
        net = Activation('relu')(net)
        net = MaxPooling1D(3, 2)(net)


        #11
        net = Conv1D(8, 3, padding='same', kernel_initializer=initial, kernel_regularizer=regularizers.l2(C))(net)
        net = BatchNormalization()(net)
        net = Activation('relu')(net)
        net = MaxPooling1D(3, 2)(net)


        #7
        net = Conv1D(16, 3, padding='same', kernel_initializer=initial, kernel_regularizer=regularizers.l2(C))(net)
        net = BatchNormalization()(net)
        net = Activation('relu')(net)
        net = MaxPooling1D(3, 2)(net)


        #5
        net = Conv1D(32, 3, padding='same', kernel_initializer=initial, kernel_regularizer=regularizers.l2(C))(net)
        net = BatchNormalization()(net)
        net = Activation('relu')(net)
        net = MaxPooling1D(3, 2)(net)


        net = Conv1D(64, 3, padding='same', kernel_initializer=initial, kernel_regularizer=regularizers.l2(C))(net)
        net = BatchNormalization()(net)
        net = Activation('relu')(net)
        net = MaxPooling1D(3, 2)(net)


        net = Conv1D(128, 3, padding='same', kernel_initializer=initial, kernel_regularizer=regularizers.l2(C))(net)
        net = BatchNormalization()(net)
        net = Activation('relu')(net)
        net = MaxPooling1D(3, 2)(net)


        net = Conv1D(256, 3, padding='same', kernel_initializer=initial, kernel_regularizer=regularizers.l2(C))(net)
        net = BatchNormalization()(net)
        net = Activation('relu')(net)
        net = MaxPooling1D(3, 2)(net)


        features = Bidirectional(LSTM(1,return_sequences=True), merge_mode='concat')(net)
        features = AttentionWithContext()(features)


        if keep_prob==0:
            features = Concatenate(axis=-1)([features, inp2])
            features = Dropout(0.2)(features)
            features= Dense(units=16, activation='relu',kernel_regularizer=regularizers.l2(C))(features)
            net = Dense(units=num_classes, activation='sigmoid',name="classifier",kernel_regularizer=regularizers.l2(C))(features)
        else:
            print(keep_prob)
            features = Dropout(keep_prob)(features)
            features= Dense(units=128, activation='relu')(features)
            features = Dropout(keep_prob)(features)
            net = Dense(units=num_classes, activation='sigmoid')(features)
        return net, features

    @staticmethod
    def hunhe2(inp, keep_prob, num_classes, C=0.001, initial='he_normal'):
        # 原始 核4

        #16
        net = Conv1D(4, 16, padding='same', kernel_initializer=initial, kernel_regularizer=regularizers.l2(C))(inp)
        net = BatchNormalization()(net)
        net = Activation('relu')(net)
        # net = PReLU()(net)
        net = MaxPooling1D(3, 2)(net)


        #11
        net = Conv1D(8, 3, padding='same', kernel_initializer=initial, kernel_regularizer=regularizers.l2(C))(net)
        net = BatchNormalization()(net)
        net = Activation('relu')(net)
        # net = PReLU()(net)
        net = MaxPooling1D(3, 2)(net)


        #7
        net = Conv1D(16, 3, padding='same', kernel_initializer=initial, kernel_regularizer=regularizers.l2(C))(net)
        net = BatchNormalization()(net)
        net = Activation('relu')(net)
        # net = PReLU()(net)
        net = MaxPooling1D(3, 2)(net)


        #5
        net = Conv1D(32, 3, padding='same', kernel_initializer=initial, kernel_regularizer=regularizers.l2(C))(net)
        net = BatchNormalization()(net)
        net = Activation('relu')(net)
        # net = PReLU()(net)
        net = MaxPooling1D(3, 2)(net)


        net = Conv1D(64, 3, padding='same', kernel_initializer=initial, kernel_regularizer=regularizers.l2(C))(net)
        net = BatchNormalization()(net)
        net = Activation('relu')(net)
        # net = PReLU()(net)
        net = MaxPooling1D(3, 2)(net)


        net = Conv1D(128, 3, padding='same', kernel_initializer=initial, kernel_regularizer=regularizers.l2(C))(net)
        net = BatchNormalization()(net)
        net = Activation('relu')(net)
        # net = PReLU()(net)
        net = MaxPooling1D(3, 2)(net)


        net = Conv1D(256, 3, padding='same', kernel_initializer=initial, kernel_regularizer=regularizers.l2(C))(net)
        net = BatchNormalization()(net)
        net = Activation('relu')(net)
        # net = PReLU()(net)
        net = MaxPooling1D(3, 2)(net)



        features = Bidirectional(LSTM(8,return_sequences=True), merge_mode='concat')(net)
        # features = Flatten()(features)
        features = AttentionWithContext()(features)


        if keep_prob==0:
            print(keep_prob)
            features = Dropout(0.2)(features)
            features= Dense(units=16, activation='relu',kernel_regularizer=regularizers.l2(C))(features)
            net = Dense(units=num_classes, activation='sigmoid',name="classifier",kernel_regularizer=regularizers.l2(C))(features)
        else:
            print(keep_prob)
            features = Dropout(keep_prob)(features)
            features= Dense(units=128, activation='relu')(features)
            features = Dropout(keep_prob)(features)
            # features = Dense(units=64, activation='relu')(features)
            net = Dense(units=num_classes, activation='sigmoid')(features)

        return net, features

    @staticmethod
    def hunhe32( inp1,inp2, keep_prob, num_classes):
        # 原始 核4
        if keep_prob==0:
            features= Concatenate(axis=-1)([inp1,inp2])
            # ss= Reshape((-1,1))(features)
            # print("xxx",ss.shape)
            features = Dropout(0.2)(features)
            features = Dense(units=4,activation="relu")(features)
            # features = PReLU()(features)


            features2= Dense(units=4,activation="relu")(inp2)
            # features2 =PReLU()(features2)

            # features=Lambda(lambda x:x*0.5)(features)
            # features2 = Lambda(lambda x: x * 0.5)(features2)
            features = Add()([features,features2])
            # features = Concatenate(axis=-1)([features,inp2])

            net = Dense(units=num_classes, activation='sigmoid')(features)

        return net,  features

