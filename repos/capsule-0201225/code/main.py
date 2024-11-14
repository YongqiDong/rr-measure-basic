
import os
import numpy as np
import time
from load_data import load_data_afdb, load_data_nsr, load_data_nsrrri
from tensorflow.keras.models import load_model
import tensorflow as tf
from test import test, metric_patient_level, metric_sample_level


os.environ["CUDA_VISIBLE_DEVICES"] = "0"


def is_af_patient(seq, pred):
    i = 0
    max_af = 0

    while i < seq.shape[0]:
        if pred[i] >= 0.5:
            e = i + 1
            while e < seq.shape[0] and pred[e] >= 0.5:
                e += 1

            max_af = max(max_af, np.sum(seq[i:e]))

            i = e
        i += 1

    return max_af


if __name__ == '__main__':
    start = time.time()

    model_path = '/code/model.h5'
    model = load_model(model_path, custom_objects={'tf': tf})

    test_path = '/data'
    dataset_af = ['AFDB']
    dataset_naf = ['NSRDB', 'NSRRRIDB']

    save_pred = np.array([])
    save_label = np.array([])

    save_af = np.array([])
    save_naf = np.array([])

    for ds in dataset_af:
        cur_path = test_path + '/' + ds

        with open(cur_path + '/' + 'RECORDS') as f:
            filenames = f.readlines()
        filenames = [file.strip() for file in filenames]

        for file in filenames:
            print('{} {} running'.format(ds, file))

            cur = test(cur_path, file, load_data_afdb, model)

            seq_time, label, pred = cur[0, :], cur[1, :], cur[2, :]

            save_label = np.hstack((save_label, label))
            save_pred = np.hstack((save_pred, pred))

            if is_af_patient(seq_time, label) >= 6 * 60:
                save_af = np.hstack((save_af, is_af_patient(seq_time, pred)))

    for ds in dataset_naf:
        cur_path = test_path + '/' + ds

        with open(cur_path + '/' + 'RECORDS') as f:
            filenames = f.readlines()
        filenames = [file.strip() for file in filenames]

        for file in filenames:
            print('{} {} running'.format(ds, file))

            if ds == 'NSRDB':
                cur = test(cur_path, file, load_data_nsr, model)
            else:
                cur = test(cur_path, file, load_data_nsrrri, model)

            seq_time, label, pred = cur[0, :], cur[1, :], cur[2, :]

            save_label = np.hstack((save_label, label))
            save_pred = np.hstack((save_pred, pred))

            save_naf = np.hstack((save_naf, is_af_patient(seq_time, pred)))

    sample_level_result = metric_sample_level(np.vstack((save_label, save_pred)).T)
    patient_level_result = metric_patient_level(save_af, save_naf)

    end = time.time()

    print('\nrunning time: {:0.3f}s'.format(end - start))

    print('\nsample level result')
    print('sen: {:0.3f}'.format(sample_level_result[0]))
    print('spe: {:0.3f}'.format(sample_level_result[1]))
    print('acc: {:0.3f}'.format(sample_level_result[2]))
    print('auc: {:0.3f}'.format(sample_level_result[3]))

    print('\npatient level result')
    print('sen: {:0.3f}'.format(patient_level_result[0]))
    print('spe: {:0.3f}'.format(patient_level_result[1]))
    print('acc: {:0.3f}'.format(patient_level_result[2]))
    print('auc: {:0.3f}'.format(patient_level_result[3]))

    running_time = 'running time: {:0.3f}s'.format(end - start)
    sample_sen = 'sen: {:0.3f}'.format(sample_level_result[0])
    sample_spe = 'spe: {:0.3f}'.format(sample_level_result[1])
    sample_acc = 'acc: {:0.3f}'.format(sample_level_result[2])
    sample_auc = 'auc: {:0.3f}'.format(sample_level_result[3])

    patient_sen = 'sen: {:0.3f}'.format(patient_level_result[0])
    patient_spe = 'spe: {:0.3f}'.format(patient_level_result[1])
    patient_acc = 'acc: {:0.3f}'.format(patient_level_result[2])
    patient_auc = 'auc: {:0.3f}'.format(patient_level_result[3])

    save_path = '/results'
    if os.path.exists(save_path + '/' + 'output.txt'):
        os.remove(save_path + '/' + 'output.txt')
    with open(save_path + '/' + 'output.txt', mode='a+') as f:
        f.writelines(running_time + '\n')
        f.writelines('\n')

        f.writelines('sample level result\n')
        f.writelines(sample_sen + '\n')
        f.writelines(sample_spe + '\n')
        f.writelines(sample_acc + '\n')
        f.writelines(sample_auc + '\n')
        f.writelines('\n')

        f.writelines('patient level result\n')
        f.writelines(patient_sen + '\n')
        f.writelines(patient_spe + '\n')
        f.writelines(patient_acc + '\n')
        f.writelines(patient_auc + '\n')
