
import os
import numpy as np
from sklearn.metrics import roc_auc_score, auc


os.environ["CUDA_VISIBLE_DEVICES"] = "-1"


def test(path, file, load_func, model, interval=90):
    seq, label = load_func(path, file)

    seq = np.reshape(seq[:seq.shape[0] // interval * interval], (-1, interval))
    label = np.reshape(label[:label.shape[0] // interval * interval], (-1, interval))

    seq = seq[(label >= 0).all(axis=-1)]
    label = label[(label >= 0).all(axis=-1)]

    if seq.shape[0] < 5:
        return None

    label = np.sum(label, axis=-1)
    label[label < interval // 2] = 0
    label[label >= interval // 2] = 1

    seq_time = np.sum(seq, axis=-1)
    pred = model_test(seq, model)
    assert label.shape[0] == pred.shape[0]

    return np.vstack((seq_time, label, pred))


def model_test(test_x, model):
    test_x = np.expand_dims(test_x, 2)
    predict_y = np.squeeze(model.predict(test_x))

    return predict_y


def metric_sample_level(array):
    label = array[:, 0]
    label[label >= 1] = 1
    label[label <= 0] = 0
    pred = array[:, 1]

    re_auc = roc_auc_score(label, pred)

    pred[pred >= 0.5] = 1
    pred[pred < 0.5] = 0

    p = pred[label >= 1]
    tp = p[p == 1]
    n = pred[label <= 0]
    tn = n[n == 0]

    return [tp.shape[0] / p.shape[0] if p.shape[0] > 0 else 1,
            tn.shape[0] / n.shape[0] if n.shape[0] > 0 else 1,
            (tp.shape[0] + tn.shape[0]) / (p.shape[0] + n.shape[0]), re_auc]


def metric_patient_level(pos, neg, th=6*60):
    sen = pos[pos >= th].shape[0] / pos.shape[0]
    spe = neg[neg < th].shape[0] / neg.shape[0]
    acc = (pos[pos >= th].shape[0] + neg[neg < th].shape[0]) / (pos.shape[0] + neg.shape[0])

    fpr = np.array([0])
    tpr = np.array([0])

    thresh = (int(max(neg)) + 1) * 60
    while thresh > 0:
        cur_t = thresh / 60
        thresh -= 10

        tp = pos[pos >= cur_t].shape[0] / pos.shape[0]
        fp = 1. - neg[neg < cur_t].shape[0] / neg.shape[0]

        fpr = np.hstack((fpr, fp))
        tpr = np.hstack((tpr, tp))

    fpr = np.hstack((fpr, 1.0))
    tpr = np.hstack((tpr, 1.0))

    roc_auc = auc(fpr, tpr)

    return [sen, spe, acc, roc_auc]
