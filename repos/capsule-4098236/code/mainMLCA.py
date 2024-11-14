import numpy as np
from sklearn.model_selection import cross_validate
from sklearn.metrics import make_scorer
import sklearn.metrics as metrics
from skmultilearn.dataset import load_dataset
from skmultilearn.dataset import available_data_sets

from mlca import MLCA

print('Start...')

# load dataset
# available_data_sets()  # list of dataset
data_name = 'emotions'
data, target, feature_names, label_names = load_dataset(data_name, 'undivided')
data = data.toarray()
target = target.toarray()

# Number of folds for cross validation
splits = 10

# Algorithm
mlca = MLCA(plambda=50, v_thres=0.55)


# Evaluation Metrics
def exact_match(y_true, y_pred):
    return metrics.accuracy_score(y_true, y_pred)
def f1_score(y_true, y_pred):
    return metrics.f1_score(y_true, y_pred, average='macro')
def ave_precision(y_true, y_pred):
    return metrics.label_ranking_average_precision_score(y_true, y_pred)
def hamming_loss(y_true, y_pred):
    return metrics.hamming_loss(y_true, y_pred)
def coverage_error(y_true, y_pred):
    return metrics.coverage_error(y_true, y_pred)
def ranking_loss(y_true, y_pred):
    return metrics.label_ranking_loss(y_true, y_pred)

score_funcs = {
    'exact_match': make_scorer(exact_match),
    'macro_f1_score': make_scorer(f1_score),
    'ave_precision': make_scorer(ave_precision),
    'hamming_loss': make_scorer(hamming_loss),
    'coverage_error': make_scorer(coverage_error),
    'ranking_loss': make_scorer(ranking_loss),
    }


# Cross Validation
cv_results = cross_validate(estimator=mlca, X=data, y=target, scoring=score_funcs, return_estimator=True, cv=splits, n_jobs=-1)


# Averaged results
n_nodes = [cv_results['estimator'][k].G_.number_of_nodes() for k in range(splits)]
print('# of Nodes: ', np.mean(n_nodes))
print('Exact Match: ', np.mean(cv_results['test_exact_match']))
print('Hamming Loss: ', np.mean(cv_results['test_hamming_loss']))
print('Finished')

