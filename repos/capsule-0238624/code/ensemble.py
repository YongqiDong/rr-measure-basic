import csv
from sklearn.model_selection import train_test_split
import numpy
import matplotlib.pyplot as plt
from keras.models import load_model

from sklearn.metrics import classification_report, confusion_matrix
from keras.utils import plot_model
from keras.models import Model

from keras.layers import Dense
from keras.layers.merge import concatenate

# load sub models
def load_all_models(n_models):
    all_models = list()
    for i in range(n_models):
        filename = '/data/ensemble/' + str(i + 1) + '.h5'
        model = load_model(filename)
        all_models.append(model)
        print('>loaded %s' % filename)
    return all_models

# create the ensemble model
def define_stacked_model(members):

    for i in range(len(members)):
        model = members[i]
        for layer in model.layers:
            layer.trainable = False # set layers to be non trainable in order to preserve weights
            layer._name = 'ensemble_' + str(i+1) + '_' + layer._name
    
    ensemble_visible = [model.input for model in members]
    ensemble_outputs = [model.output for model in members]
    merge = concatenate(ensemble_outputs)
    hidden = Dense(10, activation='relu')(merge)
    hidden1 = Dense(10, activation='relu')(hidden)
    hidden2 = Dense(10, activation='relu')(hidden1)
    hidden3 = Dense(10, activation='relu')(hidden2)
    hidden4 = Dense(10, activation='relu')(hidden3)
    hidden5 = Dense(10, activation='relu')(hidden4)
    hidden6 = Dense(10, activation='relu')(hidden5)
    hidden7 = Dense(10, activation='relu')(hidden6)
    output = Dense(1, activation='sigmoid')(hidden7)
    model = Model(inputs=ensemble_visible, outputs=output)
    model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
    return model

def fit_stacked_model(model, inputX, inputy):
    
    X = [inputX for _ in range(len(model.input))]
    history = model.fit(X, inputy, validation_split=0.33, epochs=150, batch_size=10)
    

def predict_stacked_model(model, inputX):
    X = [inputX for _ in range(len(model.input))]
    return model.predict(X)

#data from csv
Xi = []
with open("/data/normal_train.csv", 'r') as r:
    c = 0
    reader = csv.reader(r)
    for row in reader:
        if c==0:
            c+=1
            continue
        for j in range(0,len(row)):
            row[j] = float(row[j])
        Xi.append(row)

train ,test = train_test_split(Xi,test_size=0.25) # train and test set created
X = []
Y =[]
Xt = []
Yt = []
for i in train:
    X.append(i[:len(i)-1])
    Y.append(i[len(i)-1:][0])
for i in test:
    Xt.append(i[:len(i)-1])
    Yt.append(i[len(i) - 1:][0])

print(len(X))
print(len(Y))
trainX = numpy.array(X)
testX = numpy.array(Xt)
print(trainX.shape, testX.shape)
Y = numpy.array(Y)

n_members = 3
members = load_all_models(n_members)
print('Loaded %d models' % len(members))

stacked_model = define_stacked_model(members)

fit_stacked_model(stacked_model, trainX,Y)

yhat = predict_stacked_model(stacked_model, testX) # get predictions
p = [] # predicted output classes

for x in numpy.nditer(yhat):
    xi = []
    if x < float(0.5):
        p.append(0)
    else:
        p.append(1)

count = 0
for i in range(0,len(Yt)):
    if Yt[i]==p[i]: # actual output == predicted output
        count +=1

print(count)
print((count/len(Yt))*100) # accuracy score
print("=== Confusion Matrix ===")
print(confusion_matrix(Yt, p))
print('\n')
print("=== Classification Report ===")
print(classification_report(Yt, p))
print('\n')