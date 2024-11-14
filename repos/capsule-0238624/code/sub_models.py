import csv
from sklearn.model_selection import train_test_split
import numpy
from keras.models import Sequential
from keras.layers import Dense
from matplotlib import pyplot

# Training the sub model
def fit_model(trainX, trainy):
    model = Sequential()
    model.add(Dense(32, input_dim=19, activation='relu'))
    model.add(Dense(64, activation='relu'))
    model.add(Dense(128, activation='relu'))
    model.add(Dense(128, activation='relu'))
    model.add(Dense(64, activation='relu'))
    model.add(Dense(32, activation='relu'))
    model.add(Dense(1, activation='sigmoid'))
    model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
    model.fit(trainX, trainy, epochs=100)
    return model

Xi = [] #data from csv
with open("/data/normal_train.csv", 'r') as r:
    c = 0
    reader = csv.reader(r)
    for row in reader:
        if c==0:
            c+=1
            continue # omit title row
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
Y = numpy.array(Y)
Yt = numpy.array(Yt)
print(trainX.shape, testX.shape)


n_members = 3    # number of sub models
for i in range(n_members):
    model = fit_model(trainX, Y)
    filename = '/results/ensemble/' + str(i + 1) + '.h5'  # path for storing the sub model
    model.save(filename)
    print('>Saved %s' % filename)
