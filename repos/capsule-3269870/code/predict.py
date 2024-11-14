import math
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.pyplot import plot, savefig, figure
import pandas as pd
import tensorflow as tf
from tensorflow.python.framework import ops


utrain = [[  13.91402715],
          [  61.49773756],
          [ 129.47963801],
          [  10.85972851]]
stdtrain = [[  5.69248394],
            [ 11.7652854 ],
            [ 64.00527263],
            [  4.62173284]]


my_data = pd.read_csv('../data/test_data.csv').as_matrix()

M = np.shape(my_data)[0]
N = np.shape(my_data)[1]
L_Z = N
Y_I = np.shape(my_data)[1] - 1


#t_m = int(M*0.8)

#train_set = my_data[0:t_m,:]

test_set = my_data[:,:]


X_test = test_set[:,0:Y_I].T

X_test_norm = (X_test - utrain) / stdtrain

Y_test = test_set[:,Y_I].T.reshape(1,-1)

def create_placeholders(n_x, n_y):
    

    X = tf.placeholder(tf.float32, shape = [n_x,None])
    Y = tf.placeholder(tf.float32, shape = [n_y,None])
    
    return X, Y
 
def forward_propagation(X, parameters):
    print("FP")
    
    # Retrieve the parameters from the dictionary "parameters"
    W1 = parameters['W1']
    b1 = parameters['b1']
    W2 = parameters['W2']
    b2 = parameters['b2']
    W3 = parameters['W3']
    b3 = parameters['b3']
    
    ### START CODE HERE ### (approx. 5 lines)              # Numpy Equivalents:
    Z1 = tf.add(tf.matmul(W1,X),b1)                                              # Z1 = np.dot(W1, X) + b1
    A1 = tf.nn.relu(Z1)                                              # A1 = relu(Z1)
    Z2 = tf.add(tf.matmul(W2,A1),b2)                                              # Z2 = np.dot(W2, a1) + b2
    A2 = tf.nn.relu(Z2)                                              # A2 = relu(Z2)
    Z3 = tf.add(tf.matmul(W3,A2),b3)                                              # Z3 = np.dot(W3,Z2) + b3
    ### END CODE HERE ###
    
    return Z3
    
def printParams(parameters,sess, save):
    W1 = parameters['W1']
    b1 = parameters['b1']
    W2 = parameters['W2']
    b2 = parameters['b2']
    W3 = parameters['W3']
    b3 = parameters['b3']
    Utrain = parameters['Utrain']
    STDtrain = parameters['STDtrain']
    print ("W1 = " + str(W1.eval(session=sess)))
    print ("b1 = " + str(b1.eval(session=sess)))
    print ("W2 = " + str(W2.eval(session=sess)))
    print ("b2 = " + str(b2.eval(session=sess)))
    print ("W3 = " + str(W3.eval(session=sess)))
    print ("b3 = " + str(b3.eval(session=sess)))
    print ("Utrain = " + str(Utrain.eval(session=sess)))
    print ("STDtrain = " + str(STDtrain.eval(session=sess)))
    if save:
        saver = tf.train.Saver({"W1": W1,
                               "b1": b1,
                               "W2": W2,
                               "b2": b2,
                               "W3": W3,
                               "b3": b3})
        saver.save(sess, '../results/output.chkp')


 
def predict(X_test, Y_test, utrain, stdtrain, l_z):
    
    print("#######\nRunning predict.py...")
    ops.reset_default_graph() 
    (n_x, m) = X_test.shape
    n_y = Y_test.shape[0]
    
    l_z2 = l_z + l_z/2
    W1 = tf.get_variable("W1", [l_z,n_x])
    b1 = tf.get_variable("b1", [l_z,1])
    W2 = tf.get_variable("W2", [l_z2,l_z])
    b2 = tf.get_variable("b2", [l_z2,1])
    W3 = tf.get_variable("W3", [1,l_z2])
    b3 = tf.get_variable("b3", [1,1])
    Utrain = tf.constant(utrain)

    STDtrain = tf.constant(stdtrain)
    
    parameters = {"W1": W1,
                  "b1": b1,
                  "W2": W2,
                  "b2": b2,
                  "W3": W3,
                  "b3": b3
                 }
    X, Y = create_placeholders(n_x, n_y)
    A3 = tf.round(tf.sigmoid(forward_propagation(X, parameters)))
    saver = tf.train.Saver()
    sess = tf.Session()
    saver.restore(sess, "../results/output.chkp")
    parameters = {"W1": W1, "b1": b1, "W2": W2, "b2": b2, "W3": W3, "b3": b3, "Utrain": Utrain, "STDtrain": STDtrain}
    printParams(parameters,sess,False)
    result = sess.run(A3, feed_dict={X: X_test, Y: Y_test} )
    

    f = open('../results/predict-output.txt','w')

    ostr = "RESULT = "+str(result)
    print(ostr)
    str2 =  "Y = "+str(Y_test)
    ostr = ostr+'\n'+str2
    print(str2)
    equal = result == Y_test;
    str2 = "EQUAL = "+str(np.sum(equal))
    ostr = ostr+'\n'+str2
    print(str2)
    truep = np.sum( (result == 1) & (result == Y_test))
    totalp = np.sum(result == 1)
    actualp = np.sum(Y_test == 1)
    str2 = "TOTAL POSITIVE = "+str(totalp)
    ostr = ostr+'\n'+str2
    print(str2)
    str2 = "TRUE POSITIVE = "+str(truep)
    ostr = ostr+'\n'+str2
    print(str2)
    str2 = "ACTUAL POSITIVE = "+str(actualp)
    ostr = ostr+'\n'+str2
    print(str2)
    
    precision = (100* truep) / totalp
    recall = (100 *truep) / actualp
    f1 = (precision * recall) / (precision + recall)
    str2 = "PRECISION "+ str ( precision ) +"%" 
    ostr = ostr+'\n'+str2
    print(str2)
    str2 = "RECALL "+ str ( recall) +"%" 
    ostr = ostr+'\n'+str2
    print(str2)
    str2 = "F1 "+ str ( 2*f1) +"%"
    ostr = ostr+'\n'+str2
    print(str2)
    
    str2 = "TOTAL = "+str(Y_test.size)
    ostr = ostr+'\n'+str2
    print(str2)
    str2 = "ACCURACY "+ str (  (100*np.sum(result == Y_test)) / Y_test.size ) +"%"
    ostr = ostr+'\n'+str2
    print(str2)
    
    f.write(ostr)
    f.close()
    
    sess.close()


    
predict(X_test_norm, Y_test, utrain, stdtrain, L_Z)