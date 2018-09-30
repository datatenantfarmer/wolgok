#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed May  9 21:40:50 2018

@author: chanheelee
"""


import sys, os
import numpy as np
os.chdir("/Users/chanheelee/Desktop/deep-learning-from-scratch-master/alphabet")

# 데이터 불러오기
test = np.loadtxt('Newtest.csv',delimiter=',', dtype=np.float32)
data = np.loadtxt('Newsample.csv',delimiter=',', dtype=np.float32)
np.shape(data)
import tensorflow as tf
import random
data
np.random.shuffle(data)
np.random.shuffle(test)
np.shape(data)
test
# 머신 뼈대 설정
X_data = data[:, 0:-1]
X_data
Y_data = data[:,[-1]]
np.shape(Y_data)
Y_data
X = tf.placeholder(tf.float32, shape=[None, 2352])
Y = tf.placeholder(tf.int32, shape=[None, 1])
Y_one = tf.one_hot(Y, 27)
Y_one = tf.reshape(Y_one, [-1,27])
np.shape(Y_one)

variance_scaling
# 샤비에 이니셜라이저, 3중 뉴럴 네트워크
W1 = tf.Variable(tf.random_normal([2352, 28*28]))
W1 = tf.get_variable("ei1",shape=[28*28*3,28*28], initializer=tf.contrib.layers.xavier_initializer())
b1 = tf.Variable(tf.random_normal([28*28]))
L1 = tf.nn.relu(tf.matmul(X, W1)+b1)

W2 = tf.Variable(tf.random_normal([28*28, 28*13])) 
W2 = tf.get_variable("wei2",shape=[28*28,28*13], initializer=tf.contrib.layers.xavier_initializer())
b2 = tf.Variable(tf.random_normal([28*13]))
L2 = tf.nn.relu(tf.matmul(L1, W2)+b2)

W3 = tf.get_variable("weigh3",shape=[28*13,28*6], initializer=tf.contrib.layers.xavier_initializer())
b3 = tf.Variable(tf.random_normal([28*6]))
L3 = tf.nn.relu(tf.matmul(L2, W3)+b3)

W4 = tf.Variable(tf.random_normal([28*6, 28*6]))
W4 = tf.get_variable("Weigh4",shape=[28*6, 28*6], initializer=tf.contrib.layers.xavier_initializer())
b4 = tf.Variable(tf.random_normal([28*6]))
L4 = tf.nn.relu(tf.matmul(L3, W4)+b4)

W5 = tf.Variable(tf.random_normal([28*6, 27]))
W5 = tf.get_variable("Weigh5",shape=[28*6,27], initializer=tf.contrib.layers.xavier_initializer())
b5 = tf.Variable(tf.random_normal([27]))

# 가설 및 코스트 함수 설정
logits=tf.matmul(L4, W5)+b5
H = tf.nn.softmax(logits)
cost_i = tf.nn.softmax_cross_entropy_with_logits(logits=logits, labels=Y_one)
cost = tf.reduce_mean(cost_i)
train = tf.train.GradientDescentOptimizer(learning_rate=0.03).minimize(cost)
train_Adam = tf.train.AdamOptimizer(learning_rate=0.001).minimize(cost)

correct = tf.equal(tf.argmax(logits,1), tf.argmax(Y_one,1))
accuracy = tf.reduce_mean(tf.cast(correct,tf.float32))

# 배치 학습
sess = tf.Session()
sess.run(tf.global_variables_initializer())

batch_size = 2700 # 배치 사이즈 지정
training_epochs = 100 # epoch 횟수 지정

# epoch 횟수만큼 반복
for epoch in range(training_epochs) :
    avg_cost = 0
    batch_count = int(X_data.shape[0]/batch_size) # 배치의 갯수
    
    for i in range(batch_count) : # 배치를 순차적으로 읽는 루프
        batch_xs, batch_ys = X_data[i*batch_size:i*batch_size+batch_size], Y_data[i*batch_size:i*batch_size+batch_size] 
        c, _ = sess.run([cost,train_Adam], feed_dict = {X : batch_xs, Y : batch_ys})
        avg_cost+= c/batch_count
    print('Epoch:', '%04d'%(epoch+1), 'cost=', '{:.9f}'.format(avg_cost))
    
Accuracy_train = sess.run([accuracy], feed_dict={X : X_data, Y : Y_data})

Accuracy_train

# test set 지정
test
X_test = test[:,0:-1]/255
X_test
Y_test = test[:,[-1]]
feed2 = {X:X_test, Y:Y_test}
 
np.shape(X_test)
# test set performance 
Accuracy_test = sess.run([accuracy], feed_dict={X: X_test, Y: Y_test})
Accuracy_test

Accuracy_train
Accuracy_test

cost_list = []
epoch_list = [i for i in range(1, training_epochs + 1)]



for epoch in range(training_epochs) :
    avg_cost = 0
    batch_count = int(X_data.shape[0]/270) # 배치의 갯수

    for i in range(batch_count) : # 배치를 순차적으로 읽는 루프
        batch_xs, batch_ys = X_data[i*batch_size:i*batch_size+batch_size], Y_data[i*batch_size:i*batch_size+batch_size] 
        c, _ = sess.run([cost,train_Adam], feed_dict = {X : batch_xs, Y : batch_ys})
        avg_cost+= c/batch_count

    ## 18.05.09 add
    cost_list.append(avg_cost)



    print('Epoch:', '%04d'%(epoch+1), 'cost=', '{:.9f}'.format(avg_cost))

# 18.05.09 add  make cost plot (x : epoch_list, y : cost_list)
plt.plot(epoch_list[0:760], cost_list[0:760], color = 'blue')
plt.title("Optimization Using Adam optimizer")
plt.ylabel("Value of cost")
plt.xlabel("# of epochs")
plt.show()


