#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 27 23:36:14 2018

@author: chanheelee
"""
import sys, os
import numpy as np
os.chdir("/Users/chanheelee/Desktop/MNIST")

# 데이터 불러오기
test = np.loadtxt('mnist_test.csv',delimiter=',', dtype=np.float32)
data = np.loadtxt('mnist_train.csv',delimiter=',', dtype=np.float32)

import random
np.random.shuffle(data)
np.random.shuffle(test)


import tensorflow as tf

X_data = data[:, 1:]
X_data = X_data/255
np.shape(X_data)
Y_data = data[:,0]
np.shape(Y_data)
Y_data = np.reshape(Y_data, [60000,1])
np.shape(Y_data)
X = tf.placeholder(tf.float32, shape=[None, 254])
Y = tf.placeholder(tf.int32, shape=[None, 1])
Y_one = tf.one_hot(Y, 10)
Y_one = tf.reshape(Y_one, [-1,10])
keep_prob=tf.placeholder(tf.float32)
Y_data

W1 = tf.get_variable("newW1z3z1zzes1az1",shape=[254,178], initializer=tf.contrib.layers.variance_scaling_initializer())
b1 = tf.Variable(tf.random_normal([178]))
L1 = tf.nn.leaky_relu(tf.matmul(X, W1)+b1)
L1 = tf.nn.dropout(L1, keep_prob)

W2 = tf.get_variable("newW2z2zzeszza1a3z2",shape=[178,178], initializer=tf.contrib.layers.variance_scaling_initializer())
b2 = tf.Variable(tf.random_normal([178]))
L2 = tf.nn.leaky_relu(tf.matmul(L1, W2)+b2)
L2 = tf.nn.dropout(L2, keep_prob)

W3 = tf.get_variable("newW3z33s3edzz3a3dp33",shape=[178,254], initializer=tf.contrib.layers.variance_scaling_initializer())
b3 = tf.Variable(tf.random_normal([254]))
L3 = tf.nn.leaky_relu(tf.matmul(L2, W3)+b3)
L3 = tf.nn.dropout(L3, keep_prob)

W4 = tf.get_variable("newW3sz333edadzza33dp33",shape=[254,254*2], initializer=tf.contrib.layers.variance_scaling_initializer())
b4 = tf.Variable(tf.random_normal([254*2]))
L4 = tf.nn.leaky_relu(tf.matmul(L3, W4)+b4)
L4 = tf.nn.dropout(L4, keep_prob)

W5 = tf.get_variable("neawaW3z333eddzz3a3dpdaa33",shape=[254*2,254*4], initializer=tf.contrib.layers.variance_scaling_initializer())
b5 = tf.Variable(tf.random_normal([254*4]))
L5 = tf.nn.leaky_relu(tf.matmul(L4, W5)+b5)
L5 = tf.nn.dropout(L5, keep_prob)

W6 = tf.get_variable("newW3ddz333eddzaza33zadp3a3",shape=[254*4,10], initializer=tf.contrib.layers.variance_scaling_initializer())
b6 = tf.Variable(tf.random_normal([10]))


# 가설 및 코스트 함수 설정
logits=tf.matmul(L5, W6)+b6
H = tf.nn.softmax(logits)
cost_i = tf.nn.softmax_cross_entropy_with_logits(logits=logits, labels=Y_one)
cost = tf.reduce_mean(cost_i)
train_Adam = tf.train.AdamOptimizer(learning_rate=0.005).minimize(cost)

correct = tf.equal(tf.argmax(logits,1), tf.argmax(Y_one,1))
accuracy = tf.reduce_mean(tf.cast(correct,tf.float32))

# 배치 학습
sess = tf.Session()
sess.run(tf.global_variables_initializer())

batch_size = 600 # 배치 사이즈 지정
training_epochs =30000 # epoch 횟수 지정

# epoch 횟수만큼 반복
for epoch in range(training_epochs) :
    avg_cost = 0
    batch_count = int(X_data.shape[0]/batch_size) # 배치의 갯수
    
    for i in range(batch_count) : # 배치를 순차적으로 읽는 루프
        batch_xs, batch_ys = X_data[i*batch_size:i*batch_size+batch_size], Y_data[i*batch_size:i*batch_size+batch_size] 
        c, _ = sess.run([cost,train_Adam], feed_dict = {X : batch_xs, Y : batch_ys, keep_prob : 0.85})
        avg_cost+= c/batch_count
    print('Epoch:', '%04d'%(epoch+1), 'cost=', '{:.9f}'.format(avg_cost))
    

# test set 지정
X_test = test[:,1:]/255
X_test
np.shape(X_test)
Y_test = test[:,0]
Y_test = np.reshape(Y_test, [10000,1])
feed2 = {X:X_test, Y:Y_test, keep_prob : 1}
 

# test set performance 
Accuracy_train = sess.run([accuracy], feed_dict={X : X_data, Y : Y_data, keep_prob : 1})
Accuracy_test = sess.run([accuracy], feed_dict=feed2)

Accuracy_train
Accuracy_test
