#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon May 21 00:01:24 2018

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

np.random.shuffle(data)
np.random.shuffle(test)

X_data = data[:, 0:-1]
X_data
Y_data = data[:,[-1]]
X = tf.placeholder(tf.float32,[None, 28*28*3])
X_img = tf.reshape(X, [-1, 28,28, 3])
Y = tf.placeholder(tf.int32, shape=[None, 1])
Y_one = tf.one_hot(Y, 27)
Y_one = tf.reshape(Y_one, [-1,27])

W1 = tf.Variable(tf.random_normal([3,3,3, 12], stddev=0.01))
W1 = tf.get_variable("W1", shape=[3,3,3,32], initializer=tf.contrib.layers.xavier_initializer())
L1 = tf.nn.conv2d(X_img, W1, strides=[1,1,1,1], padding='SAME')
L1 = tf.nn.leaky_relu(L1)
L1 = tf.nn.max_pool(L1, ksize = [1,2,2,1], strides=[1,2,2,1], padding='SAME')

W3 = tf.get_variable("W3", shape=[3,3,32,64], initializer=tf.contrib.layers.xavier_initializer())
W2 = tf.Variable(tf.random_normal([3,3,12,14], stddev=0.01))
L2 = tf.nn.conv2d(L1, W2, strides=[1,1,1,1], padding='SAME')
L2 = tf.nn.leaky_relu(L2)
L2 = tf.nn.max_pool(L2, ksize=[1,2,2,1],strides=[1,2,2,1], padding='SAME')
L2 = tf.reshape(L2, [-1,7*7*14])

W3 = tf.get_variable("We2", shape=[7*7*14,27], initializer=tf.contrib.layers.xavier_initializer())
b= tf.Variable(tf.random_normal([27]))
hypo = tf.matmul(L2,W3)+b
H = tf.nn.softmax(hypo)
cost_i = tf.nn.softmax_cross_entropy_with_logits(logits=hypo, labels=Y_one)
cost = tf.reduce_mean(cost_i)
train_Adam = tf.train.AdamOptimizer(learning_rate=0.01).minimize(cost)
train = tf.train.GradientDescentOptimizer(learning_rate=0.03).minimize(cost)
train_delta =tf.train.AdadeltaOptimizer(learning_rate=0.1).minimize(cost)

correct = tf.equal(tf.argmax(H,1), tf.argmax(Y_one,1))
accuracy = tf.reduce_mean(tf.cast(correct,tf.float32))

sess = tf.Session()
sess.run(tf.global_variables_initializer())

batch_size = 100 # 배치 사이즈 지정
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
