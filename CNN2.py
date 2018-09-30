#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat May 26 15:27:59 2018

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
np.shape(X_data)
28*28
# 흑백반전
a = X_data[:,0:28*28]/3
b = X_data[:,28*28:28*28*2]/3
c = X_data[:,28*28*2:28*28*3]/3

X_new = a+b+c
np.shape(X_new)
Y_data = data[:,[-1]]
X = tf.placeholder(tf.float32,[None, 28*28])
X_img = tf.reshape(X, [-1, 28,28, 1])
Y = tf.placeholder(tf.int32, shape=[None, 1])
Y_one = tf.one_hot(Y, 27)
Y_one = tf.reshape(Y_one, [-1,27])

W1 = tf.Variable(tf.random_normal([3,3,1, 32], stddev=0.1))
L1 = tf.nn.conv2d(X_img, W1, strides=[1,1,1,1], padding='SAME')
L1 = tf.nn.leaky_relu(L1)
L1 = tf.nn.max_pool(L1, ksize = [1,2,2,1], strides=[1,2,2,1], padding='SAME')

W2 = tf.Variable(tf.random_normal([3,3,32,64], stddev=0.1))
L2 = tf.nn.conv2d(L1, W2, strides=[1,1,1,1], padding='SAME')
L2 = tf.nn.leaky_relu(L2)
L2 = tf.nn.max_pool(L2, ksize=[1,2,2,1],strides=[1,2,2,1], padding='SAME')


W3 = tf.Variable(tf.random_normal([3,3,64,128], stddev=0.1))
L3 = tf.nn.conv2d(L2, W3, strides=[1,1,1,1], padding='SAME')
L3 = tf.nn.leaky_relu(L3)
L3 = tf.nn.max_pool(L3, ksize=[1,2,2,1],strides=[1,2,2,1], padding='SAME')

L3 = tf.reshape(L3, [-1,4*4*128])

W4 = tf.get_variable("r4", shape=[4*4*128,27], initializer=tf.contrib.layers.xavier_initializer())
b1= tf.Variable(tf.random_normal([27]))


hypo = tf.matmul(L3,W4)+b1
H = tf.nn.softmax(hypo)
cost_i = tf.nn.softmax_cross_entropy_with_logits(logits=hypo, labels=Y_one)
cost = tf.reduce_mean(cost_i)
train_Adam = tf.train.AdamOptimizer(learning_rate=0.003).minimize(cost)
train = tf.train.GradientDescentOptimizer(learning_rate=0.03).minimize(cost)
train_delta =tf.train.AdadeltaOptimizer(learning_rate=0.1).minimize(cost)

correct = tf.equal(tf.argmax(H,1), tf.argmax(Y_one,1))
accuracy = tf.reduce_mean(tf.cast(correct,tf.float32))

sess = tf.Session()
sess.run(tf.global_variables_initializer())

batch_size = 2700 # 배치 사이즈 지정
training_epochs = 10 # epoch 횟수 지정

# epoch 횟수만큼 반복
for epoch in range(training_epochs) :
    avg_cost = 0
    batch_count = int(X_data.shape[0]/batch_size) # 배치의 갯수
    
    for i in range(batch_count) : # 배치를 순차적으로 읽는 루프
        batch_xs, batch_ys = X_new[i*batch_size:i*batch_size+batch_size], Y_data[i*batch_size:i*batch_size+batch_size] 
        c, _ = sess.run([cost,train_Adam], feed_dict = {X : batch_xs, Y : batch_ys})
        avg_cost+= c/batch_count
    print('Epoch:', '%04d'%(epoch+1), 'cost=', '{:.9f}'.format(avg_cost))
    
a = sess.run([accuracy], feed_dict={X : X_new, Y : Y_data})
a
