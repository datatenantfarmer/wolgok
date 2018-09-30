#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 27 23:14:15 2018

@author: chanheelee
"""
import tensorflow as tf

X_data = data[0:5000, 1:]
X_data = X_data/255
np.shape(X_data)
Y_data = data[0:5000,0]
np.shape(Y_data)
Y_data = np.reshape(Y_data, [5000,1])
np.shape(Y_data)
X = tf.placeholder(tf.float32, shape=[None, 254])
Y = tf.placeholder(tf.int32, shape=[None, 1])
Y_one = tf.one_hot(Y, 10)
Y_one = tf.reshape(Y_one, [-1,10])


# 랜덤 초기치 + 단층 뉴를넷
W1 = tf.Variable(tf.random_normal([254, 254*2]))
b1 = tf.Variable(tf.random_normal([254*2]))
L1 = tf.nn.sigmoid(tf.matmul(X, W1)+b1)

W2 = tf.Variable(tf.random_normal([254*2, 10]))
b2 = tf.Variable(tf.random_normal([10]))

# 가설 및 코스트 함수 설정
logits=tf.matmul(L1, W2)+b2
H = tf.nn.softmax(logits)
cost_i = tf.nn.softmax_cross_entropy_with_logits(logits=logits, labels=Y_one)
cost = tf.reduce_mean(cost_i)
train_Adam = tf.train.AdamOptimizer(learning_rate=0.01).minimize(cost)

correct = tf.equal(tf.argmax(logits,1), tf.argmax(Y_one,1))
accuracy = tf.reduce_mean(tf.cast(correct,tf.float32))

# 배치 학습
sess = tf.Session()
sess.run(tf.global_variables_initializer())

batch_size = 100 # 배치 사이즈 지정
training_epochs =1000 # epoch 횟수 지정

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
X_test = test[0:500,1:]/255
X_test
np.shape(X_test)
Y_test = test[0:500,0]
Y_test = np.reshape(Y_test, [500,1])
feed2 = {X:X_test, Y:Y_test}
 

# test set performance 
Accuracy_test = sess.run([accuracy], feed_dict={X: X_test, Y: Y_test})
Accuracy_test

Accuracy_train
Accuracy_test
