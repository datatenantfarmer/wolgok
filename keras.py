
# coding: utf-8

# In[6]:


from keras.datasets import mnist #MNIST


# In[7]:


(train_images, train_labels), (test_images, test_labels)= mnist.load_data()


# In[8]:


digit=train_images[4] 
digit


# In[9]:


from keras import models
from keras import layers


# In[10]:


network=models.Sequential()
network.add(layers.Dense(512, activation='relu', input_shape=(28*28,)))
network.add(layers.Dense(300, activation='relu'))
network.add(layers.Dense(10, activation='softmax'))


# In[11]:


network.compile(optimizer='adam',loss='categorical_crossentropy',metrics=['accuracy'])


# In[12]:


train_images= train_images.reshape((60000,28*28)) 
train_images= train_images.astype('float32')/255 

test_images= test_images.reshape((10000,28*28)) 
test_images= test_images.astype('float32') /255


# In[13]:


from keras.utils import to_categorical
train_labels= to_categorical(train_labels)
test_labels= to_categorical(test_labels)


# In[14]:


network.fit(train_images, train_labels, epochs=5, batch_size=128)


# In[39]:


test_loss, test_acc= network.evaluate(test_images, test_labels)


# In[40]:


print('test_acc:', test_acc)

