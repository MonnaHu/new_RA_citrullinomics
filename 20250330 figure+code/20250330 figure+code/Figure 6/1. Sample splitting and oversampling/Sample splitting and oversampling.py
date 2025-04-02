import pandas as pd
import numpy as np
import os
import random

def cal_max_common_len(str1,str2):
    str1,str2=(str2,str1) if len(str1)>len(str2) else (str1,str2)
    f=[]
    for i in range(len(str1),7,-1):
        for j in range(len(str1)+1-i):
            e=str1[j:j+i]
            if e in str2:
                f.append(len(e))
        if f:
            break
    if len(f)>0:
        f1=max(f)
    else:
        f1=0
    return f1

s_poss=[]
df = pd.read_excel("H:/20250330 figure+code/Figure 6/1. Sample splitting and oversampling/8816 positive.xlsx")

data_all = df.values
for data in data_all:
    try:
        sequence = data[0].strip()
    except:
        print("skip line {}".format(sequence))
    if len(sequence)!=9:
        print("skip line {}".format(sequence))
        continue
    s_poss.append(sequence)

df = pd.read_excel("H:/20250330 figure+code/Figure 6/1. Sample splitting and oversampling/67399(removed 7-9aa duplicates) negative.xlsx")

data_all = df.values
s_neg1 = []
for data in data_all:
    try:
        sequence = data[0].strip()
    except:
        print("skip line {}".format(sequence))
    if len(sequence)!=9:
        print("skip line {}".format(sequence))
        continue
    s_neg1.append(sequence)

random.shuffle(s_poss)
random.shuffle(s_neg1)

s_all=s_poss+s_neg1
import collections
aa=[item for item,count in collections.Counter(s_neg1).items() if count > 1]

s_poss=list(set(s_poss))
s_neg=list(set(s_neg1))


for num in range(10):

    s_pos_test_num=882
    s_neg_test_num=6740

    s_all=s_neg+s_poss

    assert len(s_all)==len(list(set(s_all)))
    s_neg_test=s_neg[num*s_neg_test_num:(num+1)*s_neg_test_num]
    s_neg_train=list(set(s_neg)-set(s_neg_test))
    s_pos_test=s_poss[num*s_pos_test_num:(num+1)*s_pos_test_num]
    s_pos_train=list(set(s_poss)-set(s_pos_test))


    s_pos_train_oversample=[]
    import random
    s_pos_train_oversample.extend(s_pos_train)
    if len(s_pos_train_oversample) < len(s_neg_train):
        random.seed(123)
        additional_samples = random.choices(s_pos_train, k=len(s_neg_train) - len(s_pos_train_oversample))
        s_pos_train_oversample.extend(additional_samples)

    print(len(s_pos_train_oversample))
    print(len(s_neg_train))


    f3=open("./result/train_num%d.txt"%num,"w+",encoding='utf-8')
    for pos in s_pos_train_oversample:
        if len(pos) != 9:
            print(pos)
            continue
        f3.write("{} 1\n".format(pos))
    for pos in s_neg_train[:-1]:
        if len(pos)!=9:
            print(pos)
            continue
        f3.write("{} 0\n".format(pos))
    f3.write("{} 0".format(s_neg_train[-1]))

    f3=open("./result/test_num%d.txt"%num,"w+",encoding='utf-8')
    for pos in s_pos_test:
        if len(pos)!=9:
            print(pos)
            continue
        f3.write("{} 1\n".format(pos))

    for pos in s_neg_test[:-1]:
        if len(pos)!=9:
            print(pos)
            continue
        f3.write("{} 0\n".format(pos))
    f3.write("{} 0".format(s_neg_test[-1]))