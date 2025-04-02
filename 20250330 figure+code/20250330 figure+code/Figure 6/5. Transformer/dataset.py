from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import torch.utils.data as data
import pandas as pd
import numpy as np
import os
import os.path
import re
import torch
import pickle


def default_transform(sequence,feature_dict,normalize):
    feature_map = []
    # if len(sequence) != 16:
    #     continue
    for char in sequence:
        try:
            feature_map.append(feature_dict[char])
        except:
            continue
    feature_array = np.array(feature_map)
    data = FNormalizeMult(feature_array, normalize)
    data=torch.from_numpy(data)
    return data

def slipe_transform(sequence,feature_dict,normalize,slipe_size=16):
    feature_map = []
    if len(sequence) <= slipe_size:
        for char in sequence:
            try:
                feature_map.append(feature_dict[char])
            except:
                continue
        feature_array = np.array(feature_map)
        data = FNormalizeMult(feature_array, normalize)
        data=torch.from_numpy(data)
        return [data],[sequence]
    else:
        data_ist=[]
        seq_list=[]
        for idx in range(0,len(sequence)-slipe_size):
            seq=sequence[idx:(slipe_size+idx)]
            for char in seq:
                try:
                    feature_map.append(feature_dict[char])
                except:
                    continue
            feature_array = np.array(feature_map)
            data = FNormalizeMult(feature_array, normalize)
            data=torch.from_numpy(data)
            data_ist.append(data)
            seq_list.append(seq)
        return data_ist,seq_list


def FNormalizeMult(data,normalize):
    data = np.array(data)
    for i in range(0, data.shape[1]):
        listlow = normalize[i, 0]
        listhigh = normalize[i, 1]
        delta = listhigh - listlow
        if delta != 0:
            # 第j行
            data[:, i] = (data[:, i] - listlow) / delta
    return data


def default_loader(path):
    return

def slipe_list_reader(fileList,slipe_size=16):
    lmList = []
    #后续改成 序列、label形式
    with open(fileList, 'r') as file:
        for line in file.readlines():
            tmp = line.strip("\n").split(" ")
            lmPath=tmp[0]
            label_type = tmp[-1]
            if len(lmPath)<=slipe_size:
                lmList.append((lmPath, int(label_type),lmPath))

            else:

                for idx in range(0, len(lmPath) - slipe_size):
                    seq = lmPath[idx:(slipe_size + idx)]
                    lmList.append((seq,int(label_type),lmPath))
    return lmList




def default_list_reader(fileList):
    lmList = []
    #后续改成 序列、label形式
    with open(fileList, 'r') as file:
        for line in file.readlines():
            tmp = line.strip("\n").split(" ")
            lmPath=tmp[0]
            label_type = tmp[1]
            try:
                lmList.append((lmPath, int(label_type)))
            except:
                print()
    return lmList


class LandmarkList(data.Dataset):
    def __init__(self, root, fileList,transform=default_transform, list_reader=default_list_reader, loader=default_loader):
        self.root      = root
        self.lmList   = list_reader(fileList)
        self.transform = transform
        self.loader    = loader

        self.normalize = np.load("./data/normalize.npy")

        data_dict = {"瓜氨酸": "X", "甘氨酸": "G", "丙氨酸": "A", "缬氨酸": "V", "亮氨酸": "L", "异亮氨酸": "I", "苯丙氨酸": "F", "色氨酸": "W",
                     "酪氨酸": "Y", "天冬氨酸": "D", "谷氨酸": "E", "赖氨酸": "K", "天冬酰胺": "N",
                     "谷氨酰胺": "Q", "甲硫氨酸": "M", "丝氨酸": "S", "苏氨酸": "T", "半胱氨酸": "C", "脯氨酸": "P",
                     "组氨酸": "H", "精氨酸": "R"}
        df = pd.read_excel("./data/21个氨基酸性质(1).xlsx")
        data = df.values
        feature_dict = {}
        for value in data:
            name = value[1]
            fea_list = []
            for fea in value[2:]:
                fea_list.append(float(fea))
            char = data_dict[name]
            feature_dict[char] = fea_list

        self.feature_dict=feature_dict

    def __getitem__(self, index):
        lm, target = self.lmList[index]
        lm = self.transform(lm,self.feature_dict,self.normalize)
        return lm, target, lm.shape[0]

    def __len__(self):
        return len(self.lmList)

class LandmarkListSlipe(data.Dataset):
    def __init__(self, root, fileList,transform=default_transform, list_reader=slipe_list_reader, loader=default_loader):
        self.root      = root
        self.lmList   = list_reader(fileList)
        self.transform = transform
        self.loader    = loader

        self.normalize = np.load("./data/normalize.npy")

        data_dict = {"瓜氨酸": "X", "甘氨酸": "G", "丙氨酸": "A", "缬氨酸": "V", "亮氨酸": "L", "异亮氨酸": "I", "苯丙氨酸": "F", "色氨酸": "W",
                     "酪氨酸": "Y", "天冬氨酸": "D", "谷氨酸": "E", "赖氨酸": "K", "天冬酰胺": "N",
                     "谷氨酰胺": "Q", "甲硫氨酸": "M", "丝氨酸": "S", "苏氨酸": "T", "半胱氨酸": "C", "脯氨酸": "P",
                     "组氨酸": "H", "精氨酸": "R"}
        df = pd.read_excel("./data/21个氨基酸性质(1).xlsx")
        data = df.values
        feature_dict = {}
        for value in data:
            name = value[1]
            fea_list = []
            for fea in value[2:]:
                fea_list.append(float(fea))
            char = data_dict[name]
            feature_dict[char] = fea_list

        self.feature_dict=feature_dict

    def __getitem__(self, index):
        lm, target,lm_ori = self.lmList[index]
        lm = self.transform(lm,self.feature_dict,self.normalize)
        return lm, target, lm.shape[0],lm_ori

    def __len__(self):
        return len(self.lmList)




class LandmarkListTest(data.Dataset):
    def __init__(self, root, fileList, transform=None, list_reader=default_list_reader, loader=default_loader):
        self.root      = root
        self.lmList   = list_reader(fileList)
        self.transform = transform
        self.loader    = loader
        data_dict = {"瓜氨酸": "X", "甘氨酸": "G", "丙氨酸": "A", "缬氨酸": "V", "亮氨酸": "L", "异亮氨酸": "I", "苯丙氨酸": "F", "色氨酸": "W",
                     "酪氨酸": "Y", "天冬氨酸": "D", "谷氨酸": "E", "赖氨酸": "K", "天冬酰胺": "N",
                     "谷氨酰胺": "Q", "甲硫氨酸": "M", "丝氨酸": "S", "苏氨酸": "T", "半胱氨酸": "C", "脯氨酸": "P",
                     "组氨酸": "H", "精氨酸": "R"}
        df = pd.read_excel("./data/21个氨基酸性质(1).xlsx")
        data = df.values
        feature_dict = {}
        for value in data:
            name = value[1]
            fea_list = []
            for fea in value[2:]:
                fea_list.append(float(fea))
            char = data_dict[name]
            feature_dict[char] = fea_list

        self.feature_dict = feature_dict

    def __getitem__(self, index):
        lm, target = self.lmList[index]
        # lm = self.loader(Sequence)
        lm = self.transform(lm,self.feature_dict,self.normalize)
        return lm, target, lm.shape[0]

    def __len__(self):
        return len(self.lmList)