import xgboost as xgb
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score,recall_score,precision_score,f1_score,confusion_matrix
from sklearn.preprocessing import LabelEncoder
import torch
import pandas as pd
SLIP_SIZE=9
def FNormalizeMult(data,normalize):
    data = np.array(data)
    for i in range(0, data.shape[1]):
        listlow = normalize[i, 0]
        listhigh = normalize[i, 1]
        delta = listhigh - listlow
        if delta != 0:
            data[:, i] = (data[:, i] - listlow) / delta
    return data

def default_list_reader(fileList):
    lmList = []
    with open(fileList, 'r') as file:
        for line in file.readlines():
            tmp = line.strip("\n").split(" ")
            lmPath = tmp[0]
            label_type = tmp[1]
            if len(lmPath)<=SLIP_SIZE:
                lmList.append((lmPath, int(label_type)))
            else:
                for idx in range(0, len(lmPath) - SLIP_SIZE):
                    seq = lmPath[idx:(SLIP_SIZE + idx)]
                    lmList.append((seq,int(label_type)))
    return lmList   

def slip_list_reader(fileList):
    lmList = []
    origin_seqs=[]
    with open(fileList, 'r') as file:
        for line in file.readlines():
            tmp = line.strip("\n").split(" ")
            if len(tmp)==3:
                lmPath = tmp[0]
                label_type = tmp[1]
                origin_seq = tmp[2]
            else:
                lmPath = tmp[0]
                label_type = tmp[1]
                origin_seq = tmp[0]
            if len(lmPath)<=SLIP_SIZE:
                lmList.append((lmPath, int(label_type)))
                origin_seqs.append(origin_seq)
            else:
                for idx in range(0, len(lmPath) - SLIP_SIZE):
                    seq = lmPath[idx:(SLIP_SIZE + idx)]
                    lmList.append((seq,int(label_type)))
    return lmList,origin_seqs

def default_transform(sequence,feature_dict,normalize):
    feature_map = []
    for char in sequence:
        try:
            feature_map.append(feature_dict[char])
        except:
            continue
    feature_array = np.array(feature_map)
    data = FNormalizeMult(feature_array, normalize)
    return data

train_file = './data/train_num7.txt'
test_file = './data/test_num7.txt'
test_file_out = "./data/39aa_actual.txt"#'./data/out_val.txt'#'./data/outtest_all_pos0.txt'
lmList   = default_list_reader(train_file)
lmList_test   = default_list_reader(test_file)
lmList_test_out   = slip_list_reader(test_file_out)[0]#slip_list_reader(test_file_out)

normalize = np.load("./data/normalize.npy")
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
    

X_3d=[]
y=[]
n_samples=len(lmList)
for index in range(n_samples):
    lm, target=lmList[index]
    lm = default_transform(lm,feature_dict,normalize)
    X_3d.append(lm)
    y.append(target)

X_3d=np.array(X_3d)
y=np.array(y)

X_2d = X_3d.reshape(n_samples, -1)

X_3d_test=[]
y_test=[]
n_samples_test=len(lmList_test)
for index in range(n_samples_test):#[index]
    lm, target=lmList_test[index]
    lm = default_transform(lm,feature_dict,normalize)
    X_3d_test.append(lm)
    y_test.append(target)

X_3d_test=np.array(X_3d_test)
y_test=np.array(y_test)
X_2d_test = X_3d_test.reshape(n_samples_test, -1)

X_3d_test_out=[]
y_test_out=[]
n_samples_test_out=len(lmList_test_out)
for index in range(n_samples_test_out):#[index]
    lm, target=lmList_test_out[index]
    lm = default_transform(lm,feature_dict,normalize)
    X_3d_test_out.append(lm)
    y_test_out.append(target)
    
X_3d_test_out=np.array(X_3d_test_out)
y_test_out=np.array(y_test_out)
X_2d_test_out = X_3d_test_out.reshape(n_samples_test_out, -1)

dtrain = xgb.DMatrix(X_2d, label=y)
dval = xgb.DMatrix(X_2d_test, label=y_test)
dtest = xgb.DMatrix(X_2d_test_out, label=y_test_out)

params = {
    'objective': 'binary:logistic',
    'eval_metric': 'logloss',
    'max_depth': 5,
    'eta': 0.1,
    'seed': 42
}

result = []
for round in range(10):

    num_round = 400+round*50
    #bst = xgb.train(params, dtrain, num_round)
    evals = [(dtrain, 'train'), (dtest, 'test')]
    evals_result = {}
    bst = xgb.train(params, dtrain, num_round)

    y_pred_prob = bst.predict(dval)
    y_pred = [1 if prob > 0.8 else 0 for prob in y_pred_prob]

    test_accuracy = accuracy_score(y_test, y_pred)
    test_f1_s = f1_score(y_test, y_pred)
    test_precision = precision_score(y_test, y_pred)
    test_recall = recall_score(y_test, y_pred)
    df_test = pd.DataFrame({'True Label': y_test,'Predicted Probability': y_pred})
    df_test.to_csv('XGBoost test 39' + '_round' + str(num_round) + '.csv', index=False)
    

    y_out_pred_prob = bst.predict(dtest)
    y_out_pred = [1 if prob > 0.8 else 0 for prob in y_out_pred_prob] 
    ex_accuracy = accuracy_score(y_test_out, y_out_pred)
    ex_recall=recall_score(y_test_out, y_out_pred)
    ex_f1_s = f1_score(y_test_out, y_out_pred)
    ex_precision = precision_score(y_test_out, y_out_pred)
    #ex_tn, ex_fp, ex_fn, ex_tp=confusion_matrix(y_test_out_origin, y_out_pred)
    df_ex = pd.DataFrame({'True Label': y_test_out,'Predicted Probability': y_out_pred})
    df_ex.to_csv('XGBoost external 39' + '_round' + str(num_round) + '.csv', index=False)
    
    
    result.append([num_round, f"{test_accuracy:.4f}", f"{test_f1_s:.4f}", f"{test_precision:.4f}", f"{test_recall:.4f}",f"{ex_accuracy:.4f}", f"{ex_f1_s:.4f}", f"{ex_precision:.4f}", f"{ex_recall:.4f}"])
    columns = ['num_round','test_acc', 'test_f1_score', 'test_precision','test_recall','ex_acc', 'ex_f1_score', 'ex_precision','ex_recall']
    df_result = pd.DataFrame(result, columns=columns)
    print(f'testing Accuracy: {test_accuracy:.4f}',f'testing recall: {test_recall:.4f}', f'external Accuracy: {ex_accuracy:.4f}',f'external recall: {ex_recall:.4f}')
df_result.to_csv('AllXGBoost 39' + '_round' + str(num_round) + '.csv', index=False)
