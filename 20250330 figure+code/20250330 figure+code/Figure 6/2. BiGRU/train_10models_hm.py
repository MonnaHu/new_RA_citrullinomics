
import torch
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
from torch.utils import data
from torch.nn.utils.rnn import pack_padded_sequence, pad_packed_sequence
import argparse
import numpy as np
from tqdm import tqdm
import sys
import pandas as pd
from dataset import LandmarkList
from model import *  


print(torch.__version__) 

# rnn = 'sumGRU'
# rnn = 'crnn'
# rnn = 'cnn'
# rnn = 'GRU'
# rnn = 'embedGRU'
rnn = 'biGRU_ATTEN'
# rnn = 'LSTM'
HIDDEN_DIM = 64
N_LAYERS_RNN = 3
MAX_EPOCH = 60
cutoff=0.5

atten_masks=[
[1,1,1,1,1,1,1,1,1],
[1,1,1,1,1.5,1,1,1,1],
[1,1,1,1.2,1.5,1.2,1,1,1],
[1,1,1,1,2,1,1,1,1],
[1,1,1,1.5,2,1.5,1,1,1],
[1,1,1,1,3,1,1,1,1],
[1,1,1,2,3,2,1,1,1],
[1,1,1,2,4,2,1,1,1],
[1,1,2,4,5,4,2,1,1],
[1,1,2,4,8,4,2,1,1],
]

EMBEDDING_DIM = 10
LR = 1e-4
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
SAVE_BEST_MODEL = True
result=[]

for test_val_num in range(10):
    MODEL_NAME = 'E' + str(test_val_num)
    train_file = 'data/train_num%d.txt' % (test_val_num)
    val_file = 'data/test_num%d.txt' % (test_val_num)
    atten_m = test_val_num

    def set_seed():
        torch.manual_seed(123)
        torch.backends.cudnn.deterministic = True
        torch.backends.cudnn.benchmark = True
        np.random.seed(123)

    def compute_binary_accuracy(model, data_loader, loss_function):
        total_loss, num_examples, correct_pred, tp, tp_value, fp, fp_value, tn, tn_value, fn, fn_value, tp_fn, tp_fn_value, tp_fp_value = 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.
        model.eval()
        with torch.no_grad():
            for batch, labels, lengths in data_loader:
                logits = model(batch.to(device), lengths)  
                total_loss += loss_function(logits,torch.FloatTensor(labels).unsqueeze(1).to(device)).item()
                predicted_labels = (torch.sigmoid(logits)> cutoff).long()
                
                # accuracy
                num_examples += len(lengths)
                correct_pred += (predicted_labels.squeeze(1).to(device).long() == torch.LongTensor(labels).to(device)).sum()
                accuracy = correct_pred.float().item() / num_examples * 100 if num_examples > 0 else 0
                mask1 = predicted_labels.squeeze(1).to(device).long() == torch.LongTensor(labels).to(device)
                mask2 = torch.LongTensor(labels).to(device) == 1
                mask3 = torch.LongTensor(labels).to(device) == 0
                mask4 = predicted_labels.squeeze(1).to(device).long() != torch.LongTensor(labels).to(device)
                # tp
                mask = [mask1n and mask2n for (mask1n, mask2n) in zip(mask1,mask2)]
                tp += torch.LongTensor(mask).sum()  # 被正确预测为正例的数量(即真正例 TP)
                tp_value = tp.float().item()
                # tp+fn
                tp_fn += mask2.sum()  
                tp_fn_value = tp_fn.float().item()
                recall = tp_value / tp_fn_value * 100 if (tp_fn_value) > 0 else 0
                # tp+fp
                mask = [mask4n and mask3n for (mask4n, mask3n) in zip(mask4, mask3)]
                fp += torch.LongTensor(mask).sum()
                fp_value = fp.float().item()
                tp_fp_value = tp_value + fp_value
                precision = tp_value / tp_fp_value * 100 if tp_fp_value > 0 else 0
                # tn
                mask = [mask1n and mask3n for (mask1n, mask3n) in zip(mask1, mask3)]
                tn += torch.LongTensor(mask).sum() # 被正确预测为负例的数量(即真负例 TN)tn_value = tn.float().item()
                tn_value = tn.float().item()
                # fn
                mask = [mask4n and mask2n for (mask4n, mask2n) in zip(mask4, mask2)]
                fn += torch.LongTensor(mask).sum()
                fn_value = fn.float().item()
            return num_examples, accuracy, recall, precision, total_loss, tp_value,fp_value, tn_value, fn_value
                            
    def pad_collate(batch):
        batch.sort(key=lambda x: x[2], reverse=True)
        lms, tgs, lens = zip(*batch)
        new_lms = torch.zeros((len(lms), lms[0].shape[0], lms[0].shape[1]))  # batch x seq x feature(136)
        new_lms[0] = lms[0]
        for i in range(1, len(lms)):
            new_lms[i] = torch.cat((lms[i].float(), torch.zeros((lens[0] - lens[i]), EMBEDDING_DIM)), 0)
        return new_lms, tgs, lens

    set_seed()
    if rnn == 'frameGRU':
        model = Framewise_GRU_Classifier(EMBEDDING_DIM, HIDDEN_DIM, 1, n_layer=N_LAYERS_RNN)
    if rnn == 'frameCRNN':
        model = FrameCRNN(EMBEDDING_DIM, HIDDEN_DIM, 1, n_layer=N_LAYERS_RNN)
    if rnn == 'sumGRU':
        model = sumGRU(EMBEDDING_DIM, HIDDEN_DIM, 1, n_layer=N_LAYERS_RNN)
    if rnn == 'embedGRU':
        model = embed_GRU_Classifier(EMBEDDING_DIM, HIDDEN_DIM, 1, n_layer=N_LAYERS_RNN)
    if rnn == 'GRU':
        model = GRU_Classifier(EMBEDDING_DIM, HIDDEN_DIM, 1, n_layer=N_LAYERS_RNN)
    if rnn == 'biGRU':
        model = biGRU_Classifier(EMBEDDING_DIM, HIDDEN_DIM, 1, n_layer=N_LAYERS_RNN)
    if rnn == 'biGRU_ATTEN':
        model = biGRU_ATTEN_Classifier(EMBEDDING_DIM, HIDDEN_DIM, 1, n_layer=N_LAYERS_RNN,atten_mask=atten_masks[test_val_num])
    if rnn == 'LSTM':
        model = LSTM_Classifier(EMBEDDING_DIM, HIDDEN_DIM, 1, n_layer=N_LAYERS_RNN)
    if rnn == 'cnn':
        model = cnn_Classifier(EMBEDDING_DIM, HIDDEN_DIM, 1)
    if rnn == 'crnn':
        model = crnn_Classifier(EMBEDDING_DIM, HIDDEN_DIM, 1, n_layer=N_LAYERS_RNN)
    model = model.to(device)

    loss_function = torch.nn.BCEWithLogitsLoss()
    loss_function_eval_sum = torch.nn.BCEWithLogitsLoss(reduction='sum')
    optimizer = optim.Adam(model.parameters(), lr=LR)

    dataset_train = LandmarkList(root='./datasets/', fileList=train_file)
    dataloader_train = data.DataLoader(dataset_train, batch_size=512, shuffle=True, num_workers=0,collate_fn=pad_collate)

    dataset_test = LandmarkList(root='./datasets/', fileList=val_file)
    dataloader_test = data.DataLoader(dataset_test, batch_size=128, shuffle=False, num_workers=0,collate_fn=pad_collate)

    best_test = 0.
    for epoch in tqdm(range(MAX_EPOCH), desc='Epochs', unit='epoch'):
        model.train()
        n_iter = 0
        for batch, labels, lengths in dataloader_train:
            model.zero_grad()
            out = model(batch.to(device),lengths)  # we could do a classifcation for every output (probably better)
            loss = loss_function(out, torch.FloatTensor(labels).unsqueeze(1).to(device))
            loss.backward()
            optimizer.step()
            n_iter += 1

        train_num, train_acc, train_rec, train_prec, train_loss, train_tp, train_fp, train_tn, train_fn = compute_binary_accuracy(model, dataloader_train, loss_function_eval_sum)
        test_num, test_acc, test_rec, test_prec, test_loss, test_tp, test_fp, test_tn, test_fn = compute_binary_accuracy(model, dataloader_test, loss_function_eval_sum)
        train_f1_s = 2 * (train_prec * train_rec) / (train_prec + train_rec) if (train_prec + train_rec)>0 else 0
        test_f1_s = 2 * (test_prec * test_rec) / (test_prec + test_rec) if (test_prec + test_rec) > 0 else 0
       
        result.append([epoch, test_val_num,atten_m,
                       f"{train_acc:.4f}", f"{train_f1_s:.4f}", f"{train_rec:.4f}", f"{train_prec:.4f}", f"{train_loss:.8f}",
                       f"{test_acc:.4f}", f"{test_f1_s:.4f}", f"{test_rec:.4f}", f"{test_prec:.4f}", f"{test_loss:.8f}",
                       f"{train_num:.4f}", f"{train_tp:.4f}", f"{train_fp:.4f}", f"{train_tn:.4f}", f"{train_fn:.4f}",
                       f"{test_num:.4f}", f"{test_tp:.4f}", f"{test_fp:.4f}", f"{test_tn:.4f}", f"{test_fn:.4f}"
                      ])
        columns = ['epoch', 'dataset','atten_masks','train_acc', 'train_f1_score', 'train_recall', 'train_precision','train_loss',
                   'test_acc', 'test_f1_score','test_recall', 'test_precision', 'test_loss',
                   'train_num', 'train_tp', 'train_fp', 'train_tn', 'train_fn','test_num', 'test_tp', 'test_fp', 'test_tn', 'test_fn']
        df_result = pd.DataFrame(result, columns=columns)

        test_acc=(test_acc+test_recall)/2
        if test_acc > best_test:
            best_test = test_acc
            if SAVE_BEST_MODEL:
                torch.save(model.state_dict(), './' + rnn + '_attenmask' + str(atten_m)  + '_LAYER' + str(N_LAYERS_RNN) + '_HIDDEN' + str(HIDDEN_DIM) + '_' + MODEL_NAME + '_epoch' + str(epoch) + '.pt')
                print('best epoch {}, train_acc {}, test_acc {}'.format(epoch, train_acc, test_acc))

df_result.to_csv('./All' + rnn + '_attenmask' + str(atten_m) + '_LAYER' + str(N_LAYERS_RNN) + '_HIDDEN' + str(HIDDEN_DIM) + '_' + MODEL_NAME + '.csv', index=False)

            

