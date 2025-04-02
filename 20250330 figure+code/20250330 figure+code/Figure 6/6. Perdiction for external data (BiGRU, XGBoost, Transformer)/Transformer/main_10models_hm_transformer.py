import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import Dataset, DataLoader
import pandas as pd
from torch.utils import data
import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score
# from model import TransformerClassifierv2,TransformerClassifier,TransformerClassifierv3
from model import TransformerClassifierv2
# from dataset import ECGDataset
from dataset import LandmarkList

EMBEDDING_DIM = 10
def train(model, train_loader, criterion, optimizer, device):
    model.train()
    running_loss = 0.0
    correct_predictions = 0
    total_predictions = 0
    num_examples=0
    tp, tp_fn, fp, tn, fn = 0,0,0,0,0.
    for batch, labels,lengths in train_loader:
        optimizer.zero_grad()
        outputs = model(batch.to(device))
        loss = criterion(outputs, torch.tensor(labels).to(device))
        loss.backward()
        optimizer.step()
        running_loss += loss.item() * batch.size(0)
        _, preds = torch.max(outputs, 1)
        correct_predictions += torch.sum(preds == torch.tensor(labels).to(device))
        total_predictions += torch.tensor(labels).to(device).size(0)
        
        num_examples += len(lengths)
        mask1 = preds == torch.tensor(labels).to(device)
        mask2=torch.LongTensor(labels).to(device)==1
        mask3=torch.LongTensor(labels).to(device)==0
        mask4 = preds != torch.tensor(labels).to(device)
        
        mask=[mask1n and mask2n for (mask1n,mask2n) in zip(mask1,mask2) ]
        tp += torch.LongTensor(mask).sum()
        tp_value = tp.float().item()
        tp_fn += mask2.sum()
        tp_fn_value = tp_fn.float().item()
        recall = tp_value / tp_fn_value * 100 if (tp_fn_value) > 0 else 0

        mask = [mask4n and mask3n for (mask4n, mask3n) in zip(mask4, mask3)]
        fp += torch.LongTensor(mask).sum()
        fp_value = fp.float().item()
        tp_fp_value = tp_value + fp_value
        precision = tp_value / tp_fp_value * 100 if tp_fp_value > 0 else 0

        mask = [mask1n and mask3n for (mask1n, mask3n) in zip(mask1, mask3)]
        tn += torch.LongTensor(mask).sum()
        tn_value = tn.float().item()

        mask = [mask4n and mask2n for (mask4n, mask2n) in zip(mask4, mask2)]
        fn += torch.LongTensor(mask).sum()
        fn_value = fn.float().item()
    
    epoch_loss = running_loss / len(train_loader.dataset)
    epoch_acc = correct_predictions.double() / total_predictions
    return epoch_loss, epoch_acc.item(), num_examples, recall, precision, tp_value, tn_value, fp_value, fn_value

def evaluate(model, test_loader, criterion, device):
    model.eval()
    test_loss = 0.0
    correct_predictions = 0
    total_predictions = 0
    num_examples=0
    tp, tp_fn, fp, tn, fn = 0,0,0,0,0.
    with torch.no_grad():
        for batch, labels,lengths in test_loader:
            outputs = model(batch.to(device))
            loss = criterion(outputs, torch.tensor(labels).to(device))
            test_loss += loss.item() * batch.to(device).size(0)
            _, preds = torch.max(outputs, 1)
            correct_predictions += torch.sum(preds == torch.tensor(labels).to(device))
            total_predictions += torch.tensor(labels).to(device).size(0)
            
            num_examples += len(lengths)
            mask1 = preds == torch.tensor(labels).to(device)
            mask2=torch.LongTensor(labels).to(device)==1
            mask3=torch.LongTensor(labels).to(device)==0
            mask4 = preds != torch.tensor(labels).to(device)
        
            mask=[mask1n and mask2n for (mask1n,mask2n) in zip(mask1,mask2) ]
            tp += torch.LongTensor(mask).sum()
            tp_value = tp.float().item()
        
            tp_fn += mask2.sum()
            tp_fn_value = tp_fn.float().item()
            recall = tp_value / tp_fn_value * 100 if (tp_fn_value) > 0 else 0

            mask = [mask4n and mask3n for (mask4n, mask3n) in zip(mask4, mask3)]
            fp += torch.LongTensor(mask).sum()
            fp_value = fp.float().item()
            tp_fp_value = tp_value + fp_value
            precision = tp_value / tp_fp_value * 100 if tp_fp_value > 0 else 0

            mask = [mask1n and mask3n for (mask1n, mask3n) in zip(mask1, mask3)]
            tn += torch.LongTensor(mask).sum()
            tn_value = tn.float().item()

            mask = [mask4n and mask2n for (mask4n, mask2n) in zip(mask4, mask2)]
            fn += torch.LongTensor(mask).sum()
            fn_value = fn.float().item()
        
        epoch_loss = test_loss / len(test_loader.dataset)
        epoch_acc = correct_predictions.double() / total_predictions
        return epoch_loss, epoch_acc.item(), num_examples, recall, precision, tp_value, tn_value, fp_value, fn_value


def pad_collate(batch):
    batch.sort(key=lambda x: x[2], reverse=True)
    lms, tgs, lens = zip(*batch)
    new_lms = torch.zeros((len(lms), lms[0].shape[0], lms[0].shape[1]))
    new_lms[0] = lms[0]
    for i in range(1, len(lms)):
        new_lms[i] = torch.cat((lms[i].float(), torch.zeros((lens[0] - lens[i]),EMBEDDING_DIM)), 0)
    return new_lms, tgs, lens

SLIP_SIZE=9
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

result=[]
def main():
    device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
    result =[]
    for test_val_num in [7]:
        MODEL_NAME='E'+str(test_val_num)
        train_file='data/train_num%d.txt'%(test_val_num)
        val_file='data/test_num%d.txt'%(test_val_num)

        dataset_train = LandmarkList(root='./datasets/', fileList=train_file)
        train_loader = DataLoader(dataset_train, batch_size=512, shuffle=True, num_workers=0, collate_fn=pad_collate)
        dataset_val = LandmarkList(root='./datasets/', fileList=val_file)
        val_loader = DataLoader(dataset_val, batch_size=128, shuffle=False, num_workers=0, collate_fn=pad_collate)
        input_dim = 10
        sequence_length=16
        d_model = 256#16#128
        nhead = 8#8
        num_layers = 3#6
        num_classes = 2
        print("#########################################################################################")
        print(num_layers, nhead, d_model)
        for j in range(1):
            model = TransformerClassifierv2(input_dim, d_model, nhead, num_layers, num_classes).to(device)
            criterion = nn.CrossEntropyLoss()

            #criterion = torch.nn.BCEWithLogitsLoss()

            optimizer = optim.Adam(model.parameters(), lr=0.0001)
            f1=open('./train_log.txt','w+')

            num_epochs = 60#50
            max = 0
            for epoch in range(num_epochs):
                train_loss, train_acc, train_num, train_rec, train_prec, train_tp, train_tn, train_fp, train_fn = train(model, train_loader, criterion, optimizer, device)
                test_loss, test_acc, test_num, test_rec, test_prec, test_tp, test_tn, test_fp, test_fn = evaluate(model, val_loader, criterion, device)
                test_f1_s = 2*test_rec*test_prec/(test_prec+test_rec)
                # print(f'Epoch {epoch + 1}/{num_epochs}')
                # print(f'Train Loss: {train_loss:.4f} Acc: {train_acc:.4f}')
                # print(f'Test Loss: {val_loss:.4f} Acc: {val_acc:.4f}')
                print(f'{epoch}_{test_num:.4f}_{test_acc:.4f}_{test_f1_s:.4f}_{test_rec:.4f}_{test_prec:.4f}_{test_tp:.4f}_{test_fp:.4f}_{test_fn:.4f}')
                f1.write(f'{epoch}_{test_num:.4f}_{test_acc:.4f}_{test_f1_s:.4f}_{test_rec:.4f}_{test_prec:.4f}_{test_tp:.4f}_{test_fp:.4f}_{test_fn:.4f}\n')
                
                result.append([epoch, test_val_num, f"{train_num:.4f}", f"{train_acc:.4f}", f"{train_loss:.4f}", f"{train_rec:.4f}", f"{train_prec:.4f}",
                               f"{train_tp:.4f}", f"{train_tn:.4f}", f"{train_fp:.4f}", f"{train_fn:.4f}", 
                               f"{test_num:.4f}", f"{test_acc:.4f}", f"{test_loss:.4f}", f"{test_rec:.4f}",  f"{test_prec:.4f}",
                               f"{test_tp:.4f}", f"{test_tn:.4f}", f"{test_fp:.4f}", f"{test_fn:.4f}"])
                columns = ['epoch', 'dataset', 'train_num', 'train_acc', 'train_loss','train_recall', 'train_precision',
                           'train_tp', 'train_tn','train_fp', 'train_fn',
                           'test_num', 'test_acc', 'test_loss','test_recall', 'test_precision',
                           'test_tp', 'test_tn','test_fp', 'test_fn']
                df_result = pd.DataFrame(result, columns=columns)
                torch.save(model.state_dict(), './transformer' + '_LAYER' + str(num_layers) + '_head' + str(nhead) + '_dmodel' + str(d_model) + '_' + MODEL_NAME + '_epoch' + str(epoch) + '.pt')
                
                score = (test_acc+test_rec)/2
                if score > max:
                    torch.save(model.state_dict(),'./model/' + str(d_model) + '_' + str(nhead) + str(num_layers) + 'final_all1.pt')
                    max = score
            f1.close()
            for test_file in ['./data/39_slip_9aa.txt','./data/external_slip_9aa.txt']:
                dataset_test = LandmarkList(root='./datasets/', fileList=test_file)
                test_loader = DataLoader(dataset_test, batch_size=128, shuffle=False, num_workers=0,collate_fn=pad_collate)
                lmList,test_origin_seqs=slip_list_reader(test_file)

                y_true = []
                y_pred = []
                with torch.no_grad():
                    for batch, labels,lengths  in test_loader:
                        #inputs, labels = inputs.float().to(device), labels.to(device)
                        outputs = model(batch.to(device))
                        _, preds = torch.max(outputs, 1)
                        y_true.extend(torch.tensor(labels).to(device).cpu().numpy())
                        y_pred.extend(preds.cpu().numpy())
                n_samples_test_out=len(y_pred)
                test_origin_seqs2prob = {}
                test_origin_seqs2label = {}
                for i in range(n_samples_test_out):
                    if test_origin_seqs[i] not in test_origin_seqs2prob:
                        test_origin_seqs2prob[test_origin_seqs[i]] = 0.0
                    if y_pred[i] > test_origin_seqs2prob[test_origin_seqs[i]]:
                        test_origin_seqs2prob[test_origin_seqs[i]] = y_pred[i]
                    test_origin_seqs2label[test_origin_seqs[i]]=y_true[i]

                for origin_seq, max_prob in test_origin_seqs2prob.items():
                    if max_prob > 0.5:
                        test_origin_seqs2prob[origin_seq] = 1
                    else:
                        test_origin_seqs2prob[origin_seq] = 0
                y_true_origin = []
                y_pred_origin = []
                for seq, label in test_origin_seqs2label.items():
                    y_true_origin.append(label)
                    y_pred_origin.append(test_origin_seqs2prob[seq])
                y_true=y_true_origin
                y_pred=y_pred_origin
                acc = accuracy_score(y_true, y_pred)
                pre = precision_score(y_true, y_pred)
                rec = recall_score(y_true, y_pred)
                f1 = f1_score(y_true, y_pred)
                
                file_name = test_file.split('/')[2]
                file_name = file_name.split('.')[0]
                df = pd.DataFrame({'True Label': y_true,'Predicted Probability': y_pred, 'test_file': file_name})
                df.to_csv('./Alltransformer' + '_LAYER' + str(num_layers) + '_head' + str(nhead) + '_dmodel' + str(d_model) + '_' + MODEL_NAME + '_epoch' +str(epoch) + file_name +'.csv', index=False)
                
                print(test_file)
                print(f'Accuracy: {acc:.4f}')
                print(f'Precision: {pre:.4f}')
                print(f'Recall: {rec:.4f}')
                print(f'F1 Score: {f1:.4f}')
    df_result.to_csv('./Alltransformer' + '_LAYER' + str(num_layers) + '_head' + str(nhead) + '_dmodel' + str(d_model) + '_' + MODEL_NAME + '.csv', index=False)
if __name__ == '__main__':
    main()
