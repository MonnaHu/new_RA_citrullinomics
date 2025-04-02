import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import Dataset, DataLoader
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, confusion_matrix


# 定义Transformer模型
class TransformerClassifierv2(nn.Module):
    def __init__(self, input_dim, model_dim, num_heads, num_encoder_layers, num_classes):
        super(TransformerClassifierv2, self).__init__()
        self.model_dim = model_dim
        self.transformer_encoder = nn.TransformerEncoder(
            nn.TransformerEncoderLayer(d_model=model_dim, nhead=num_heads),
            num_layers=num_encoder_layers
        )
        self.input_projection = nn.Linear(input_dim, model_dim)
        self.classifier = nn.Linear(model_dim, num_classes)

    def forward(self, src):
        # 将输入投影到模型维度
        src = self.input_projection(src) * (self.model_dim ** 0.5)  # 缩放嵌入
        # 在最后一个时间步上应用Transformer编码器
        output = self.transformer_encoder(src.permute(1, 0, 2)).permute(1, 0, 2)  # B x S x D
        # 取序列的最后一个时间步的输出
        output = output[:, -1, :]
        # 应用分类器
        output = self.classifier(output)
        return output

