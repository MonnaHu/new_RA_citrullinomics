import pandas as pd  # 导入pandas库并简写为pd,pandas一个开源的Python库，用于数据分析和数据处理
import numpy as np
import os

# 创建字典
data_dict = {"瓜氨酸": "X", "甘氨酸": "G", "丙氨酸": "A", "缬氨酸": "V", "亮氨酸": "L", "异亮氨酸": "I", "苯丙氨酸": "F", "色氨酸": "W",
                     "酪氨酸": "Y", "天冬氨酸": "D", "谷氨酸": "E", "赖氨酸": "K", "天冬酰胺": "N",
                     "谷氨酰胺": "Q", "甲硫氨酸": "M", "丝氨酸": "S", "苏氨酸": "T", "半胱氨酸": "C", "脯氨酸": "P",
                     "组氨酸": "H", "精氨酸": "R"}

# XXdict.values 返回字典中的所有值
# list() 将元祖转换成列表
char_list=list(data_dict.values())

# def 开始创建一个函数
# cal_max_common_len()创建的函数的名字
# str1和str2，这个函数里的两个变量
def cal_max_common_len(str1,str2):
    str1,str2=(str2,str1) if len(str1)>len(str2) else (str1,str2)
    f=[]
    for i in range(len(str1),9,-1):
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

def check_seq_valid(sequence,min_len=9):
    if len(sequence)<min_len:
        return False
    for s in sequence:
        if s not in char_list:
            return False
    return True

def convert_xlsx_to_txt(xlsx_path,data_row_idx=0,txt_path=None,label_type=1,sheet_name=0):

    df = pd.read_excel(xlsx_path,sheet_name=sheet_name)
    idx_list=[]
    data_all = df.values
    s_poss = []
    s_zmy=[]
    for data in data_all:
        #这里做输入检查

        if not isinstance(data[data_row_idx-1],str): # isinstance()判断变量的类型是不是字符串
            # 如果不满足上述条件，也就是变量不是字符串，则去除这个变量，这样做是因为表格中有的是一串数字，就运行以下代码
            idx_list.append(0)
            print("skip invalid string {}".format(data[data_row_idx-1]))
            continue
        sequence = data[data_row_idx-1].strip()
        valid=check_seq_valid(sequence)
        if not valid:
            idx_list.append(0)
            print("skip invalid string {}".format(sequence))
            continue
        try:
            s_zmy.append(data[3].strip())  # 在列表末尾添加新的对象
        except:
            s_zmy.append("")
        idx_list.append(1)
        s_poss.append(sequence)
    print("Collect valid test sequense:{}".format(str(len(s_poss))))
    if not txt_path:
        txt_path=xlsx_path.replace(".xlsx",".txt")
    f2 = open(txt_path, "w+")
    if len(s_poss)<1:
        return None
    for pos in s_poss[:-1]:
        f2.write("{} 1\n".format(pos))
    f2.write("{} 1".format(s_poss[-1]))
    return txt_path,idx_list,s_zmy








