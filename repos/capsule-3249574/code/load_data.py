import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import torch
from sentence_transformers import SentenceTransformer
import networkx as nx
import os

def label_to_num(label):
    if label in ['E13', 'TFP'] :
        num = 1
    elif label in ['INT', 'FSF', 'TWT']:
        num = 2
    else:
        num = 3
    return num

def load_node_csv(paths, index_col, node_type, encoders=None, **kwargs):
    df = pd.read_csv(paths[0], index_col=index_col, encoding='ISO-8859-1',**kwargs)
    print('loading nodes:', paths[0])
    
    for i in range(len(paths)-1):
        df_single_dataset = pd.read_csv(paths[i+1], index_col=index_col, encoding='ISO-8859-1',**kwargs)
        print('loading nodes:', paths[i+1])
        df = df.append(df_single_dataset)
    
    item_id = df.index
    mapped_item_id = {index: i for i, index in enumerate(item_id.unique())}
    
    y = None
    if node_type == 'user':
        labelEncoder = LabelEncoder()
        y = labelEncoder(df['dataset'])
        #print(df['dataset'])
        
    x = None
    if encoders is not None:
        xs = [encoder(df[col]) for col, encoder in encoders.items()]
        x = torch.cat(xs, dim=-1)

    return x, mapped_item_id, y

class SequenceEncoder(object):
    def __init__(self, model_name='all-MiniLM-L6-v2', device=None):
        self.device = device
        self.model = SentenceTransformer(model_name, device=device)

    @torch.no_grad()
    def __call__(self, df):
        df = df.fillna(' ')
        x = self.model.encode(df.values, show_progress_bar=True, convert_to_tensor=True, device=self.device)
        return x.cpu()
    

class NumberEncoder(object):
    def __call__(self, df):
        df = df.fillna(0)
        x = torch.zeros(len(df), 1)
        i = 0
        for item in df:
            x[i][0] = item
            i = i+1
        return x
       

class LabelEncoder(object):

    def __call__(self, df):
        y = torch.zeros(len(df), 1)
        i = 0
        for item in df:
            y[i][0] = label_to_num(str(item))
            i = i+1
        return y







class IdentityEncoder(object):
    def __init__(self, dtype=None):
        self.dtype = dtype
    
    def __call__(self, df):
        return torch.from_numpy(df.values).view(-1, 1).to(self.dtpye)