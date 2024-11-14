import math
import random
import numpy as np
from torch_geometric.nn import HGTConv, Linear
import torch
import torch.nn.functional as F
from torch_geometric.nn import GCNConv
from torch_geometric.datasets import Planetoid
from torch_geometric.utils import to_networkx
from torch_geometric.data import HeteroData
from torch_geometric.nn import HGTConv, Linear
import sys
import os


def get_mask(all_num, val_rate, test_rate):
    val_num = math.floor(all_num*val_rate)
    test_num = math.floor(all_num*test_rate)
    train_num = all_num - val_num - test_num
    all_mask = np.random.randint(1, size=train_num).tolist() + np.random.randint(1, 2, size=val_num).tolist() + np.random.randint(2, 3, size=test_num).tolist()
    #all_musk = random.shuffle()
    shuffled_mask = random.shuffle(all_mask)
    
    train_mask = torch.tensor([True if item == 0 else False for item in all_mask], dtype=torch.bool)
    val_mask = torch.tensor([True if item == 1 else False for item in all_mask], dtype=torch.bool)
    test_mask = torch.tensor([True if item == 2 else False for item in all_mask], dtype=torch.bool)
    
    return train_mask, val_mask, test_mask



class Logger(object):
    def __init__(self, filename="Default.log"):
        self.terminal = sys.stdout
        self.log = open(filename, "a")

    def write(self, message):
        self.terminal.write(message)
        self.log.write(message)

    def flush(self):
        pass

def save_results():
    type = sys.getfilesystemencoding()
    sys.stdout = Logger("../results/output.txt")