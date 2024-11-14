import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import torch
import torch.nn.functional as F
from torch_geometric.nn import GCNConv
from torch_geometric.datasets import Planetoid
from torch_geometric.utils import to_networkx
from torch_geometric.data import HeteroData
from torch_geometric.nn import HGTConv, Linear
from sklearn.metrics import precision_score, recall_score, f1_score, roc_auc_score, roc_curve
from warnings import simplefilter
import networkx as nx
import os
import load_data
import utils
os.environ['KMP_DUPLICATE_LIB_OK'] = 'TRUE'
os.environ['CUDA_LAUNCH_BLOCKING'] = '1'
simplefilter(action='ignore', category=FutureWarning)

class HGT(torch.nn.Module):
    def __init__(self, hidden_channels, out_channels, num_heads, num_layers):
        super().__init__()

        self.lin_dict = torch.nn.ModuleDict()
        for node_type in data.node_types:
            self.lin_dict[node_type] = Linear(-1, hidden_channels)

        self.convs = torch.nn.ModuleList()
        for _ in range(num_layers):
            conv = HGTConv(hidden_channels, hidden_channels, data.metadata(),
                           num_heads, group='sum')
            self.convs.append(conv)

        self.lin = Linear(hidden_channels, out_channels)

    def forward(self, x_dict, edge_index_dict):
        x_dict = {
            node_type: self.lin_dict[node_type](x).relu_()
            for node_type, x in x_dict.items()
        }

        for conv in self.convs:
            x_dict = conv(x_dict, edge_index_dict)

        return self.lin(x_dict['user'])

# loading edges

def load_edge_csv(paths, edge_type, mapped_dst_id_type, src_index_col, src_id, dst_index_col, dst_id, encoders=None, **kwargs):
    df = pd.read_csv(paths[0], dtype={'id':np.int64}, encoding='ISO-8859-1')
    print('loading edges:', paths[0])
    for i in range(len(paths)-1):
        df_single_dataset = pd.read_csv(paths[i+1], dtype={'id':np.int64},encoding='ISO-8859-1')
        print('loading edges:', paths[i+1])
        df = df.append(df_single_dataset)
    df.insert(2, 'type', edge_type)
    
    #print(df.size)
    
    df = df.loc[df[src_index_col].isin(src_id)]
    df = df.loc[df[dst_index_col].isin(dst_id)]
    
    #df[dst_index_col] = df[dst_index_col].astype('int64')
    src = [user_id[index] for index in df[src_index_col]]
    #print(df[dst_index_col])
    dst = [mapped_dst_id_type[index] for index in df[dst_index_col]]
    #dst = [mapped_dst_id_type.get(index) for index in df[dst_index_col]]
    edge_index = torch.tensor([src, dst])
    
    edge_attr = None
    if encoders is not None:
        edge_attrs = [encoder(df[col]) for col, encoder in encoders.items()]
        edge_attr = torch.cat(edge_attrs, dim=-1)
    
    return edge_index.long(), edge_attr.long()



def train():
    model.train()
    optimizer.zero_grad()
    out = model(data.x_dict, data.edge_index_dict)
    mask = data['user'].train_mask
    loss = F.cross_entropy(out[mask], data['user'].y[mask].squeeze(1).long())
    loss.backward()
    optimizer.step()
    return float(loss)


@torch.no_grad()
def test():
    model.eval()
    pred = model(data.x_dict, data.edge_index_dict).argmax(dim=-1)
    prob = F.softmax(torch.FloatTensor(pred.float().cpu()), dim=0)
    
    accs = []
    prc_scores = []
    rca_scores = []
    f1_scores = []
    #auc_scores = []

    for split in ['train_mask', 'val_mask', 'test_mask']:
        mask = data['user'][split]
        acc = (pred[mask] == data['user'].y[mask].squeeze(1).long()).sum() / mask.sum()
        accs.append(float(acc))
        
        prc = precision_score(data['user'].y[mask].squeeze(1).long().cpu().numpy(), pred[mask].cpu().numpy(), average='macro')
        prc_scores.append(float(prc))
                              
        rca = recall_score(data['user'].y[mask].squeeze(1).long().cpu().numpy(), pred[mask].cpu().numpy(), average='macro')
        rca_scores.append(float(rca))                      
                              
        f1 = f1_score(data['user'].y[mask].squeeze(1).long().cpu().numpy(), pred[mask].cpu().numpy(), average='macro')
        f1_scores.append((f1))
        
        #auc = roc_auc_score(data['user'].y[mask].squeeze(1).long().cpu().numpy(), prob[mask])
        #auc_scores.append(auc)
        
    return accs, prc_scores, rca_scores, f1_scores





if __name__ == '__main__':

    utils.save_results()

    followers_path = ['../data/TWT.csv/followers.csv', '../data/TFP.csv/followers.csv']
    friends_path = ['../data/TWT.csv/friends.csv', '../data/TFP.csv/friends.csv']
    tweets_path = ['../data/TWT.csv/tweets.csv', '../data/TFP.csv/tweets.csv']
    users_path = ['../data/TWT.csv/users.csv', '../data/TFP.csv/users.csv']

    # loading user nodes
    user_x, user_id, user_label = load_data.load_node_csv(
        users_path, index_col='id', node_type='user', encoders={
            'name': load_data.SequenceEncoder(),
            'statuses_count': load_data.NumberEncoder(),
            'followers_count': load_data.NumberEncoder(),
            'friends_count': load_data.NumberEncoder(),
            'favourites_count': load_data.NumberEncoder(),
            'description': load_data.SequenceEncoder()
        })
    #print(user_x.size())
    #print(user_x)

    # loading tweet nodes
    
    tweet_x, tweet_id, tweet_label = load_data.load_node_csv(
            tweets_path, index_col='id', node_type='tweet', encoders={
            'text': load_data.SequenceEncoder(),
            #'source':SequenceEncoder(),
            'retweet_count': load_data.NumberEncoder(),
            'reply_count': load_data.NumberEncoder(),
            'favorite_count': load_data.NumberEncoder(),
            'num_urls': load_data.NumberEncoder(),
            'num_mentions': load_data.NumberEncoder()
        })
    #print(tweet_x.size())
    #print(tweet_x)
    '''
    #only for debug
    #print(list(user_id.items())[:10])
    #print(list(tweet_id.items())[:10])
    torch.save(user_x, './user_x.pth')
    torch.save(user_id, './user_id.pth')
    torch.save(user_label, './user_label.pth')
    torch.save(tweet_x, './tweet_x.pth')
    torch.save(tweet_id, './tweet_id.pth')
    torch.save(tweet_label, './tweet_label.pth')

    user_x = torch.load('./user_x.pth')
    user_id = torch.load('./user_id.pth')
    user_label = torch.load('./user_label.pth')
    tweet_x = torch.load('./tweet_x.pth')
    tweet_id = torch.load('./tweet_id.pth')
    tweet_label = torch.load('./tweet_label.pth')
    '''
    train_mask, val_mask, test_mask = utils.get_mask(user_x.size()[0], 0.1, 0.1)

    data = HeteroData()

    data['user'].x = user_x
    data['user'].y = user_label
    data['user'].train_mask = train_mask
    data['user'].val_mask = val_mask
    data['user'].test_mask = test_mask
    data['tweet'].x = tweet_x


    follow_edge_index, follow_edge_label = load_edge_csv(
        followers_path,
        edge_type=1,
        mapped_dst_id_type=user_id,
        src_index_col='source_id',
        src_id=user_id,
        dst_index_col='target_id',
        dst_id=user_id,
        encoders={'type': load_data.NumberEncoder()},
    )
    #print(follow_edge_index)
    #print(follow_edge_index.shape)
    #print(follow_edge_label)


    friend_edge_index, friend_edge_label = load_edge_csv(
        friends_path,
        edge_type=2,
        mapped_dst_id_type=user_id,
        src_index_col='source_id',
        src_id=user_id,
        dst_index_col='target_id',
        dst_id=user_id,
        encoders={'type': load_data.NumberEncoder()},
    )
    #print(friend_edge_index)
    #print(friend_edge_index.shape)
    #print(friend_edge_label)


    creat_edge_index, creat_edge_label = load_edge_csv(
        tweets_path,
        edge_type=-1,
        mapped_dst_id_type=tweet_id,
        src_index_col='user_id',
        src_id=user_id,
        dst_index_col='id',
        dst_id=tweet_id,
        encoders={'type': load_data.NumberEncoder()},
    )
    #print(creat_edge_index)


    data['user', 'follow', 'user'].edge_index = follow_edge_index
    data['user', 'follow', 'user'].edge_label = follow_edge_label
    data['user', 'create', 'tweet'].edge_index = creat_edge_index
    data['user', 'create', 'tweet'].edge_label = creat_edge_label

    #print(data)
    #print(data.x_dict)
    #print(data.y_dict)
    #print(data['user', 'follow', 'user'].edge_index)
    #print(data['user', 'create', 'tweet'].edge_label.dtype)
    #print(data['user', 'follow', 'user'].edge_label.dtype)
    #print(data['user', 'follow', 'user'].edge_label)

    model = HGT(hidden_channels=64, out_channels=6, num_heads=2, num_layers=2)
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    data, model = data.to(device), model.to(device)


    with torch.no_grad():  # Initialize lazy modules.
        out = model(data.x_dict, data.edge_index_dict)

    optimizer = torch.optim.Adam(model.parameters(), lr=0.002, weight_decay=0.001)
    
    for epoch in range(1, 201):
        loss = train()
        accs, prc_scores, rca_scores, f1_scores = test()
        print(f'Epoch: {epoch:03d}, Loss: {loss:.5f}, Train: {accs[0]:.5f}, Val: {accs[1]:.5f},'
            f'Test: {accs[2]:.5f}, Precision: {prc_scores[2]:.5f}, Recall: {rca_scores[2]:.5f}, F1-score: {f1_scores[2]:.5f}')

