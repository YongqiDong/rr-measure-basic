#!/bin/bash
unzip ../data/shapenetcore_partanno_v0.zip -d ../data
python part_seg/test.py
## uncommont the next line to train the network
# python part_seg/train_multi_gpu.py