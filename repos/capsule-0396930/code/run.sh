#!/usr/bin/env bash

# Setup GERNERMED component package
python3 -m pip install ./de_GERNERMED-1.0.0.tar.gz

# Run evaluation test
python3 -m spacy evaluate /data/gernermed_pipeline /data/ner_medical.test.spacy -o /results/eval_scores.json

# Run annotation demo
python3 /code/example_simple.py > /results/annotation_example.txt