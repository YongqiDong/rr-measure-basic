#!/bin/bash

for f in *.ipynb
do
    jupyter nbconvert \
      --ExecutePreprocessor.allow_errors=True \
      --ExecutePreprocessor.timeout=-1 \
      --FilesWriter.build_directory=../results \
      --execute "$f" &
done

wait