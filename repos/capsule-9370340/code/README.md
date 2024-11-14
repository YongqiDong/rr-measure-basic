# Neurons learn by predicting future activity
## Our MLP model on MNIST
To run our basic model (MLP with 1000 hidden neurons on MNIST) CHL_clamped.py, go:

```
python -u CHL_clamped.py "$@"
```

*Currently, number of epochs is set up to 3 (executin time ~7min). You can change number of epochs to 601 (line #343) for full training. <br/>
*if you run it for full training, it will take 21 hours to finish.

## Output of Siumulation on MNIST
After training this network, it will plot learning curves for train and test sets (plotlc.png). The x-axis and y-axis for the learning curves are epoch and training error, respectively. 