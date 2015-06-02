# Random_Forest_Multi_Touch_Attribution

This package demonstrates how to assign multi-touch attribution credits using decision tree algorithm
Methodology:

1.) Use decision tree (optionally with bagging for random forest) to learn hierarchy of importance among channels.
2.) The hierarchy and information gain learned from the previous step for each channel are used to assign weighting factor to the credit attribution.
3.) Only channels that causes the exposed population to increase conversion rate receives credit.
4.) If using random forest, final credits for each channel is the average of the results from each individual tree
