# Reds_BatterPerformanceProjection

Sam Rizzuto

Cincinnati Reds

Technical Assessment: 2

21 January 2022


The attached folder, `22-ds`, contains 11 files. Six of those are visualizations (in .png format) that I created to further represent the probability of hits or singles/doubles/triples/HR depending on the graph selected. Two files are the original `train.csv` and `test.csv` files that were given. Along with those are two more csv's called `myTrainDF.csv` and `myTestDF.csv` - these contain all my transformations carried out on the original data sets. In `myTrainDF.csv`, I carried out three different statistical/machine learning methods to calculate the probabilty of a single, double, triple, and home run for each given plate appearance. The columns for these models are labeled with the same structure `probability{HitType}_{ModelType}`. The three models I implemented are: logistic regression, random forest, and SVM, which were formed from the given and created data.

An extra column called `distance` inside `myTrainDF.csv` was calculated based on the angle, exit velocity, and gravity. The calculation was performed with an assumption that air resistance = 0.

In `myTestDF.csv` are the given columns, along with four additional columns displaying the probability of a single, double, triple, and home run.

One thing I would have liked to have information on to implement into my dataset is player location (i.e., whether there is a shift on or not).

Elapsed Time: 8h 23m
