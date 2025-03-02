# Anomaly-Detection-In-Industrial-Control-Systems

CMPT 318 Group Project - Spring 2024

## Group Members

Layan<br>
Nathan<br>
Annie<br>
Kevin

## Project Overview

The following is a technical cybersecurity study focused on performing unsupervised intrusion detection using time series analysis and forecasting in R.

The project consists of four parts:

1. Feature Scaling: This is a preprocessing technique used to normalize the values of the dataset. Its purpose is to ensure all features are in the same range. Examples of these techniques include standardization and normalization.

2. Feature Engineering: Involves the selection of response variables to train the Hidden Markov Models (HMMs). Specifically, using Principal Component Analysis (PCA) to determine the best variables for training from the dataset.

3. HMM Training and Testing: The dataset is split into two: training and testing. Then, multivariate HMMs are trained with multiple states. The best HMMs are selected based on their performance, which compares their log-likelihood values and Bayesian Information Criterion (BIC).

4. Anomaly Detection: The selected trained model is used to examine how far a data sequence from a new data set differs from its behavior. Calculating the log-likelihood showcases the maximum deviation, which can be used to identify anomalous behavior.

