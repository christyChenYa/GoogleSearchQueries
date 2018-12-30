# Google Search Queries
A training set of query id, url id, 10 attributes, and a response variable is provided. 80,046 observations
are included. Binary response variables are labeled as 1 for relevance and 0 for irrelevance. A test data
with 30,001 observations is used for a final evaluation of the project.

The project provides features which determine relevant urls for search queries. The main goal of this
project is to predict the relevance of urls, given specific search queries. I try five different classifiers:
Random Forest, Ada Boosting, Gradient Boosting, Logistic Regression and Support Vector Machine.
Gradient Boosting performs the best based on my experiments. This report presents data observations,
solution evaluation, candidate solutions and corresponding tuning methods.
