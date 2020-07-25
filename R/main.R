library(jsonlite)
library(dplyr)
library(magrittr)
library(stringr)
library(caret)
library(ggplot2)
# Set the wd to your repo folder
setwd("/Users/stephen.wallace/Documents/Trivia/Rankings/")

# Load in functions
source("R/functions.R")
# Load the data from the json
snackcakes_json <- fromJSON("data/snackcakes.json")

# Create an encoded dataframe
enc_df <- trans_df(snackcakes_json)

# Run principal components analysis on the data.
# Also removes columns with zero variance
pca_model <- preProcess(enc_df[, -1], method = c("zv","pca"), thresh = .95)

# Add the PC values to the dataset
pred_pca <- predict(pca_model, newdata = enc_df)

# Plot the first two components. For snackcakes, this is about 65% of the variance
ggplot(data = pred_pca, aes(x = PC1, y = PC2, label = name)) + geom_point() + 
  theme_bw() +
  geom_text(aes(label=name),hjust=0, vjust=0)
