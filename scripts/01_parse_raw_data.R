library(data.table)

setwd('../')

quizstarts <- fread('raw_data/SciBridge - quizstarts.csv')
questions <- fread('raw_data/SciBridge - questions.csv')
personal_infos <- fread('raw_data/SciBridge - personalinfos.csv')

