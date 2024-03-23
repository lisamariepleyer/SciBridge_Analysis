library(data.table)

setwd('../')

quizstarts <- fread('raw_data/SciBridge - quizstarts.csv')
questions <- fread('raw_data/SciBridge - questions.csv')
personal_infos <- fread('raw_data/SciBridge - personalinfos.csv')

# parse Ja, Nein, keine Angabe and NA values to something programmatically meaningful

for (col in c("isCorrect", "hasViewedSource", "hasViewedGame")) {
  questions[is.na(get(col)), eval(col) := FALSE]
}

for (col in c("hasUsedSources", "hasUsedGoogle")) {
  personal_infos[get(col) == "Ja", eval(col) := TRUE]
  personal_infos[get(col) == "Nein", eval(col) := FALSE]
}

for (col in c("age", "gender")) {
  personal_infos[get(col) == "keine Angabe", eval(col) := NA]
}
