library(data.table)

setwd('../')

quizstarts <- fread('raw_data/SciBridge - quizstarts.csv')
questions <- fread('raw_data/SciBridge - questions.csv')
personal_infos <- fread('raw_data/SciBridge - personalinfos.csv')

# parse Ja, Nein, keine Angabe and NA values to something programmatically meaningful

if (length(unique(quizstarts$uid)) < length(quizstarts$uid)) {
  warning("User ID given at quizstart is not unique!")
}

for (col in c("isCorrect", "hasViewedSource", "hasViewedGame")) {
  questions[is.na(get(col)), eval(col) := FALSE]
}
#questions[, answer_duration := as.numeric(NA)]

for (col in c("hasUsedSources", "hasUsedGoogle")) {
  personal_infos[get(col) == "", eval(col) := NA]
  personal_infos[get(col) == "Ja", eval(col) := TRUE]
  personal_infos[get(col) == "Nein", eval(col) := FALSE]
  personal_infos[, eval(col) := as.logical(get(col))]
}

for (col in c("age", "gender")) {
  personal_infos[get(col) == "keine Angabe", eval(col) := NA]
  personal_infos[get(col) == "", eval(col) := NA]
}

setkey(questions, uid)
setkey(personal_infos, uid)

# calculate time between question per user

for (i in 1:nrow(quizstarts)) {
  times <- sort(c(quizstarts[i, timestamp], 
                  questions[quizstarts[i, uid], timestamp]))
  durations <- as.numeric(diff(times))
  
  # add delay when question feedback is shown
  if (quizstarts[i, view] == "feedback") {
    durations <- durations - 2
  }

  questions[quizstarts[i, uid], 
            answer_duration := durations[order(question)], 
            by = .EACHI]
}
