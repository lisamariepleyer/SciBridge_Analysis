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

# gather average per user data

average_per_user_data <- data.table(uid = quizstarts$uid,
                                    view = quizstarts$view)

setkey(average_per_user_data, uid)

# track quiz score

average_per_user_data <- questions[,.N, uid][average_per_user_data][is.na(N), N := 0]
setnames(average_per_user_data, "N", "number_of_answered_questions")

average_per_user_data <- questions[isCorrect == TRUE, .N, uid][average_per_user_data][is.na(N), N := 0]
setnames(average_per_user_data, "N", "number_of_correct_answers")

average_per_user_data[, percent_correct_answers := (number_of_correct_answers / number_of_answered_questions) * 100]
average_per_user_data[is.na(percent_correct_answers), percent_correct_answers := 0]

# track whether quiz was finished

average_per_user_data[, has_finished_quiz := ifelse(number_of_answered_questions == 10, TRUE, FALSE)]

# track average time spent on each question

average_per_user_data <- questions[, mean(answer_duration), uid][average_per_user_data]
setnames(average_per_user_data, "V1", "average_time_spent_on_answer")

# track usage of sources

average_per_user_data <- questions[, sum(hasViewedSource), uid][average_per_user_data]
setnames(average_per_user_data, "V1", "number_has_used_sources")

average_per_user_data[, has_used_sources := ifelse(number_has_used_sources > 0, TRUE, FALSE)]

average_per_user_data <- questions[, sum(hasViewedGame), uid][average_per_user_data]
setnames(average_per_user_data, "V1", "number_has_viewed_game")

average_per_user_data[view == "plain", number_has_viewed_game := NA]
average_per_user_data[, has_viewed_game := ifelse(number_has_viewed_game > 0, TRUE, FALSE)]

average_per_user_data <- personal_infos[, .(uid, hasUsedGoogle)][average_per_user_data]
setnames(average_per_user_data, "hasUsedGoogle", "has_used_google")

# track personal infos

average_per_user_data <- personal_infos[, .(uid, age, gender, level_it, level_physics_chemistry, level_medicine, level_climate_change)][average_per_user_data]

# save data

fwrite(average_per_user_data, "raw_data/average_per_user_data.csv")
saveRDS(average_per_user_data, "raw_data/average_per_user_data.rds")
