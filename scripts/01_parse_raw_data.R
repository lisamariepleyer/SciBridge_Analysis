library(data.table)

quizstarts <- fread('raw_data/SciBridge - quizstarts.csv')
questions <- fread('raw_data/SciBridge - questions.csv')
personal_infos <- fread('raw_data/SciBridge - personalinfos.csv')

hard_questions <- c(2, 5, 6, 7, 8, 10)
easy_questions <- c(1, 3, 4, 9)

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

## gather average per question data

average_per_question_data <- questions[,
                                       .(.N,
                                         sum(isCorrect),
                                         sum(isCorrect)/.N,
                                         mean(answer_duration),
                                         sd(answer_duration),
                                         sum(hasViewedSource),
                                         sum(hasViewedSource)/.N,
                                         sum(hasViewedGame),
                                         sum(hasViewedGame)/.N), 
                                       question]

names(average_per_question_data) <- c("question",
                                      "total_submissions", 
                                      "no_answered_correctly",
                                      "perc_answered_correctly",
                                      "mean_time_to_answer",
                                      "std_time_to_answer",
                                      "no_has_viewed_source",
                                      "perc_has_viewed_source",
                                      "no_has_viewed_game",
                                      "perc_has_viewed_game")

average_per_question_data [, "area" := c("it", "physics_chemistry", "medicine", "physics_chemistry", "it", "climate_change", "medicine", "physics_chemistry", "it", "climate_change")]

## gather average per user data

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
setnames(average_per_user_data, "V1", "average_time_spent_to_answer")

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

## gather average per question difficulty data

answered_all_questions <- questions[,.N, uid][N == 10, uid]
setkey(quizstarts, uid)

average_per_qd_data <- data.table(uid = rep(answered_all_questions, 2),
                                  view = rep(quizstarts[answered_all_questions, view], 2),
                                  difficulty_level = rep(c("difficult", "easy"), each = length(answered_all_questions)))
setkey(average_per_qd_data, uid, difficulty_level)

questions[, difficulty_level := ""]
questions[question %in% hard_questions, difficulty_level := "difficult"]
questions[question %in% easy_questions, difficulty_level := "easy"]

# track quiz score

tmp <- questions[isCorrect == TRUE, .N, .(uid, difficulty_level)]
setnames(tmp, "N", "number_of_correct_answers"); setkey(tmp, uid, difficulty_level)
average_per_qd_data <- tmp[average_per_qd_data]

average_per_qd_data[difficulty_level == "easy", percent_correct_answers := (number_of_correct_answers / length(easy_questions)) * 100]
average_per_qd_data[difficulty_level == "difficult", percent_correct_answers := (number_of_correct_answers / length(hard_questions)) * 100]
average_per_qd_data[is.na(percent_correct_answers), percent_correct_answers := 0]

# track average time spent on each question

tmp <- questions[, mean(answer_duration), .(uid, difficulty_level)]
setnames(tmp, "V1", "average_time_spent_to_answer"); setkey(tmp, uid, difficulty_level)
average_per_qd_data <- tmp[average_per_qd_data]

# track usage of sources

tmp <- questions[, sum(hasViewedSource), .(uid, difficulty_level)]
setnames(tmp, "V1", "number_has_used_sources"); setkey(tmp, uid, difficulty_level)
average_per_qd_data <- tmp[average_per_qd_data]

average_per_qd_data[, has_used_sources := ifelse(number_has_used_sources > 0, TRUE, FALSE)]

tmp <- questions[, sum(hasViewedGame), .(uid, difficulty_level)]
setnames(tmp, "V1", "number_has_viewed_game"); setkey(tmp, uid, difficulty_level)
average_per_qd_data <- tmp[average_per_qd_data]

average_per_qd_data[view == "plain", number_has_viewed_game := NA]
average_per_qd_data[, has_viewed_game := ifelse(number_has_viewed_game > 0, TRUE, FALSE)]

average_per_qd_data <- personal_infos[, .(uid, age, gender, level_it, level_physics_chemistry, level_medicine, level_climate_change)][average_per_qd_data]

# save data

fwrite(average_per_question_data, "raw_data/average_per_question_data.csv")
saveRDS(average_per_question_data, "raw_data/average_per_question_data.rds")

fwrite(questions, "raw_data/parsed_questions_data.csv")
saveRDS(questions, "raw_data/parsed_questions_data.rds")

fwrite(average_per_user_data, "raw_data/average_per_user_data.csv")
saveRDS(average_per_user_data, "raw_data/average_per_user_data.rds")

fwrite(average_per_qd_data, "raw_data/average_per_question_difficulty_data.csv")
saveRDS(average_per_qd_data, "raw_data/average_per_question_difficulty_data.rds")
