library(data.table)
library(ggplot2)

source("scripts/helper_functions.R")

average_per_user_data <- readRDS("raw_data/average_per_user_data.rds")

# for analysis filter for participants which have answered at least one question, otherwise exclude
participants <- average_per_user_data[number_of_answered_questions > 0]

light_cols <- c("#d0cfe7", "#bdbdbd")
dark_cols <- c("#756bb1", "#636363")

# plot quiz score per user per view

mean_scores <- participants[, mean(percent_correct_answers), view]
setnames(mean_scores, "V1", "mean_scores")

participants[, median(percent_correct_answers), view]
participants[, mean(percent_correct_answers), view]
t.test(percent_correct_answers ~ view, participants, alternative = "greater")

ggplot(participants, 
       aes(x=view, y=percent_correct_answers)) + 
  geom_boxplot(width = 0.5,
               aes(colour=view)) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.75, 
               aes(fill=view),
               position = position_jitter(width = 0.25, height = 0.25, seed = 1)) +
  geom_text(data = mean_scores, 
            aes(x = view, y = 105, label = sprintf("%.2f%%", mean_scores)),
            vjust = -0.5, color = "black") +
  #geom_jitter(shape=16, position=position_jitter(0.2))
  scale_y_continuous(breaks=seq(0,100,20), limits = c(-0.3,110)) +
  labs(x="View", 
       y = "Correctly answered questions [%]") +
  scale_fill_manual(values = light_cols) +
  scale_colour_manual(values = dark_cols) +
  theme_linedraw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("plots/hypo_quiz_score_per_view.png", width = 3, height = 4)

# plot quiz score per user per view only completed quiz participanst

mean_scores <- average_per_user_data[number_of_answered_questions == 10, mean(percent_correct_answers), view]
setnames(mean_scores, "V1", "mean_scores")

average_per_user_data[number_of_answered_questions == 10, median(percent_correct_answers), view]
average_per_user_data[number_of_answered_questions == 10, mean(percent_correct_answers), view]
t.test(percent_correct_answers ~ view, average_per_user_data[number_of_answered_questions == 10], alternative = "greater")

ggplot(average_per_user_data[number_of_answered_questions == 10], 
       aes(x=view, y=percent_correct_answers)) + 
  geom_boxplot(width = 0.5,
               aes(colour=view)) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.75, 
               aes(fill=view),
               position = position_jitter(width = 0.25, height = 0.25, seed = 1)) +
  geom_text(data = mean_scores, 
            aes(x = view, y = 105, label = sprintf("%.2f%%", mean_scores)),
            vjust = -0.5, color = "black") +
  #geom_jitter(shape=16, position=position_jitter(0.2))
  scale_y_continuous(breaks=seq(0,100,20), limits = c(-0.3,110)) +
  labs(x="View", 
       y = "Correctly answered questions [%]") +
  scale_fill_manual(values = light_cols) +
  scale_colour_manual(values = dark_cols) +
  theme_linedraw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("plots/hypo_quiz_score_per_view_only_completed.png", width = 3, height = 4)

# plot time spent answering questions

mean_scores <- participants[, mean(average_time_spent_to_answer), view]
setnames(mean_scores, "V1", "mean_scores")

max_score <- participants[, max(average_time_spent_to_answer)]

t.test(average_time_spent_to_answer ~ view, participants, alternative = "greater")
t.test(average_time_spent_to_answer ~ view, participants)
t.test(average_time_spent_to_answer ~ view, participants, alternative = "less")

ggplot(participants, 
       aes(x=view, y=average_time_spent_to_answer)) + 
  geom_boxplot(width = 0.5,
               aes(colour=view)) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.75,
               aes(fill=view),
               position = position_jitter(width = 0.25, height = 0.25, seed = 1)) +
  geom_text(data = mean_scores, 
            aes(x = view, y = max_score * 1.1, label = sprintf("%.2fs", mean_scores)),
            vjust = -0.5, color = "black") +
  #geom_jitter(shape=16, position=position_jitter(0.2))
  scale_y_continuous(limits = c(0, max_score * 1.2)) +
  labs(x="View", 
       y = "Time [seconds]") +
  scale_fill_manual(values = light_cols) +
  scale_colour_manual(values = dark_cols) +
  theme_linedraw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("plots/hypo_average_time_spent_answering_question.png", width = 3, height = 4)

# plot number of answered questions

mean_scores <- participants[, mean(number_of_answered_questions), view]
setnames(mean_scores, "V1", "mean_scores")

t.test(participants[view=="plain", number_of_answered_questions],
       participants[view=="feedback", number_of_answered_questions])

t.test(number_of_answered_questions ~ view, participants, alternative = "greater")

ggplot(participants, 
       aes(x=view, y=number_of_answered_questions)) + 
  geom_boxplot(width = 0.5,
               aes(colour=view)) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.75,
               aes(fill=view), 
               position = position_jitter(width = 0.25, height = 0.25, seed = 1)) +
  geom_text(data = mean_scores, 
            aes(x = view, y = 10.5, label = sprintf("%.2f", mean_scores)),
            vjust = -0.5, color = "black") +
  scale_y_continuous(breaks=seq(0,10,2), limits = c(0,11)) +
  labs(x="View", 
       y = "Number of answered questions") +
  scale_fill_manual(values = light_cols) +
  scale_colour_manual(values = dark_cols) +
  theme_linedraw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("plots/hypo_number_of_answered_questions.png", width = 3, height = 4)

# plot number of people finishing quiz

categories <- c("Number of participants", "Number of completed quizzes", "Number of aborted quizzes", "Completion Rate")

barplot_tmp <- get_barplot_df(participants, "has_finished_quiz", categories)
barplot_labels_tmp <- get_barplot_labels(barplot_tmp, categories)

barplot_labels_legend <- sort(apply(expand.grid(unique(barplot_tmp$view), categories[c(3, 2)]), 1, function(row) paste(row, collapse=" | ")))

ggplot() +
  geom_bar(data=barplot_tmp[category == categories[3] | category == categories[2]], 
           aes(x=view, 
               y=value, 
               fill=fill_colour_category, 
               colour=fill_colour_category),
           stat="identity", width = 0.75) +
  scale_fill_manual(breaks=barplot_labels_legend,
                    values = c("white", light_cols[1], "white", light_cols[2])) +
  scale_color_manual(breaks=barplot_labels_legend,
                     values = c(dark_cols[1], dark_cols[1], dark_cols[2], dark_cols[2])) +
  labs(x="View", 
       y = "Number of quiz participants") +
  geom_text(data = barplot_labels_tmp,
            aes(y=get(paste(strsplit(categories[2], split = " ")[[1]], collapse = "")) + 2, 
                x = view, 
                label=sprintf("%.2f%%", get(paste(strsplit(categories[4], split = " ")[[1]], collapse = "")))), 
            vjust=1, size=3.5, color=dark_cols) +
  geom_text(data = barplot_labels_tmp,
            aes(y=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = "")) + 2, 
                x = view, 
                label=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = ""))), 
            vjust=1, size=3.5, color=dark_cols) +
  theme_linedraw() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank())

ggsave("plots/hypo_number_of_completed_quizzes.png", width = 5, height = 4)

# plot times sources checked per person

mean_scores <- participants[, mean(number_has_used_sources), view]
setnames(mean_scores, "V1", "mean_scores")

t.test(participants[view=="plain", number_has_used_sources],
       participants[view=="feedback", number_has_used_sources])

t.test(number_has_used_sources ~ view, participants, alternative = "greater")

ggplot(participants, 
       aes(x=view, y=number_has_used_sources)) + 
  geom_boxplot(width = 0.5,
               aes(colour=view)) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.75,
               aes(fill=view), 
               position = position_jitter(width = 0.25, height = 0.25, seed = 1)) +
  geom_text(data = mean_scores, 
            aes(x = view, y = 10.5, label = sprintf("%.2f", mean_scores)),
            vjust = -0.5, color = "black") +
  scale_y_continuous(breaks=seq(0,10,2), limits = c(-0.3,11)) +
  labs(x="View", 
       y = "Frequency of additional sources beingchecked") +
  scale_fill_manual(values = light_cols) +
  scale_colour_manual(values = dark_cols) +
  theme_linedraw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("plots/hypo_number_of_times_sources_were_checked.png", width = 3, height = 4)

# plot number of people checking additional sources

categories <- c("Number of participants", "Participants using additional sources", "Participants not using additional sources", "Sources usage rate")

barplot_tmp <- get_barplot_df(participants, "has_used_sources", categories)
barplot_labels_tmp <- get_barplot_labels(barplot_tmp, categories)

barplot_labels_legend <- sort(apply(expand.grid(unique(barplot_tmp$view), categories[c(3, 2)]), 1, function(row) paste(row, collapse=" | ")))

ggplot() +
  geom_bar(data=barplot_tmp[category == categories[3] | category == categories[2]], 
           aes(x=view, 
               y=value, 
               fill=fill_colour_category, 
               colour=fill_colour_category),
           stat="identity", width = 0.75) +
  scale_fill_manual(breaks=barplot_labels_legend,
                    values = c("white", light_cols[1], "white", light_cols[2])) +
  scale_color_manual(breaks=barplot_labels_legend,
                     values = c(dark_cols[1], dark_cols[1], dark_cols[2], dark_cols[2])) +
  labs(x="View", 
       y = "Number of quiz participants") +
  geom_text(data = barplot_labels_tmp,
            aes(y=get(paste(strsplit(categories[2], split = " ")[[1]], collapse = "")) + 2, 
                x = view, 
                label=sprintf("%.2f%%", get(paste(strsplit(categories[4], split = " ")[[1]], collapse = "")))), 
            vjust=1, size=3.5, color=dark_cols) +
  geom_text(data = barplot_labels_tmp,
            aes(y=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = "")) + 2, 
                x = view, 
                label=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = ""))), 
            vjust=1, size=3.5, color=dark_cols) +
  theme_linedraw() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank())

ggsave("plots/hypo_number_of_people_checking_sources.png", width = 5.5, height = 4.5)

# plot number of people playing minigame

categories <- c("Number of participants", "Participants playing minigame", "Participants not playing minigame", "Rate of people playing minigame")

barplot_tmp <- get_barplot_df(participants, "has_viewed_game", categories)
barplot_labels_tmp <- get_barplot_labels(barplot_tmp, categories)

barplot_labels_legend <- sort(apply(expand.grid(unique(barplot_tmp$view), categories[c(3, 2)]), 1, function(row) paste(row, collapse=" | ")))

ggplot() +
  geom_bar(data=barplot_tmp[category == categories[3] | category == categories[2]], 
           aes(x=view, 
               y=value, 
               fill=fill_colour_category, 
               colour=fill_colour_category),
           stat="identity", width = 0.75) +
  scale_fill_manual(breaks=barplot_labels_legend,
                    values = c("white", light_cols[1], "white", light_cols[2])) +
  scale_color_manual(breaks=barplot_labels_legend,
                     values = c(dark_cols[1], dark_cols[1], dark_cols[2], dark_cols[2])) +
  labs(x="View", 
       y = "Number of quiz participants") +
  geom_text(data = barplot_labels_tmp,
            aes(y=get(paste(strsplit(categories[2], split = " ")[[1]], collapse = "")) + 2, 
                x = view, 
                label=sprintf("%.2f%%", get(paste(strsplit(categories[4], split = " ")[[1]], collapse = "")))), 
            vjust=1, size=3.5, color=dark_cols) +
  geom_text(data = barplot_labels_tmp,
            aes(y=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = "")) + 2, 
                x = view, 
                label=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = ""))), 
            vjust=1, size=3.5, color=dark_cols) +
  theme_linedraw() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank())

ggsave("plots/hypo_number_of_people_playing_minigame.png", width = 5.5, height = 4.5)

# plot number of people using external sources

categories <- c("Number of participants", "Participants using external sources", "Participants not using external sources", "Sources usage rate")

barplot_tmp <- get_barplot_df(participants, "has_used_google", categories)
barplot_labels_tmp <- get_barplot_labels(barplot_tmp, categories)

barplot_labels_legend <- sort(apply(expand.grid(unique(barplot_tmp$view), categories[c(3, 2)]), 1, function(row) paste(row, collapse=" | ")))

ggplot() +
  geom_bar(data=barplot_tmp[category == categories[3] | category == categories[2]], 
           aes(x=view, 
               y=value, 
               fill=fill_colour_category, 
               colour=fill_colour_category),
           stat="identity", width = 0.75) +
  scale_fill_manual(breaks=barplot_labels_legend,
                    values = c("white", light_cols[1], "white", light_cols[2])) +
  scale_color_manual(breaks=barplot_labels_legend,
                     values = c(dark_cols[1], dark_cols[1], dark_cols[2], dark_cols[2])) +
  labs(x="View", 
       y = "Number of quiz participants") +
  geom_text(data = barplot_labels_tmp,
            aes(y=get(paste(strsplit(categories[2], split = " ")[[1]], collapse = "")) + 2, 
                x = view, 
                label=sprintf("%.2f%%", get(paste(strsplit(categories[4], split = " ")[[1]], collapse = "")))), 
            vjust=1, size=3.5, color=dark_cols) +
  geom_text(data = barplot_labels_tmp,
            aes(y=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = "")) + 2, 
                x = view, 
                label=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = ""))), 
            vjust=1, size=3.5, color=dark_cols) +
  theme_linedraw() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank())

ggsave("plots/hypo_number_of_people_checking_external_sources.png", width = 5.5, height = 4.5)

