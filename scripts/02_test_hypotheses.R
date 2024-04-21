library(data.table)
library(ggplot2)

average_per_user_data <- readRDS("raw_data/average_per_user_data.rds")

light_cols <- c("#d0cfe7", "#bdbdbd")
dark_cols <- c("#756bb1", "#636363")

# plot quiz score per user per view

mean_scores <- average_per_user_data[number_of_answered_questions > 0, mean(percent_correct_answers), view]
setnames(mean_scores, "V1", "mean_scores")

t.test(average_per_user_data[number_of_answered_questions > 0 & view=="plain", percent_correct_answers],
       average_per_user_data[number_of_answered_questions > 0 & view=="feedback", percent_correct_answers])

ggplot(average_per_user_data[number_of_answered_questions > 0], 
       aes(x=view, y=percent_correct_answers)) + 
  geom_boxplot(width = 0.5,
               aes(colour=view)) +
  geom_dotplot(binaxis='y', dotsize=0.75, stackdir='center',
               aes(fill=view)) +
  geom_text(data = mean_scores, 
            aes(x = view, y = 105, label = sprintf("%.2f%%", mean_scores)),
            vjust = -0.5, color = "black") +
  #geom_jitter(shape=16, position=position_jitter(0.2))
  scale_y_continuous(breaks=seq(0,100,20), limits = c(0,110)) +
  labs(title="Distribution of Quiz Scores", 
       x="View", 
       y = "Correctly answered questions [%]") +
  scale_fill_manual(values = light_cols) +
  scale_colour_manual(values = dark_cols) +
  theme_linedraw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("plots/quiz_score_per_view.png", width = 3, height = 4)

# plot time spent answering questions

mean_scores <- average_per_user_data[number_of_answered_questions > 0, mean(average_time_spent_to_answer), view]
setnames(mean_scores, "V1", "mean_scores")

max_score <- average_per_user_data[number_of_answered_questions > 0, max(average_time_spent_to_answer)]

t.test(average_per_user_data[number_of_answered_questions > 0 & view=="plain", average_time_spent_to_answer],
       average_per_user_data[number_of_answered_questions > 0 & view=="feedback", average_time_spent_to_answer])

ggplot(average_per_user_data[number_of_answered_questions > 0], 
       aes(x=view, y=average_time_spent_to_answer)) + 
  geom_boxplot(width = 0.5,
               aes(colour=view)) +
  geom_dotplot(binaxis='y', dotsize=0.75, stackdir='center',
               aes(fill=view)) +
  geom_text(data = mean_scores, 
            aes(x = view, y = max_score * 1.1, label = sprintf("%.2fs", mean_scores)),
            vjust = -0.5, color = "black") +
  #geom_jitter(shape=16, position=position_jitter(0.2))
  scale_y_continuous(limits = c(0, max_score * 1.2)) +
  labs(title="Average answering time", 
       x="View", 
       y = "Time [seconds]") +
  scale_fill_manual(values = light_cols) +
  scale_colour_manual(values = dark_cols) +
  theme_linedraw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("plots/average_time_spent_answering_question.png", width = 3, height = 4)

# plot number of answered questions

mean_scores <- average_per_user_data[number_of_answered_questions > 0, mean(number_of_answered_questions), view]
setnames(mean_scores, "V1", "mean_scores")

t.test(average_per_user_data[number_of_answered_questions > 0 & view=="plain", number_of_answered_questions],
       average_per_user_data[number_of_answered_questions > 0 & view=="feedback", number_of_answered_questions])

ggplot(average_per_user_data[number_of_answered_questions > 0], 
       aes(x=view, y=number_of_answered_questions)) + 
  geom_boxplot(width = 0.5,
               aes(colour=view)) +
  geom_dotplot(binaxis='y', dotsize=0.75, stackdir='center',
               aes(fill=view)) +
  geom_text(data = mean_scores, 
            aes(x = view, y = 10.5, label = sprintf("%.2f", mean_scores)),
            vjust = -0.5, color = "black") +
  #geom_jitter(shape=16, position=position_jitter(0.2))
  scale_y_continuous(breaks=seq(0,10,2), limits = c(0,11)) +
  labs(title="Quiz Completion", 
       x="View", 
       y = "Number of answered questions") +
  scale_fill_manual(values = light_cols) +
  scale_colour_manual(values = dark_cols) +
  theme_linedraw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("plots/number_of_answered_questions.png", width = 3, height = 4)

# plot number of people finishing quiz

# plot number of people playing minigame

# plot number of people checking additional sources

# plot number of people using external sources

