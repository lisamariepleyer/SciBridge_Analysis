library(data.table)
library(ggplot2)

average_per_user_data <- readRDS("raw_data/average_per_user_data.rds")

cols <- c("#d0cfe7", "#756bb1", "#bdbdbd", "#636363")
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
       y = "% of correctly answered questions") +
  scale_fill_manual(values = light_cols) +
  scale_colour_manual(values = dark_cols) +
  theme_linedraw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("plots/quiz_score_per_view.png", width = 3, height = 4)
