library(data.table)
library(ggplot2)

# source("scripts/helper_functions.R")

average_per_user_data <- readRDS("raw_data/average_per_user_data.rds")

light_cols <- c("#d0cfe7", "#bdbdbd")
dark_cols <- c("#756bb1", "#636363")

# plot number of participants

tmp <- data.table(value=c(nrow(average_per_user_data),
                          average_per_user_data[number_of_answered_questions == 0, .N, ],
                          average_per_user_data[number_of_answered_questions > 0, .N, ],
                          average_per_user_data[number_of_answered_questions == 0, .N, ]),
                  xlab=c("Quiz starts",
                         "Aborting immediately",
                         "Participants",
                         "Completed"))
tmp[, xlab := factor(xlab, levels = xlab)]

ggplot(tmp,
       aes(x=xlab,
           y=value)) +
  geom_bar(stat="identity", width=0.75, fill="white", colour=dark_cols[2]) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, nrow(average_per_user_data) * 1.1)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title="Completion Statistics", 
       x="View", 
       y = "Count") +
  geom_text(data = tmp,
            y = 1.5, 
            aes(label=value),
            size=3.5,
            color = dark_cols[2]) +
  theme_linedraw() +
  theme(axis.title.x = element_blank())

ggsave("plots/demo_completion_stats.png", width = 3, height = 4)




