library(data.table)
library(ggplot2)

source("scripts/helper_functions.R")

participants <- readRDS("raw_data/average_per_question_difficulty_data.rds")

light_cols <- c("#d0cfe7", "#bdbdbd")
dark_cols <- c("#756bb1", "#636363")

difficulty_levels <- c("easy", "difficult")

participants[, difficulty_level := factor(difficulty_level, levels = difficulty_levels)]

# plot quiz score per user per view

for (g in difficulty_levels) {
  print(g)
  print(t.test(percent_correct_answers ~ view, participants[difficulty_level == g], alternative = "greater"))
}

mean_scores <- participants[, mean(percent_correct_answers), .(view, difficulty_level)]
setnames(mean_scores, "V1", "mean_scores")

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
  facet_grid(~difficulty_level) +
  scale_y_continuous(breaks=seq(0,100,20), limits = c(-0.3,110)) +
  labs(x="View", 
       y = "Correctly answered questions [%]") +
  scale_fill_manual(values = light_cols) +
  scale_colour_manual(values = dark_cols) +
  theme_linedraw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        strip.background = element_rect(fill="#f2f2f2"),
        strip.text = element_text(color="black"))

ggsave("plots/qdif_quiz_score_per_view.png", width = 6, height = 4)

# plot time spent answering questions

mean_scores <- participants[, mean(average_time_spent_to_answer), .(view, difficulty_level)]
setnames(mean_scores, "V1", "mean_scores")

max_score <- participants[, max(average_time_spent_to_answer)]

for (g in difficulty_levels) {
  print(g)
  print(t.test(average_time_spent_to_answer ~ view, participants[difficulty_level == g], alternative = "less"))
}

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
  facet_grid(~difficulty_level) +
  #geom_jitter(shape=16, position=position_jitter(0.2))
  scale_y_continuous(limits = c(0, max_score * 1.2)) +
  labs(x="View", 
       y = "Time [seconds]") +
  scale_fill_manual(values = light_cols) +
  scale_colour_manual(values = dark_cols) +
  theme_linedraw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        strip.background = element_rect(fill="#f2f2f2"),
        strip.text = element_text(color="black"))

ggsave("plots/qdif_average_time_spent_answering_question.png", width = 6, height = 4)

# plot number of answered questions

# skipped

# plot number of people finishing quiz

# skipped

# plot number of people checking additional sources

# categories <- c("Number of participants", "Participants using additional sources", "Participants not using additional sources", "Sources usage rate")
# 
# barplot_tmp <- do.call(rbind, lapply(goi, function(g) {
#   tmp <- get_barplot_df(participants[gender == g], "has_used_sources", categories)
#   tmp[, gender := g]
#   return(tmp)
# }))
# barplot_tmp[, gender := factor(gender, levels = goi)]
# 
# barplot_labels_tmp <- do.call(rbind, lapply(goi, function(g) {
#   tmp <- get_barplot_labels(barplot_tmp[gender == g], categories)
#   tmp[, gender := g]
#   return(tmp)
# }))
# barplot_labels_tmp[, gender := factor(gender, levels = goi)]
# 
# barplot_labels_legend <- sort(apply(expand.grid(unique(barplot_tmp$view), categories[c(3, 2)]), 1, function(row) paste(row, collapse=" | ")))
# 
# ggplot() +
#   geom_bar(data=barplot_tmp[category == categories[3] | category == categories[2]], 
#            aes(x=view, 
#                y=value, 
#                fill=fill_colour_category, 
#                colour=fill_colour_category),
#            stat="identity", width = 0.75) +
#   facet_grid(~gender) +
#   scale_fill_manual(breaks=barplot_labels_legend,
#                     values = c("white", light_cols[1], "white", light_cols[2])) +
#   scale_color_manual(breaks=barplot_labels_legend,
#                      values = c(dark_cols[1], dark_cols[1], dark_cols[2], dark_cols[2])) +
#   labs(x="View", 
#        y = "Number of quiz participants") +
#   geom_text(data = barplot_labels_tmp,
#             aes(y=get(paste(strsplit(categories[2], split = " ")[[1]], collapse = "")) + 1, 
#                 x = view,
#                 label=sprintf("%.2f%%", get(paste(strsplit(categories[4], split = " ")[[1]], collapse = "")))), 
#             vjust=1, size=3.5, color = c(dark_cols[1], dark_cols[2], dark_cols[2], dark_cols[1])) +
#   geom_text(data = barplot_labels_tmp,
#             aes(y=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = "")) + 1, 
#                 x = view, 
#                 label=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = ""))), 
#             vjust=1, size=3.5, color = c(dark_cols[1], dark_cols[2], dark_cols[2], dark_cols[1])) +
#   theme_linedraw() +
#   theme(legend.title = element_blank(),
#         axis.title.x = element_blank())
# 
# ggsave("plots/qdif_number_of_people_checking_sources.png", width = 8, height = 4.5)

# plot times source per person & difficulty level

mean_scores <- participants[, mean(number_has_used_sources), .(view, difficulty_level)]
setnames(mean_scores, "V1", "mean_scores")

for (g in difficulty_levels) {
  print(g)
  print(t.test(number_has_used_sources ~ view, participants[difficulty_level == g], alternative = "greater"))
}

for (v in c("plain", "feedback")) {
  print(v)
  print(t.test(number_has_used_sources ~ difficulty_level, participants[view == v]))
}
# though not fair cause there were more hard than easy questions!

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
  facet_grid(~difficulty_level) +
  scale_y_continuous(breaks=seq(0,10,2), limits = c(-0.3,11)) +
  labs(x="View", 
       y = "Frequency of sources being checked") +
  scale_fill_manual(values = light_cols) +
  scale_colour_manual(values = dark_cols) +
  theme_linedraw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        strip.background = element_rect(fill="#f2f2f2"),
        strip.text = element_text(color="black"))

ggsave("plots/qdif_number_of_times_sources_were_checked_difficulty_level.png", width = 6, height = 4)

# plot number of people playing minigame

# categories <- c("Number of participants", "Participants playing minigame", "Participants not playing minigame", "Rate of people playing minigame")
# 
# barplot_tmp <- do.call(rbind, lapply(goi, function(g) {
#   tmp <- get_barplot_df(participants[gender == g], "has_viewed_game", categories)
#   tmp[, gender := g]
#   return(tmp)
# }))
# barplot_tmp[, gender := factor(gender, levels = goi)]
# 
# barplot_labels_tmp <- do.call(rbind, lapply(goi, function(g) {
#   tmp <- get_barplot_labels(barplot_tmp[gender == g], categories)
#   tmp[, gender := g]
#   return(tmp)
# }))
# barplot_labels_tmp[, gender := factor(gender, levels = goi)]
# 
# barplot_labels_legend <- sort(apply(expand.grid(unique(barplot_tmp$view), categories[c(3, 2)]), 1, function(row) paste(row, collapse=" | ")))
# 
# ggplot() +
#   geom_bar(data=barplot_tmp[category == categories[3] | category == categories[2]], 
#            aes(x=view, 
#                y=value, 
#                fill=fill_colour_category, 
#                colour=fill_colour_category),
#            stat="identity", width = 0.75) +
#   facet_grid(~gender) +
#   scale_fill_manual(breaks=barplot_labels_legend,
#                     values = c("white", light_cols[1], "white", light_cols[2])) +
#   scale_color_manual(breaks=barplot_labels_legend,
#                      values = c(dark_cols[1], dark_cols[1], dark_cols[2], dark_cols[2])) +
#   labs(x="View", 
#        y = "Number of quiz participants") +
#   geom_text(data = barplot_labels_tmp,
#             aes(y=get(paste(strsplit(categories[2], split = " ")[[1]], collapse = "")) + 1, 
#                 x = view,
#                 label=sprintf("%.2f%%", get(paste(strsplit(categories[4], split = " ")[[1]], collapse = "")))), 
#             vjust=1, size=3.5, color = c(dark_cols[1], dark_cols[2], dark_cols[2], dark_cols[1])) +
#   geom_text(data = barplot_labels_tmp,
#             aes(y=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = "")) + 1, 
#                 x = view, 
#                 label=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = ""))), 
#             vjust=1, size=3.5, color = c(dark_cols[1], dark_cols[2], dark_cols[2], dark_cols[1])) +
#   theme_linedraw() +
#   theme(legend.title = element_blank(),
#         axis.title.x = element_blank())
# 
# ggsave("plots/gend_number_of_people_playing_minigame.png", width = 8, height = 4.5)

# plot times mingame played per person & difficulty_level

mean_scores <- participants[, mean(number_has_viewed_game), .(view, difficulty_level)]
mean_scores <- mean_scores[view == "feedback"]
setnames(mean_scores, "V1", "mean_scores")

t.test(number_has_viewed_game ~ difficulty_level, participants[view == "feedback"])

ggplot(participants[view == "feedback"], 
       aes(x=difficulty_level, y=number_has_viewed_game)) + 
  geom_boxplot(width = 0.5,
               aes(colour=difficulty_level)) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.75,
               aes(fill=difficulty_level), 
               position = position_jitter(width = 0.25, height = 0.25, seed = 1)) +
  geom_text(data = mean_scores, 
            aes(x = difficulty_level, y = 10.5, label = sprintf("%.2f", mean_scores)),
            vjust = -0.5, color = "black") +
  scale_y_continuous(breaks=seq(0,10,2), limits = c(-0.3,11)) +
  labs(x="View", 
       y = "Frequency of minigame being played") +
  scale_fill_manual(values = c("#f6e8c3", "#c7eae5")) +
  scale_colour_manual(values = c("#d8b365", "#5ab4ac")) +
  theme_linedraw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("plots/qdif_number_of_times_minigame_was_played_difficulty_level.png", width = 3, height = 4)

# plot number of people using external sources

# categories <- c("Number of participants", "Participants using external sources", "Participants not using external sources", "Sources usage rate")
# 
# barplot_tmp <- do.call(rbind, lapply(goi, function(g) {
#   tmp <- get_barplot_df(participants[gender == g], "has_used_google", categories)
#   tmp[, gender := g]
#   return(tmp)
# }))
# barplot_tmp[, gender := factor(gender, levels = goi)]
# 
# barplot_labels_tmp <- do.call(rbind, lapply(goi, function(g) {
#   tmp <- get_barplot_labels(barplot_tmp[gender == g], categories)
#   tmp[, gender := g]
#   return(tmp)
# }))
# barplot_labels_tmp[, gender := factor(gender, levels = goi)]
# 
# barplot_labels_legend <- sort(apply(expand.grid(unique(barplot_tmp$view), categories[c(3, 2)]), 1, function(row) paste(row, collapse=" | ")))
# 
# ggplot() +
#   geom_bar(data=barplot_tmp[category == categories[3] | category == categories[2]], 
#            aes(x=view, 
#                y=value, 
#                fill=fill_colour_category, 
#                colour=fill_colour_category),
#            stat="identity", width = 0.75) +
#   facet_grid(~gender) +
#   scale_fill_manual(breaks=barplot_labels_legend,
#                     values = c("white", light_cols[1], "white", light_cols[2])) +
#   scale_color_manual(breaks=barplot_labels_legend,
#                      values = c(dark_cols[1], dark_cols[1], dark_cols[2], dark_cols[2])) +
#   labs(x="View", 
#        y = "Number of quiz participants") +
#   geom_text(data = barplot_labels_tmp,
#             aes(y=get(paste(strsplit(categories[2], split = " ")[[1]], collapse = "")) + 1, 
#                 x = view,
#                 label=sprintf("%.2f%%", get(paste(strsplit(categories[4], split = " ")[[1]], collapse = "")))), 
#             vjust=1, size=3.5, color = c(dark_cols[1], dark_cols[2], dark_cols[2], dark_cols[1])) +
#   geom_text(data = barplot_labels_tmp,
#             aes(y=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = "")) + 1, 
#                 x = view, 
#                 label=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = ""))), 
#             vjust=1, size=3.5, color = c(dark_cols[1], dark_cols[2], dark_cols[2], dark_cols[1])) +
#   theme_linedraw() +
#   theme(legend.title = element_blank(),
#         axis.title.x = element_blank())
# 
# ggsave("plots/gend_number_of_people_checking_external_sources.png", width = 8, height = 4.5)

questions <- readRDS("raw_data/parsed_questions_data.rds")
answered_all_questions <- questions[,.N, uid][N == 10, uid]

t.test(answer_duration ~ difficulty_level, questions[answered_all_questions], alternative="greater")

