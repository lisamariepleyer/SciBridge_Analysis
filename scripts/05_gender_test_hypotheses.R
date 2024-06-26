library(data.table)
library(ggplot2)

source("scripts/helper_functions.R")

average_per_user_data <- readRDS("raw_data/average_per_user_data.rds")

# for analysis filter for participants which have answered at least one question, otherwise exclude
participants <- average_per_user_data[number_of_answered_questions > 0]

light_cols <- c("#d0cfe7", "#bdbdbd")
dark_cols <- c("#756bb1", "#636363")

genders <- c("männlich", "weiblich", "nicht-binär", "andere", "ns")
genders_thesis <- c("male", "female", "non-binary", "other", "ns")
goi <- c("male", "female")
ngoi <- genders_thesis[!genders_thesis %in% goi]

participants[is.na(gender), gender := "ns"]
for (i in 1:length(genders)) {
  participants[gender == genders[i], gender := genders_thesis[i]] 
}
participants[, gender := factor(gender, levels = genders_thesis)]

# plot quiz score per user per view

for (g in goi) {
  print(g)
  print(t.test(percent_correct_answers ~ view, participants[gender == g], alternative = "greater"))
}

mean_scores <- participants[, mean(percent_correct_answers), .(view, gender)]
mean_scores <- mean_scores[gender %in% goi]
setnames(mean_scores, "V1", "mean_scores")

ggplot(participants[gender %in% goi], 
       aes(x=view, y=percent_correct_answers)) + 
  geom_boxplot(width = 0.5,
               aes(colour=view)) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.75, 
               aes(fill=view),
               position = position_jitter(width = 0.25, height = 0.25, seed = 1)) +
  geom_text(data = mean_scores, 
            aes(x = view, y = 105, label = sprintf("%.2f%%", mean_scores)),
            vjust = -0.5, color = "black") +
  facet_grid(~gender) +
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

ggsave("plots/gend_quiz_score_per_view.png", width = 6, height = 4)

# plot time spent answering questions

mean_scores <- participants[, mean(average_time_spent_to_answer), .(view, gender)]
mean_scores <- mean_scores[gender %in% goi]
setnames(mean_scores, "V1", "mean_scores")

max_score <- participants[gender %in% goi, max(average_time_spent_to_answer)]

for (g in goi) {
  print(g)
  print(t.test(average_time_spent_to_answer ~ view, participants[gender == g], alternative = "less"))
}

ggplot(participants[gender %in% goi], 
       aes(x=view, y=average_time_spent_to_answer)) + 
  geom_boxplot(width = 0.5,
               aes(colour=view)) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.75,
               aes(fill=view),
               position = position_jitter(width = 0.25, height = 0.25, seed = 1)) +
  geom_text(data = mean_scores, 
            aes(x = view, y = max_score * 1.1, label = sprintf("%.2fs", mean_scores)),
            vjust = -0.5, color = "black") +
  facet_grid(~gender) +
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

ggsave("plots/gend_average_time_spent_answering_question.png", width = 6, height = 4)

# plot number of answered questions

# skipped, cause always 10 if gender info was provided

# plot number of people finishing quiz

# skipped, cause has to have finished if gender info was provided

# plot times sources checked per person

mean_scores <- participants[, mean(number_has_used_sources), .(view, gender)]
mean_scores <- mean_scores[gender %in% goi]
setnames(mean_scores, "V1", "mean_scores")

max_score <- participants[gender %in% goi, max(number_has_used_sources)]

for (g in goi) {
  print(g)
  print(t.test(number_has_used_sources ~ view, participants[gender == g], alternative = "greater"))
}

ggplot(participants[gender %in% goi], 
       aes(x=view, y=number_has_used_sources)) + 
  geom_boxplot(width = 0.5,
               aes(colour=view)) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.75,
               aes(fill=view), 
               position = position_jitter(width = 0.25, height = 0.25, seed = 1)) +
  geom_text(data = mean_scores, 
            aes(x = view, y = 10.5, label = sprintf("%.2f", mean_scores)),
            vjust = -0.5, color = "black") +
  facet_grid(~gender) +
  scale_y_continuous(breaks=seq(0,10,2), limits = c(-0.3,11)) +
  labs(x="View", 
       y = "Frequency of additional sources beingchecked") +
  scale_fill_manual(values = light_cols) +
  scale_colour_manual(values = dark_cols) +
  theme_linedraw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        strip.background = element_rect(fill="#f2f2f2"),
        strip.text = element_text(color="black"))

ggsave("plots/gend_number_of_times_sources_were_checked.png", width = 6, height = 4)

# plot number of people checking additional sources

categories <- c("Number of participants", "Participants using additional sources", "Participants not using additional sources", "Sources usage rate")

barplot_tmp <- do.call(rbind, lapply(goi, function(g) {
  tmp <- get_barplot_df(participants[gender == g], "has_used_sources", categories)
  tmp[, gender := g]
  return(tmp)
}))
barplot_tmp[, gender := factor(gender, levels = goi)]

barplot_labels_tmp <- do.call(rbind, lapply(goi, function(g) {
  tmp <- get_barplot_labels(barplot_tmp[gender == g], categories)
  tmp[, gender := g]
  return(tmp)
}))
barplot_labels_tmp[, gender := factor(gender, levels = goi)]

barplot_labels_legend <- sort(apply(expand.grid(unique(barplot_tmp$view), categories[c(3, 2)]), 1, function(row) paste(row, collapse=" | ")))

ggplot() +
  geom_bar(data=barplot_tmp[category == categories[3] | category == categories[2]], 
           aes(x=view, 
               y=value, 
               fill=fill_colour_category, 
               colour=fill_colour_category),
           stat="identity", width = 0.75) +
  facet_grid(~gender) +
  scale_fill_manual(breaks=barplot_labels_legend,
                    values = c("white", light_cols[1], "white", light_cols[2])) +
  scale_color_manual(breaks=barplot_labels_legend,
                     values = c(dark_cols[1], dark_cols[1], dark_cols[2], dark_cols[2])) +
  labs(x="View", 
       y = "Number of quiz participants") +
  geom_text(data = barplot_labels_tmp,
            aes(y=get(paste(strsplit(categories[2], split = " ")[[1]], collapse = "")) + 1, 
                x = view,
                label=sprintf("%.2f%%", get(paste(strsplit(categories[4], split = " ")[[1]], collapse = "")))), 
            vjust=1, size=3.5, color = c(dark_cols[1], dark_cols[2], dark_cols[2], dark_cols[1])) +
  geom_text(data = barplot_labels_tmp,
            aes(y=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = "")) + 1, 
                x = view, 
                label=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = ""))), 
            vjust=1, size=3.5, color = c(dark_cols[1], dark_cols[2], dark_cols[2], dark_cols[1])) +
  theme_linedraw() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_rect(fill="#f2f2f2"),
        strip.text = element_text(color="black"))

ggsave("plots/gend_number_of_people_checking_sources.png", width = 8, height = 4.5)

# plot times source per person & gender

mean_scores <- participants[, mean(number_has_used_sources), .(view, gender)]
mean_scores <- mean_scores[gender %in% goi]
setnames(mean_scores, "V1", "mean_scores")

for (g in goi) {
  print(g)
  print(t.test(number_has_used_sources ~ view, participants[gender == g], alternative = "greater"))
}

for (v in c("plain", "feedback")) {
  print(v)
  print(t.test(number_has_used_sources ~ gender, participants[gender != "ns" & view == v]))
}

ggplot(participants[gender %in% goi], 
       aes(x=view, y=number_has_used_sources)) + 
  geom_boxplot(width = 0.5,
               aes(colour=view)) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.75,
               aes(fill=view), 
               position = position_jitter(width = 0.25, height = 0.25, seed = 1)) +
  geom_text(data = mean_scores, 
            aes(x = view, y = 10.5, label = sprintf("%.2f", mean_scores)),
            vjust = -0.5, color = "black") +
  facet_grid(~gender) +
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

ggsave("plots/gend_number_of_times_sources_were_checked_gender.png", width = 6, height = 4)

# plot number of people playing minigame

categories <- c("Number of participants", "Participants playing minigame", "Participants not playing minigame", "Rate of people playing minigame")

barplot_tmp <- do.call(rbind, lapply(goi, function(g) {
  tmp <- get_barplot_df(participants[gender == g], "has_viewed_game", categories)
  tmp[, gender := g]
  return(tmp)
}))
barplot_tmp[, gender := factor(gender, levels = goi)]

barplot_labels_tmp <- do.call(rbind, lapply(goi, function(g) {
  tmp <- get_barplot_labels(barplot_tmp[gender == g], categories)
  tmp[, gender := g]
  return(tmp)
}))
barplot_labels_tmp[, gender := factor(gender, levels = goi)]

barplot_labels_legend <- sort(apply(expand.grid(unique(barplot_tmp$view), categories[c(3, 2)]), 1, function(row) paste(row, collapse=" | ")))

ggplot() +
  geom_bar(data=barplot_tmp[category == categories[3] | category == categories[2]], 
           aes(x=view, 
               y=value, 
               fill=fill_colour_category, 
               colour=fill_colour_category),
           stat="identity", width = 0.75) +
  facet_grid(~gender) +
  scale_fill_manual(breaks=barplot_labels_legend,
                    values = c("white", light_cols[1], "white", light_cols[2])) +
  scale_color_manual(breaks=barplot_labels_legend,
                     values = c(dark_cols[1], dark_cols[1], dark_cols[2], dark_cols[2])) +
  labs(x="View", 
       y = "Number of quiz participants") +
  geom_text(data = barplot_labels_tmp,
            aes(y=get(paste(strsplit(categories[2], split = " ")[[1]], collapse = "")) + 1, 
                x = view,
                label=sprintf("%.2f%%", get(paste(strsplit(categories[4], split = " ")[[1]], collapse = "")))), 
            vjust=1, size=3.5, color = c(dark_cols[1], dark_cols[2], dark_cols[2], dark_cols[1])) +
  geom_text(data = barplot_labels_tmp,
            aes(y=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = "")) + 1, 
                x = view, 
                label=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = ""))), 
            vjust=1, size=3.5, color = c(dark_cols[1], dark_cols[2], dark_cols[2], dark_cols[1])) +
  theme_linedraw() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_rect(fill="#f2f2f2"),
        strip.text = element_text(color="black"))

ggsave("plots/gend_number_of_people_playing_minigame.png", width = 8, height = 4.5)

# plot times mingame played per person & gender

mean_scores <- participants[, mean(number_has_viewed_game), .(view, gender)]
mean_scores <- mean_scores[gender %in% goi]
mean_scores <- mean_scores[view == "feedback"]
setnames(mean_scores, "V1", "mean_scores")

t.test(number_has_viewed_game ~ gender, participants[gender %in% goi & view == "feedback"])

ggplot(participants[gender %in% goi & view == "feedback"], 
       aes(x=gender, y=number_has_viewed_game)) + 
  geom_boxplot(width = 0.5,
               aes(colour=gender)) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.75,
               aes(fill=gender), 
               position = position_jitter(width = 0.25, height = 0.25, seed = 1)) +
  geom_text(data = mean_scores, 
            aes(x = gender, y = 10.5, label = sprintf("%.2f", mean_scores)),
            vjust = -0.5, color = "black") +
  scale_y_continuous(breaks=seq(0,10,2), limits = c(-0.3,11)) +
  labs(x="View", 
       y = "Frequency of minigame being played") +
  scale_fill_manual(values = c("#d1e5f0", "#fddbc7")) +
  scale_colour_manual(values = c("#67a9cf", "#ef8a62")) +
  theme_linedraw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("plots/gend_number_of_times_minigame_was_played_gender.png", width = 3, height = 4)

# plot number of people using external sources

categories <- c("Number of participants", "Participants using external sources", "Participants not using external sources", "Sources usage rate")

barplot_tmp <- do.call(rbind, lapply(goi, function(g) {
  tmp <- get_barplot_df(participants[gender == g], "has_used_google", categories)
  tmp[, gender := g]
  return(tmp)
}))
barplot_tmp[, gender := factor(gender, levels = goi)]

barplot_labels_tmp <- do.call(rbind, lapply(goi, function(g) {
  tmp <- get_barplot_labels(barplot_tmp[gender == g], categories)
  tmp[, gender := g]
  return(tmp)
}))
barplot_labels_tmp[, gender := factor(gender, levels = goi)]

barplot_labels_legend <- sort(apply(expand.grid(unique(barplot_tmp$view), categories[c(3, 2)]), 1, function(row) paste(row, collapse=" | ")))

ggplot() +
  geom_bar(data=barplot_tmp[category == categories[3] | category == categories[2]], 
           aes(x=view, 
               y=value, 
               fill=fill_colour_category, 
               colour=fill_colour_category),
           stat="identity", width = 0.75) +
  facet_grid(~gender) +
  scale_fill_manual(breaks=barplot_labels_legend,
                    values = c("white", light_cols[1], "white", light_cols[2])) +
  scale_color_manual(breaks=barplot_labels_legend,
                     values = c(dark_cols[1], dark_cols[1], dark_cols[2], dark_cols[2])) +
  labs(x="View", 
       y = "Number of quiz participants") +
  geom_text(data = barplot_labels_tmp,
            aes(y=get(paste(strsplit(categories[2], split = " ")[[1]], collapse = "")) + 1, 
                x = view,
                label=sprintf("%.2f%%", get(paste(strsplit(categories[4], split = " ")[[1]], collapse = "")))), 
            vjust=1, size=3.5, color = c(dark_cols[1], dark_cols[2], dark_cols[2], dark_cols[1])) +
  geom_text(data = barplot_labels_tmp,
            aes(y=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = "")) + 1, 
                x = view, 
                label=get(paste(strsplit(categories[1], split = " ")[[1]], collapse = ""))), 
            vjust=1, size=3.5, color = c(dark_cols[1], dark_cols[2], dark_cols[2], dark_cols[1])) +
  theme_linedraw() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_rect(fill="#f2f2f2"),
        strip.text = element_text(color="black"))

ggsave("plots/gend_number_of_people_checking_external_sources.png", width = 8, height = 4.5)



