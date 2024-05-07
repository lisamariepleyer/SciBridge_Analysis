library(data.table)
library(ggplot2)
library(RColorBrewer)

average_per_question_data <- readRDS("raw_data/average_per_question_data.rds")

light_cols <- c("#d0cfe7", "#bdbdbd")
dark_cols <- c("#756bb1", "#636363")

areas <- c("it", "physics_chemistry", "medicine", "climate_change", "ns")
areas_thesis <- c("IT", "Physics & Chemistry", "Medicine", "Climate Change")

area_cols_dark <- c("#f1a340", "#5ab4ac", "#e9a3c9", "#7fbf7b")
area_cols_light <- c("#fee0b6", "#c7eae5", "#fde0ef", "#d9f0d3")

average_per_question_data[, area := factor(area, levels = areas)]

col_num <- 10

# # define colours
# 
# heatmap.breaks = seq(0,1,0.05)
# heatmap.cols <- colorRampPalette(c("#f7f7f7", "#636363"))
# heatmap.cols <- heatmap.cols(length(heatmap.breaks) - 1)
# 
# # plot how frequently a questionw as answered correctly
# 
# # plot legend
# 
# legend.mat <- t(as.matrix(heatmap.breaks))
# 
# png("plots/ques_heatmap_legend.png", height = 175)
# 
# image(t(legend.mat), 
#       col = heatmap.cols, 
#       breaks = heatmap.breaks, 
#       axes = F)
# 
# axis(side = 1, labels = c("0", "0.5", "1"), at = c(0,0.5,1), las = 0, cex.axis = 2, col = "white")
# #abline(v = seq(-1/(2*(ncol(legend.mat)-1)), 1+1/(2*(ncol(legend.mat)-1)), length.out = ncol(legend.mat)+1), lwd = 2) # add lines between colours
# 
# abline(v = seq(0-1/col_num/4.4, 1+1/col_num/4.4, length.out = 2), lwd = 4) # add line at beginning and end of bar
# abline(h = c(-1,1), lwd = 4)
# 
# dev.off()
# 
# # squares
# 
# png("plots/ques_heatmap_correctly_answered_rate.png", height = 175)
# 
# image(as.matrix(average_per_question_data$perc_answered_correctly), 
#       col = heatmap.cols, 
#       breaks = heatmap.breaks, 
#       axes = F)
# 
# axis(side = 2, labels = "Correct [%]", at = 0, las = 1, cex.axis = 2, col = "white", col.ticks = "black")
# axis(side = 3, labels = 1:col_num, at = seq(0, 1, length.out = col_num), las = 2, cex.axis = 2, col = "white", col.ticks = "black")
# 
# abline(v = c(0-1/col_num/1.9, seq(-1/(2*(col_num-1)), 1+1/(2*(col_num-1)), length.out = col_num+1), 1+1/col_num/1.9), lwd = 2)
# abline(h = c(-1,1), lwd = 4)
# 
# dev.off()

# plot how frequently a question was answered correctly

tmp <- data.table(type = rep(c("incorrect", "correct"), each = col_num),
                  value = c(average_per_question_data[, total_submissions - no_answered_correctly], average_per_question_data[, no_answered_correctly]),
                  question = rep(average_per_question_data[, question], 2),
                  area = rep(average_per_question_data[, area], 2))

lapply(areas, function(a) {
  tmp <- tmp[area == a, area := areas_thesis[which(areas == a)]]
})

area_labels <- do.call(paste0,expand.grid(c("incorrect", "correct"), " | ", areas_thesis))
tmp[, label := factor(paste(type, area, sep = " | "), levels = area_labels)]

ggplot() +
  geom_bar(data=tmp, 
           aes(x=question, 
               y=value, 
               fill=label,
               color=label),
           stat="identity", width = 0.75) +
  scale_x_continuous(breaks=seq(1, 10)) +
  scale_y_continuous(limits = c(0, max(tmp[,sum(value),question]$V1) * 1.2), expand = c(0, 0)) +
  scale_color_manual(values = rep(area_cols_dark, each = 2)) +
  scale_fill_manual(values = c(rbind(area_cols_light,matrix(rep("white", length(area_cols_light)),ncol=length(area_cols_light))))) +
  #scale_fill_manual(values = c(light_cols[2], "white")) +
  labs(x="View", 
       y = "Number of question submissions") +
  geom_text(data = average_per_question_data[, .(question, perc_answered_correctly, no_answered_correctly)],
            aes(y=no_answered_correctly-6, 
                x=question, 
                label=sprintf("%.2f%%", perc_answered_correctly*100),
                angle = 90), 
            vjust=0.5, size=3.5) +
  geom_text(data = average_per_question_data[, .(question, total_submissions)],
            aes(y=total_submissions + 3, 
                x=question, 
                label=total_submissions),
            vjust=1, size=3.5) +
  theme_linedraw() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank())

ggsave("plots/ques_barplot_correctly_answered_rate.png", width = 8, height = 4)

# plot average answering time

ggplot(average_per_question_data,
       aes(x=question)) +
  geom_bar(aes(y=mean_time_to_answer,
               fill=area, color=area),
           stat="identity", width = 0.75) +
  geom_errorbar(aes(ymax=mean_time_to_answer + std_time_to_answer,
                    ymin=mean_time_to_answer - std_time_to_answer,
                    color=area),
                width = 0.25) +
  scale_x_continuous(breaks=seq(1, 10)) +
  scale_y_continuous(limits = c(min(average_per_question_data[, mean_time_to_answer - std_time_to_answer]), max(average_per_question_data[, mean_time_to_answer + std_time_to_answer]) * 1.2), 
                     expand = c(0, 0)) +
  scale_fill_manual(values=area_cols_light, labels=areas_thesis) +
  scale_color_manual(values=area_cols_dark, labels=areas_thesis) +
  labs(x="View", 
       y = "Time [seconds]") +
  geom_label(aes(y=mean_time_to_answer+4,
                label=sprintf("%.2fs", mean_time_to_answer)), 
            vjust=1, size=3.5, label.size=0) +
  theme_linedraw() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank())

ggsave("plots/ques_barplot_average_time_to_answer.png", width = 8, height = 4)

# plot how frequently the additional sources were quecked per question

tmp <- data.table(type = rep(c("checked", "not checked"), each = col_num),
                  value = c(average_per_question_data[, no_has_viewed_source], average_per_question_data[, total_submissions - no_has_viewed_source]),
                  question = rep(average_per_question_data[, question], 2),
                  area = rep(average_per_question_data[, area], 2))

lapply(areas, function(a) {
  tmp <- tmp[area == a, area := areas_thesis[which(areas == a)]]
})

area_labels <- do.call(paste0,expand.grid(c("not checked", "checked"), " | ", areas_thesis))
tmp[, label := factor(paste(type, area, sep = " | "), levels = area_labels)]

ggplot() +
  geom_bar(data=tmp, 
           aes(x=question, 
               y=value, 
               fill=label,
               color=label),
           stat="identity", width = 0.75) +
  scale_x_continuous(breaks=seq(1, 10)) +
  scale_y_continuous(limits = c(0, max(tmp[,sum(value),question]$V1) * 1.2), expand = c(0, 0)) +
  scale_color_manual(values = rep(area_cols_dark, each = 2)) +
  scale_fill_manual(values = c(rbind(matrix(rep("white", length(area_cols_light)), ncol=length(area_cols_light)),
                                     area_cols_light))) +
  #scale_fill_manual(values = c(light_cols[2], "white")) +
  labs(x="View", 
       y = "Number of question submissions") +
  geom_text(data = average_per_question_data[, .(question, perc_has_viewed_source, no_has_viewed_source)],
            aes(y=no_has_viewed_source + 6, 
                x=question, 
                label=sprintf("%.2f%%", perc_has_viewed_source*100)), 
            vjust=0.5, size=3.5,
            angle = 90) +
  geom_text(data = average_per_question_data[, .(question, total_submissions)],
            aes(y=total_submissions + 3, 
                x=question, 
                label=total_submissions),
            vjust=1, size=3.5) +
  theme_linedraw() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank())

ggsave("plots/ques_barplot_have_checked_sources_rate.png", width = 8, height = 4)

# plot how frequently a question was answered correctly

tmp <- data.table(type = rep(c("checked", "not checked"), each = col_num),
                  value = c(average_per_question_data[, no_has_viewed_game], average_per_question_data[, total_submissions - no_has_viewed_game]),
                  question = rep(average_per_question_data[, question], 2),
                  area = rep(average_per_question_data[, area], 2))

lapply(areas, function(a) {
  tmp <- tmp[area == a, area := areas_thesis[which(areas == a)]]
})

area_labels <- do.call(paste0,expand.grid(c("not checked", "checked"), " | ", areas_thesis))
tmp[, label := factor(paste(type, area, sep = " | "), levels = area_labels)]

ggplot() +
  geom_bar(data=tmp, 
           aes(x=question, 
               y=value, 
               fill=label,
               color=label),
           stat="identity", width = 0.75) +
  scale_x_continuous(breaks=seq(1, 10)) +
  scale_y_continuous(limits = c(0, max(tmp[,sum(value),question]$V1) * 1.2), expand = c(0, 0)) +
  scale_color_manual(values = rep(area_cols_dark, each = 2)) +
  scale_fill_manual(values = c(rbind(matrix(rep("white", length(area_cols_light)), ncol=length(area_cols_light)),
                                     area_cols_light))) +
  #scale_fill_manual(values = c(light_cols[2], "white")) +
  labs(x="View", 
       y = "Number of question submissions") +
  geom_text(data = average_per_question_data[, .(question, perc_has_viewed_game, no_has_viewed_game)],
            aes(y=no_has_viewed_game + 6, 
                x=question, 
                label=sprintf("%.2f%%", perc_has_viewed_game*100)), 
            vjust=0.5, size=3.5,
            angle = 90) +
  geom_text(data = average_per_question_data[, .(question, total_submissions)],
            aes(y=total_submissions + 3, 
                x=question, 
                label=total_submissions),
            vjust=1, size=3.5) +
  theme_linedraw() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank())

ggsave("plots/ques_barplot_have_played_minigame_rate.png", width = 8, height = 4)
