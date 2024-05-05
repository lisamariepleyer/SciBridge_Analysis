library(data.table)
library(ggplot2)
library(RColorBrewer)

average_per_question_data <- readRDS("raw_data/average_per_question_data.rds")

light_cols <- c("#d0cfe7", "#bdbdbd")
dark_cols <- c("#756bb1", "#636363")

areas <- c("it", "physics_chemistry", "medicine", "climate_change", "ns")

# define colours

heatmap.breaks = seq(0,1,0.05)
heatmap.cols <- colorRampPalette(c("#f7f7f7", "#636363"))
heatmap.cols <- heatmap.cols(length(heatmap.breaks) - 1)

col_num <- 10

# plot legend

legend.mat <- t(as.matrix(heatmap.breaks))

png("plots/ques_heatmap_legend.png", height = 175)

image(t(legend.mat), 
      col = heatmap.cols, 
      breaks = heatmap.breaks, 
      axes = F)

axis(side = 1, labels = c("0", "0.5", "1"), at = c(0,0.5,1), las = 0, cex.axis = 2, col = "white")
#abline(v = seq(-1/(2*(ncol(legend.mat)-1)), 1+1/(2*(ncol(legend.mat)-1)), length.out = ncol(legend.mat)+1), lwd = 2) # add lines between colours

abline(v = seq(0-1/col_num/4.4, 1+1/col_num/4.4, length.out = 2), lwd = 4) # add line at beginning and end of bar
abline(h = c(-1,1), lwd = 4)

dev.off()

# plot how frequently a questionw as answered correctly

png("plots/ques_heatmap_correctly_answered_rate.png", height = 175)

image(as.matrix(average_per_question_data$perc_answered_correctly), 
      col = heatmap.cols, 
      breaks = heatmap.breaks, 
      axes = F)

axis(side = 2, labels = "Correct [%]", at = 0, las = 1, cex.axis = 2, col = "white", col.ticks = "black")
axis(side = 3, labels = 1:col_num, at = seq(0, 1, length.out = col_num), las = 2, cex.axis = 2, col = "white", col.ticks = "black")

abline(v = c(0-1/col_num/1.9, seq(-1/(2*(col_num-1)), 1+1/(2*(col_num-1)), length.out = col_num+1), 1+1/col_num/1.9), lwd = 2)
abline(h = c(-1,1), lwd = 4)

dev.off()

tmp <- data.table(type = rep(c("incorrect", "correct"), each = col_num),
                  value = c(average_per_question_data[, total_submissions - no_answered_correctly], average_per_question_data[, no_answered_correctly]),
                  question = rep(average_per_question_data[, question], 2))

tmp[, type := factor(type, levels = c("incorrect", "correct"))]

ggplot() +
  geom_bar(data=tmp, 
           aes(x=question, 
               y=value, 
               fill=type),
           stat="identity", width = 0.75, color = dark_cols[2]) +
  scale_x_continuous(breaks=seq(1, 10)) +
  scale_y_continuous(limits = c(0, max(tmp[,sum(value),question]$V1) * 1.2), expand = c(0, 0)) +
  scale_fill_manual(values = c(light_cols[2], "white")) +
  labs(title="Question Statistics", 
       x="View", 
       y = "Number of question submissions") +
  geom_text(data = average_per_question_data[, .(question, perc_answered_correctly, no_answered_correctly)],
            aes(y=no_answered_correctly-1, 
                x=question, 
                label=sprintf("%.2f%%", perc_answered_correctly*100)), 
            vjust=1, size=3.5, color=dark_cols[2]) +
  geom_text(data = average_per_question_data[, .(question, total_submissions)],
            aes(y=total_submissions + 2, 
                x=question, 
                label=total_submissions), 
            vjust=1, size=3.5, color=dark_cols[2]) +
  theme_linedraw() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank())

ggsave("plots/ques_barplot_correctly_answered_rate.png", width = 10, height = 5)

