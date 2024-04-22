library(data.table)
library(ggplot2)

source("scripts/helper_functions.R")

average_per_user_data <- readRDS("raw_data/average_per_user_data.rds")

light_cols <- c("#d0cfe7", "#bdbdbd")
dark_cols <- c("#756bb1", "#636363")

views <- c("feedback", "plain")
age_groups <- c('unter 18', '18-24', '25-34', '35-44', '45-54', '55-64', '65-74', '75-84', 'über 85', "kA")
genders <- c("männlich", "weiblich", "nicht-binär", "andere", "kA")
levels <- c("1", "2", "3", "ka")
areas <- c("it", "physics_chemistry", "medicine", "climate_change", "ns")

participants <- average_per_user_data[number_of_answered_questions > 0]

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

# plot number of participants per view

ggplot(participants[, .N, view],
       aes(x=view,
           y=N,
           color=view)) +
  geom_bar(stat="identity", width=0.75, fill="white") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(average_per_user_data[number_of_answered_questions > 0, .N, view]$N) * 1.1)) +
  scale_color_manual(values = dark_cols) +
  labs(title="View Statistics", 
       x="View", 
       y = "Number of Participants") +
  geom_text(y = 1,
            aes(label=N),
            size=3.5) +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        legend.position = "none")

ggsave("plots/demo_participants_per_view.png", width = 2, height = 4)

# plot number of participants per age group

number_in_group_per_view <- get_number_in_group_per_view(participants, "age", age_groups)

ggplot(number_in_group_per_view,
       aes(x=age,
           y=N)) +
  geom_bar(stat="identity", width=0.75, fill="white", color=dark_cols[2]) +
  facet_wrap(~view) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(number_in_group_per_view$N) * 1.1)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title="Per View Age Group Distribution", 
       x="View", 
       y = "Number of Participants") +
  geom_text(y = 0.5,
            aes(label=N),
            size=3.5, color=dark_cols[2]) +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        legend.position = "none")

ggsave("plots/demo_participants_per_age_group.png", width = 8, height = 4)

# plot number of participants per gender

number_in_group_per_view <- get_number_in_group_per_view(participants, "gender", genders)

ggplot(number_in_group_per_view,
       aes(x=gender,
           y=N)) +
  geom_bar(stat="identity", width=0.75, fill="white", color=dark_cols[2]) +
  facet_wrap(~view) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(number_in_group_per_view$N) * 1.1)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title="Per View Gender Distribution", 
       x="View", 
       y = "Number of Participants") +
  geom_text(y = 0.5,
            aes(label=N),
            size=3.5, color=dark_cols[2]) +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        legend.position = "none")

ggsave("plots/demo_participants_per_gender.png", width = 5, height = 4)

# plot number of experts per knowledge field

number_in_group_per_view <- do.call(rbind,
        lapply(c("level_it", "level_physics_chemistry", "level_medicine", "level_climate_change"), 
               function(l) {
         tmp <- get_number_in_group_per_view(participants, l, levels)[, 
                                                                          `:=` (area = substr(l, 7, nchar(l)), 
                                                                                knowledge_level = get(l))]
         
         setkey(tmp, "area")
         tmp[, (l):=NULL]
         
         return(tmp)
}))

tmp <- unique(number_in_group_per_view[knowledge_level=="ka", .(view, N, knowledge_level)])
tmp[, area:="ns"]
number_in_group_per_view <- number_in_group_per_view[knowledge_level!="ka"]
number_in_group_per_view <- rbind(number_in_group_per_view, tmp)

number_in_group_per_view[, knowledge_level := factor(knowledge_level, levels = levels)]
number_in_group_per_view [, area := factor(area, levels = areas)]

ggplot(number_in_group_per_view,
       aes(x=area,
           y=N,
           fill=knowledge_level)) +
  geom_bar(position="dodge", stat="identity", width=0.75, color = "black") +
  facet_wrap(~view) +
  scale_y_continuous(breaks=seq(0, max(number_in_group_per_view$N), 2),
                     expand = c(0, 0),
                     limits = c(0, max(number_in_group_per_view$N) * 1.1)) +
  scale_fill_manual(values = c("#f0f0f0","#bdbdbd", "#636363", "#f7f7f7"),
                    labels = c("no prior knowledge", "heard about it", "dealt with it", "ns")) +
  scale_x_discrete(labels=c("it" = "IT", "physics_chemistry" = "Physics & Chemistry", "medicine" = "Medicine", "climate_change" = "Climate Change")) +
  labs(title="Knowledge Level Statistics", 
       x="View", 
       y = "Number of Participants") +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 20, hjust = 1),
        legend.title = element_blank())

ggsave("plots/demo_participants_per_knowledge_area.png", width = 7, height = 4)
