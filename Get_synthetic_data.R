#get simulated data out of our actual data

# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not,
# then load them into the R session if load_pkg is set to true.

ipak <- function(pkg, load_pkg = T){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)){
    install.packages(new.pkg, dependencies = TRUE)}
  if (load_pkg){
    sapply(pkg, require, character.only = TRUE)}
}

# usage
packages_to_load <- c("data.table", "dplyr", "brms", "ggplot2", "patchwork")

packages_not_to_load <- c("readxl", "mclogit", "job", "stringr",
                          "nnet", "stevemisc", "coda", "bayestestR",
                          "performance", "insight", "ggokabeito",
                          "tidybayes", "ggthemes")

ipak(packages_to_load)

ipak(packages_not_to_load, load_pkg = F)

# the data ----


dt_or <- readRDS("dt.rds")

dt_or<- dt_or |>
  select(-c("Omission", "Congruence", "Congruence_resp", "Age_months", "Answer"))

summary(dt_or)

table(dt_or$Answer_type)

table(dt_or$Item, dt_or$Age_group)[1:5,]

table(dt_or$Item, dt_or$Condition)[1:5,]

table(dt_or$Participant, dt_or$Age_group)[c(1:2, 20:22),]

table(dt_or$Participant, dt_or$Condition)[1:5,]



syn_dt <- data.frame(Participant = as.factor(rep(1:80, each = 34)),
                     Item = as.factor(rep(1:34, times = 80)),
                     Age_group = rep(levels(dt_or$Age_group), each = 680),
                     Condition = rep(levels(dt_or$Condition),
                                     times = c(10, 10, 4, 10)),
                     Answer_type = NA,
                     stringsAsFactors = T)



for_sampling <- levels(dt_or$Answer_type)

for_prob <- c(4,0,8,1,0,3,9)

for (i in 1:nrow(syn_dt)) {
  current_prob <- c(rep(0, times = 7))
  current_dt <- syn_dt[i,]
  #change depending on age group
  current_prob <- case_when(current_dt$Age_group == "3 y/o" ~
                              for_prob + c(-3,0,-7,0,+2,-1,-4),
                            current_dt$Age_group == "4 y/o" ~
                              for_prob + c(-2,+1,-3,0,+1,+1,0),
                            current_dt$Age_group == "5 y/o" ~
                              for_prob + c(+1,+2,2,0,0,+2,0),
                            current_dt$Age_group == "Adults" ~
                              for_prob + c(+5,0,0,0,0,-2,0))
  #change depending on Condition
  current_prob <- case_when(current_dt$Condition == "Adjunct" ~
                              current_prob + c(0,0,0,0,0,-1,+2),
                            current_dt$Condition == "Direct_object" ~
                              current_prob + c(0,+3,+1,0,+1,+1,0),
                            current_dt$Condition == "Indirect_object" ~
                              current_prob + c(-1,0,0,0,0,+2,0),
                            current_dt$Condition == "Subject" ~
                              current_prob + c(+5,0,0,0,0,0,0))


  syn_dt$Answer_type[i] <- sample(for_sampling, size = 1, prob = current_prob)

}

syn_dt$Answer_type <- as.factor(syn_dt$Answer_type)

table(syn_dt$Answer_type)

table(syn_dt$Item, syn_dt$Age_group)[1:5,]

table(syn_dt$Item, syn_dt$Condition)[1:5,]

table(syn_dt$Participant, syn_dt$Age_group)[c(1:2, 20:22),]

table(syn_dt$Participant, syn_dt$Condition)[1:5,]

summary(syn_dt)


#saveRDS(syn_dt, file = "synthetic_dt.rds")

#show how different they are:
p1 <- ggplot(dt_or, aes(x = Condition, fill = Answer_type))+
  geom_bar()+
  facet_wrap(vars(Age_group), nrow = 2)+
  ggtitle("original data")

p2 <- ggplot(syn_dt, aes(x = Condition, fill = Answer_type))+
  geom_bar()+
  facet_wrap(vars(Age_group), nrow = 2)+
  ggtitle("synthetic data")

(p1 | p2)
