if (!require('this.path')) install.packages('this.path')
library(devtools)
library(this.path)
cur_dir2 = dirname(this.path())
cur_dir2
## Data
train_logs <-read.csv(paste(cur_dir2, "/train_logs.csv", sep = ""))
train_scores <-read.csv(paste(cur_dir2, "/train_score.csv", sep = ""))


## Processing
users <- unique(train_logs$id)
length(users)

names <- c("id","Score", "present_of_Nonproduction", "present_of_Input", "present_of_RemoveCut", "present_of_Paste", "present_of_Replace", "present_of_MoveFromTo",  "tuning_of_action_time", "tuning_value")
train_1 <- matrix(0, nrow = length(users), ncol = length(names))
colnames(train_1) <- names

for(i in 1: length(users)){ # 1){ #

    user <- users[i]
    user_logs <- train_logs[train_logs$id == user,]

    train_1[i,1] <- user

    acticity <- unique(user_logs$activity)
    
    num_of_Nonproduction = nrow(user_logs[user_logs$activity == "Nonproduction",])
    num_of_Input = nrow(user_logs[user_logs$activity == "Input",])
    num_of_RemoveCut = nrow(user_logs[user_logs$activity == "Remove/Cut",])
    num_of_Paste = nrow(user_logs[user_logs$activity == "Paste",])
    num_of_Replace = nrow(user_logs[user_logs$activity == "Replace",])
    num_of_MoveFromTo = length(user_logs$activity) - num_of_Nonproduction - num_of_Input - num_of_RemoveCut - num_of_Paste - num_of_Replace

    present_of_Nonproduction = num_of_Nonproduction / length(user_logs$activity)
    present_of_Input = num_of_Input / length(user_logs$activity)
    present_of_RemoveCut = num_of_RemoveCut / length(user_logs$activity)
    present_of_Paste = num_of_Paste / length(user_logs$activity)
    present_of_Replace = num_of_Replace / length(user_logs$activity)
    present_of_MoveFromTo = num_of_MoveFromTo / length(user_logs$activity)

    tuning_of_action_time = var(user_logs$action_time[user_logs$activity == "Input"])
    tuning_s = rep(0,length(acticity))
    for(j in 1:(length(acticity)-1)){
        tuning_value[j] = train_logs$down_time[j+1] - train_logs$up_time[j]
    }

    train_1[i,2] <- as.numeric(train_scores[train_scores$id == user,2])
    train_1[i,3] <- as.numeric(present_of_Nonproduction)
    train_1[i,4] <- as.numeric(present_of_Input)
    train_1[i,5] <- as.numeric(present_of_RemoveCut)
    train_1[i,6] <- as.numeric(present_of_Paste)
    train_1[i,7] <- as.numeric(present_of_Replace)
    train_1[i,8] <- as.numeric(present_of_MoveFromTo)
    train_1[i,9] <- as.numeric(tuning_of_action_time)
}

print(colnames(train_1))

save(train_1, file = paste(cur_dir2, "/MyData.RData", sep = ""))
save(users, file = paste(cur_dir2, "/users.RData", sep = ""))