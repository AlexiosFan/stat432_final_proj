library(devtools)
## Data
train_logs <-read.csv("/Users/melchizedek/Library/CloudStorage/OneDrive-HKUSTConnect/STAT/STAT432/Final_Proj/train_logs.csv")
train_scores <-read.csv("/Users/melchizedek/Library/CloudStorage/OneDrive-HKUSTConnect/STAT/STAT432/Final_Proj/train_scores.csv")


## Processing
users <- unique(train_logs$id)
length(users)


train_1 <- matrix(0, nrow = length(users), ncol = 11)
colnames(train_1) <- c("id","Score", "present_of_Nonproduction", "present_of_Input", "present_of_RemoveCut", "present_of_Paste", "present_of_Replace", "present_of_MoveFromTo", "tuning_of_down_time", "tuning_of_up_time", "tuning_of_action_time")

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

    tuning_of_down_time = var(user_logs$down_time[user_logs$activity == "Input"])
    tuning_of_up_time = var(user_logs$up_time[user_logs$activity == "Input"])
    tuning_of_action_time = var(user_logs$action_time[user_logs$activity == "Input"])

    train_1[i,2] <- as.numeric(train_scores[train_scores$id == user,2])
    train_1[i,3] <- as.numeric(present_of_Nonproduction)
    train_1[i,4] <- as.numeric(present_of_Input)
    train_1[i,5] <- as.numeric(present_of_RemoveCut)
    train_1[i,6] <- as.numeric(present_of_Paste)
    train_1[i,7] <- as.numeric(present_of_Replace)
    train_1[i,8] <- as.numeric(present_of_MoveFromTo)
    train_1[i,9] <- as.numeric(tuning_of_down_time)
    train_1[i,10] <- as.numeric(tuning_of_up_time)
    train_1[i,11] <- as.numeric(tuning_of_action_time)
}

print(colnames(train_1))

save(train_1, file = "/Users/melchizedek/Library/CloudStorage/OneDrive-HKUSTConnect/STAT/STAT432/Final_Proj/MyData.RData")
save(users, file = "/Users/melchizedek/Library/CloudStorage/OneDrive-HKUSTConnect/STAT/STAT432/Final_Proj/users.RData")
