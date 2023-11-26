if (!require('this.path')) install.packages('this.path')
library(devtools)
library(this.path)
cur_dir2 = dirname(this.path())
cur_dir2
# Data
train_logs <-read.csv(paste(cur_dir2, "/train_logs.csv", sep = ""))
train_scores <-read.csv(paste(cur_dir2, "/train_scores.csv", sep = ""))
users <- unique(train_logs$id)
length(users)

# model (0 for whole set and save the data, 1 for whole set but not save the data, 2 for only the first user and not save the data)
model = 2
length_of_loop = length(users)
if(model == 2) length_of_loop = 1

## Processing

# the names of variables
names_of_variables <- c("id","Score", "present_of_Nonproduction", "present_of_Input", "present_of_RemoveCut", "present_of_Paste", "present_of_Replace", "present_of_MoveFromTo",  
                "tuning_of_action_time", "tuning_space_time", "fluquence_of_change", "word_time_ratio", "word_event_ratio","event_time_ratio","idle_time_ratio","tuning_sentence_length")
MyDataSet <- matrix(0, nrow = length_of_loop, ncol = length(names_of_variables))
colnames(MyDataSet) <- names_of_variables

# begin processing
print("processing began")
for(i in 1: length_of_loop){ 

    # current user's name 
    user <- users[i]
    user_logs <- train_logs[train_logs$id == user,]


    # precent of all activities
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


    # tuning varible of action time
    tuning_of_action_time = var(user_logs$action_time[user_logs$activity == "Input"])

    # tuning vrailve of spacing time 
    tuning_time_space = rep(0, length(user_logs$activity)-1)

    for(j in 1:(length(user_logs$activity)-1)){
        tuning_time_space = user_logs$down_time[j+1] - user_logs$up_time[j]
    }

    # the fluquence of change inputs
    fluquence_of_change = rep(0, length(user_logs$activity)-1)

    for(j in 1:(length(user_logs$activity)-1)){
        fluquence_of_change[j] = (user_logs$activity[j+1] != user_logs$activity[j])
    }



    # ratios 
    word_count_max = max(user_logs$word_count)
    event_id_max = max(user_logs$event_id)
    up_time_max = max(user_logs$up_time)
    action_time_gap_sum = sum(tuning_time_space)
    word_time_ratio = word_count_max / up_time_max
    word_event_ratio = word_count_max / event_id_max
    event_time_ratio = event_id_max  / up_time_max
    idle_time_ratio = action_time_gap_sum / up_time_max

    # tuning sentence length
    num_sentence <- length(user_logs$activity[user_logs$up_event == '.']) - length(user_logs$activity[user_logs$up_event == 'Backspace' & user_logs$text_change == '.' ])
    sentence_length <- rep(0, num_sentence)
    count <- 1
    sum_of_pre_word <- 0
    for(i in 1:length(user_logs$activity)){
        if(user_logs$up_event[i] == '.'){
            sentence_length[count] = user_logs$word_count[i] - sum_of_pre_word
            sum_of_pre_word = sum_of_pre_word + sentence_length[count]
            count = count + 1
        }
        if(user_logs$up_event[i] == 'Backspace' & user_logs$text_change[i] == '.' ){
            count = count - 1
            sum_of_pre_word = sum_of_pre_word - sentence_length[count]
        }
    }


    # data pull in 
    MyDataSet[i,1] <- user
    MyDataSet[i,2] <- as.numeric(train_scores[train_scores$id == user,2])
    MyDataSet[i,3] <- as.numeric(present_of_Nonproduction)
    MyDataSet[i,4] <- as.numeric(present_of_Input)
    MyDataSet[i,5] <- as.numeric(present_of_RemoveCut)
    MyDataSet[i,6] <- as.numeric(present_of_Paste)
    MyDataSet[i,7] <- as.numeric(present_of_Replace)
    MyDataSet[i,8] <- as.numeric(present_of_MoveFromTo)
    MyDataSet[i,9] <- as.numeric(tuning_of_action_time)
    MyDataSet[i,10] <- as.numeric(mean(tuning_time_space))
    MyDataSet[i, 11] <- as.numeric(sum(fluquence_of_change)/length(fluquence_of_change))
    MyDataSet[i, 12] <- as.numeric(word_time_ratio)
    MyDataSet[i, 13] <- as.numeric(word_event_ratio)
    MyDataSet[i, 14] <- as.numeric(event_time_ratio)
    MyDataSet[i, 15] <- as.numeric(idle_time_ratio)

    if(i == 1) {
        # first user's data
        first_urser_data <- user_logs
    }
}

print(colnames(MyDataSet))

if(model != 2){
    save(MyDataSet, file = paste(cur_dir2, "/MyData.RData", sep = ""))
    save(users, file = paste(cur_dir2, "/users.RData", sep = ""))
}
