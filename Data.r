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
model = 0
length_of_loop = length(users)
if(model == 2) {
    length_of_loop = 1
    i = 1
}

## Processing

## helper function
# input text processing
extractNumbers <- function(text) {
  # Regular expression to match numbers
  matches <- gregexpr("\\d+", text)
  numbers <- regmatches(text, matches)[[1]]
  numbers <- as.numeric(numbers) # Convert to numeric
  return(numbers)
}

calculateSentenceLengths <- function(essayText) {
  # Find all indices of sentence-ending punctuation
  punctuationIndices <- unlist(gregexpr("[.!?]", essayText))

  # Check if there are no sentence-ending punctuations
  if (length(punctuationIndices) == 0) {
    return(c(nchar(essayText)))  # Return the entire text length if no punctuations are found
  }

  # Add the start and end of the text to the indices for proper calculation
  punctuationIndices <- c(0, punctuationIndices, nchar(essayText) + 1)

  # Calculate lengths of each sentence
  sentenceLengths <- diff(punctuationIndices) - 1

  # Remove zeros from sentence lengths
  sentenceLengths <- sentenceLengths[sentenceLengths != 0]

  return(sentenceLengths)
}

processingInputs <- function(currTextInput) {
  essayText <- ""

  for (k in 1:nrow(currTextInput)) {
    Input <- currTextInput[k, ]
    activity <- as.character(Input[1])
    cursor_position <- as.numeric(Input[2])
    text_change <- as.character(Input[3])

    if (activity == "Replace") {
      replaceTxt <- unlist(strsplit(text_change, " => "))
      before <- replaceTxt[1]
      after <- replaceTxt[2]
      essayText <- paste0(substr(essayText, 1, cursor_position - nchar(after)), 
                          after, 
                          substr(essayText, cursor_position + nchar(before) - nchar(after) + 1, nchar(essayText)))
    } else if (activity == "Paste") {
      essayText <- paste0(substr(essayText, 1, cursor_position - nchar(text_change)), 
                          text_change, 
                          substr(essayText, cursor_position - nchar(text_change) + 1, nchar(essayText)))
    } else if (activity == "Remove/Cut") {
      essayText <- paste0(substr(essayText, 1, cursor_position), 
                          substr(essayText, cursor_position + nchar(text_change) + 1, nchar(essayText)))
    } else if (grepl("Move From", activity)) {
      moveData <- extractNumbers(activity)
      if (moveData[1] != moveData[3]) {
        if (moveData[1] < moveData[3]) {
          movingText <- substr(essayText, moveData[1], moveData[2])
          essayText <- paste0(substr(essayText, 1, moveData[1] - 1), 
                              substr(essayText, moveData[2] + 1, moveData[3]), 
                              movingText, 
                              substr(essayText, moveData[3] + 1, nchar(essayText)))
        } else {
          movingText <- substr(essayText, moveData[1], moveData[2])
          essayText <- paste0(substr(essayText, 1, moveData[3] - 1), 
                              movingText, 
                              substr(essayText, moveData[3], moveData[1] - 1), 
                              substr(essayText, moveData[2] + 1, nchar(essayText)))
        }
      }
    } else if (activity == "Input") {
      # Assuming 'input' activity
      if(cursor_position == 1){
        essayText <- paste0(text_change, essayText)
      } else if(cursor_position == nchar(essayText) + 1){
        essayText <- paste0(essayText, text_change)
      } else {
        essayText <- paste0(substr(essayText, 1, cursor_position - 1), 
                            text_change, 
                            substr(essayText, cursor_position, nchar(essayText)))
      }
    }else{
        # Assuming 'Nonproduction' activity
        essayText <- essayText
    }
  }
  return(essayText)
}



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
    currTextInput <- user_logs[c("activity", "cursor_position", "text_change")]
    essayText <- processingInputs(currTextInput)
    length_of_text <- nchar(essayText)
    
    # number of sentences
    num_of_sentence = length(gregexpr("[.!?]", essayText)[[1]])
    sentence_length <- calculateSentenceLengths(essayText)


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
    MyDataSet[i,10] <- as.numeric(mean(tuning_time_space)) # tuning_time_space
    MyDataSet[i, 11] <- as.numeric(sum(fluquence_of_change)/length(fluquence_of_change)) #`fluquence_of_change
    MyDataSet[i, 12] <- as.numeric(word_time_ratio) #`word_time_ratio
    MyDataSet[i, 13] <- as.numeric(word_event_ratio) #`word_event_ratio
    MyDataSet[i, 14] <- as.numeric(event_time_ratio) # tuning event time
    MyDataSet[i, 15] <- as.numeric(idle_time_ratio) # tuning idle time
    MyDataSet[i, 16] <- as.numeric(mean(sentence_length)) # tuning sentence length

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
