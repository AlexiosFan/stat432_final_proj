if (!require('this.path')) install.packages('this.path')
if (!require('devtools')) install.packages('devtools')

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
first = 0
if(model == 2) {
    length_of_loop = 1
    i = 1
    # frist be a random user
    first = sample(1:length(users), 1)
    length_of_loop = 1
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

calculateWordLengths <- function(text) {
  # 使用正则表达式分割文本，包括空格、逗号、问号、句号、引号和分号
  words <- unlist(strsplit(text, "[ ,.?\"';!\']+"))
  # 移除空字符串
  words <- words[words != ""]
  # 计算每个单词的长度
  wordLengths <- sapply(words, nchar)
  return(wordLengths)
}

calculateParagraphCount <- function(text) {
  # 使用 '\n' 分割文本为段落
  paragraphs <- unlist(strsplit(text, "\n"))
  # 移除长度为 0 的段落
  paragraphs <- paragraphs[nchar(paragraphs) > 0]
  # 计算段落数
  paragraphCount <- length(paragraphs)
  return(paragraphCount)
}



# the names of variables
names_of_variables <- c("id","Score", "present_of_Nonproduction", "present_of_Input", "present_of_RemoveCut", "present_of_Paste", "present_of_Replace", "present_of_MoveFromTo",  
                "tuning_of_action_time", "tuning_space_time", "fluquence_of_change", "word_time_ratio", "word_event_ratio","event_time_ratio","idle_time_ratio","tuning_sentence_length", "num_of_sentence"
                , "tuning_word_length", "num_of_words", "num_of_paragraphs")
MyDataSet <- matrix(0, nrow = length_of_loop, ncol = length(names_of_variables))
colnames(MyDataSet) <- names_of_variables

# begin processing
print("processing began")
for(k in 1 : length_of_loop){ 

    i = k + first 
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

    # sentence length
    sentence_length <- calculateSentenceLengths(essayText)

    # tuning word length
    word_length <- calculateWordLengths(essayText)

    #num of words
    num_of_words = length(word_length)
    
    # num of paragraphs
    num_of_paragraphs <- calculateParagraphCount(essayText)


    # data pull in 
    MyDataSet[k,1] <- user
    MyDataSet[k,2] <- as.numeric(train_scores[train_scores$id == user,2])
    MyDataSet[k,3] <- as.numeric(present_of_Nonproduction)
    MyDataSet[k,4] <- as.numeric(present_of_Input)
    MyDataSet[k,5] <- as.numeric(present_of_RemoveCut)
    MyDataSet[k,6] <- as.numeric(present_of_Paste)
    MyDataSet[k,7] <- as.numeric(present_of_Replace)
    MyDataSet[k,8] <- as.numeric(present_of_MoveFromTo)
    MyDataSet[k,9] <- as.numeric(tuning_of_action_time)
    MyDataSet[k,10] <- as.numeric(mean(tuning_time_space)) # tuning_time_space
    MyDataSet[k, 11] <- as.numeric(sum(fluquence_of_change)/length(fluquence_of_change)) #`fluquence_of_change
    MyDataSet[k, 12] <- as.numeric(word_time_ratio) #`word_time_ratio
    MyDataSet[k, 13] <- as.numeric(word_event_ratio) #`word_event_ratio
    MyDataSet[k, 14] <- as.numeric(event_time_ratio) # tuning event time
    MyDataSet[k, 15] <- as.numeric(idle_time_ratio) # tuning idle time
    MyDataSet[k, 16] <- as.numeric(mean(sentence_length)) # tuning sentence length
    MyDataSet[k, 17] <- as.numeric(num_of_sentence) # number of sentences
    MyDataSet[k, 18] <- as.numeric(mean(word_length)) # tuning word length
    MyDataSet[k, 19] <- as.numeric(num_of_words) # number of words
    MyDataSet[k, 20] <- as.numeric(num_of_paragraphs) # number of paragraphs


    if(k == 1) {
        # first user's data
        first_urser_data <- user_logs
    }
}

print(colnames(MyDataSet))

if(model != 2){
    save(MyDataSet, file = paste(cur_dir2, "/MyData.RData", sep = ""))
    save(users, file = paste(cur_dir2, "/users.RData", sep = ""))
}

