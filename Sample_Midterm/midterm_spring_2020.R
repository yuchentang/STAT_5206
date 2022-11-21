# Context:
# Twitter is a internet platform where people can
# broadcast short messages (<280 characters) to their
# followers. It is popular among celebrities, politicians,
# and journalists because of its reach and ease of use.
# Twitter also allows many of its recent data to be consumed
# freely by the public.
#
# The data from this problem are from querying tweets made
# by the previous, current, and future president of the US.
library(jsonlite)
twitter <- read_json("5206_twitter.json")


# Q0. Please show some of the code you used to explore the data structure.
# This exploration should show that you understand the length of the
# data, some of the fields, and some of the sub-fields within each record.
# The key is to see that you know how to explore hierarchical datasets.
class(twitter)
length(twitter)
class(twitter[[1]])
names(twitter[[1]])
twitter[[1]]


# Q1. What types of "entities" are in the data? For example,
# these could be "mentions", "annotations", etc.
entities <- unique(unlist(map(twitter, ~ names(.x$entities))))

# Q2. Please create a data frame where each row
# corresponds to a different tweet and each column is
# a different variable. Please pay attention to the data
# type for each variable and make sure your choice is reasonable.
# Lots of partial credit is expected
# for this problem, do not get stuck here.
# The variables should capture:
# - The "author_id"
# - The text of the tweet
# - Please compute the number of words in each tweet using parse_tweet() below
# - Please compute the number of unique words in each tweet using parse_tweet() below
# - The total number entities in the tweet, e.g. there are 2 annotations, 1 mention, but no
#   other entities for the tweet with ID = 1324931722165821440 so its total
#   would be 3. Hint: make sure you can handle cases like the tweet with ID = 1324007806694023169.

parse_tweet <- function(tweet){
  # Takes in a character vector, then returns
  # a list with number of elements matching the length
  # of the character vector, each element will be a vector of
  # words. E.g. if you pass in a character vector
  # of length 1, it will return a list of length 1.
  non_word <- "[^A-z0-9_]"
  words <- strsplit(tweet, non_word)
  non_empty_words <- lapply(words, function(x) x[nchar(x) > 0])
  lowered_words <- lapply(non_empty_words, tolower)
  return(lowered_words)
}





# Q3. If you couldn't solve Q2, please use the file "/course/data/Q2_midterm_data_frame.csv",
# this dataset is not fully correct but will allow you to move forward.
# Please calculate:
# 1) the total number of tweets
# 2) the average ratio between the unique words vs total words across the tweets  
#    for each author.
# Please make sure your answer has reasonable names for any columns
# or rows. You should assume the reader only sees the output and not the code.




# Q4. For all authors with more than 20 tweets. Please create a visualization 
# that describes their twitter behavior that can also help us decide
# which author to follow. Please explain why the visualization may
# help us decide who we should follow. Your visualization must identify 
# some difference between the authors and then you should make an argument
# why this "may" help us decide who to follow. This argument only needs to be
# reasonable but does not have to be extremely convincing.
# You can modify the data you have if the features you want are
# not available so far.
