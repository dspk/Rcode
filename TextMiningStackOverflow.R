#. Text Mining unstructured data from Stackoverflow posts and predicting tags 
#. Author: @dspk


#. A function to create three lists (1) for how many times tags occur, (2) for how many times words occur, 
#. and (3) a list of lists for pairwise Tagword-combos
#. Input to the function is a list of tags and a list of words.
Tagword.Construct = function(tagslist, wordslist){
  tags.list = vector("list")
  words.list = vector("list")
  tagwords.list = vector("list")
  for(i in 1:length(tagslist)){
    for(j in 1:length(tagslist[[i]])){ 
      if(length(which(names(tags.list) == tagslist[[i]][j])) != 0){  #. is this word tagslist[[i]][j] contained in the dictionary tags.list?
        tags.list[[tagslist[[i]][j]]] = tags.list[[tagslist[[i]][j]]] + 1  #. if it is in the dict add 1; 
      }
      else{
        tags.list[[tagslist[[i]][j]]] = 1 #. if not, add it and initialize to 1
      } 
      #. Create a tagword list of lists which contains the number of times tag and word combinations occur
      for(l in 1:length(wordslist[[i]])){ # all the words in the ith question
        if(length(which(names(tagwords.list) == tagslist[[i]][j])) != 0) {  #. if the tag is contained in the tagwords list
          if(length(which(names(tagwords.list[[tagslist[[i]][j]]]) == wordslist[[i]][l])) != 0) {  #. if the word is equal to the tag
            tagwords.list[[tagslist[[i]][j]]][[wordslist[[i]][l]]]  = tagwords.list[[tagslist[[i]][j]]][[wordslist[[i]][l]]] + 1  #. increment the element corresponding to tag tagslist[[i]][j] and word wordslist[[i]][l] 
          }
          else{
            tagwords.list[[tagslist[[i]][j]]][[wordslist[[i]][l]]]  = 1
          }
        }
        else{ #. if the tag is not contained in the tagwords list
          tagwords.list[[tagslist[[i]][j]]] = vector("list") #. create a tagwords list
          tagwords.list[[tagslist[[i]][j]]][[wordslist[[i]][l]]]  = 1 
        }      
      }
    }
    #. Create a dictionary for the words words.list which contains the number of times each words appears
    for(l in 1:length(wordslist[[i]])){ 
      if(length(which(names(words.list) == wordslist[[i]][l])) != 0) {
        words.list[[wordslist[[i]][l]]] = words.list[[wordslist[[i]][l]]] + 1    
      }
      else{
        words.list[[wordslist[[i]][l]]] = 1   
      } 
    }
  }
  return(list(tags.list=tags.list, words.list=words.list, tagwords.list=tagwords.list))
}


#. A function for computing Support - fraction of questions with both tag A and word B pairwise support
#. Input to this function is a list of tags, a list of words and the tag-words dictionary that contains the number 
#. of occurences of a particular tag-word combination.
Support = function(tagslist, wordslist, tagwordslist){
  support.list = vector("list")
  for(i in 1:length(wordslist)){
    for(j in 1:length(tagslist)){
      if(length(which(names(tagwordslist) == names(tagslist)[[j]])) != 0){  #. does the tagwordlist contain a particular tag
        if(length(which(names(tagwordslist[[names(tagslist)[[j]]]]) == names(wordslist)[[i]])) !=0){ #. is the word the same as the tag
          if(length(which(names(support.list) == names(wordslist)[[i]])) == 0){ #. if the word is not in support.list
            support.list[[names(wordslist)[[i]]]] = vector("list") #. add the word to the support list
          }
          support.list[[names(wordslist)[[i]]]][[names(tagslist)[[j]]]]  = tagwordslist[[names(tagslist)[[j]]]][[names(wordslist)[[i]]]]/length(tagslist)
        }
      }
    }
  }
  return(support.list)
}


#. A function for computing confidence  
#. P(A intersection B) = Support{tagA, wordB}/Support{B} 
#. = (fraction of questions with both A and B)/ (fraction of questions that contain B)
#. Input to this function is a list of tags, a list of words and the tag-words dictionary that contains the number 
#. of occurences of a particular tag-word combination.
Confidence = function(tagslist, wordslist, tagwordslist){
  confidence.result = vector("list")
  for(i in 1:length(wordslist)){
    for(j in 1:length(tagslist)){
      if(length(which(names(tagwordslist) == names(tagslist)[[j]])) != 0){
        if(length(which(names(tagwordslist[[names(tagslist)[[j]]]]) == names(wordslist)[[i]])) !=0){
          if(length(which(names(confidence.result) == names(wordslist)[[i]])) == 0){
            confidence.result[[names(wordslist)[[i]]]] = vector("list")
          }
          confidence.result[[names(wordslist)[[i]]]][[names(tagslist)[[j]]]]  = tagwordslist[[names(tagslist)[[j]]]][[names(wordslist)[[i]]]]/wordslist[[i]]
        }
      }
    }
  }
  return(confidence.result)
}


#. An algorithm for predicting tags, given a new question
#. Input to this function is a new question and the computed
#. confidence values for tag word combinations
TagPrediction = function(new.question, confidence.input){
  #clean "character" word file and convert to list
  library(tm)
  new.question.corpus = Corpus(VectorSource(new.question))
  new.question.corpus = tm_map(new.question.corpus, removeNumbers)  #. remove numbers
  new.question.corpus = tm_map(new.question.corpus, tolower) #. convert to lower case
  new.question.corpus = tm_map(new.question.corpus, removePunctuation) #. remove punctuation
  mystopwords = as.character(c(stopwords("english"), single.prep$prepositionsclean))
  new.question.corpus = tm_map(new.question.corpus, removeWords, mystopwords) #. remove stopwords
  new.question.corpus = tm_map(new.question.corpus, stripWhitespace) 
  
  new.question.word =  strsplit(unlist(new.question.corpus), " " ) #. convert corpus to list
  tag.assigned = vector("list")  #. create empty vector to contain tags which will be assigned
  for(i in 1:length(new.question.word)){
    for(j in 1:length(new.question.word[[i]])){
      if(new.question.word[[i]][j] != ""){
        if(length(names(confidence.input[[new.question.word[[i]][j]]][names(which(confidence.input[[new.question.word[[i]][j]]] > 0.5))])) != 0)
        {
          tag.temp = names(confidence.input[[new.question.word[[i]][j]]][names(which(confidence.input[[new.question.word[[i]][j]]] > 0.5))]) #. do the tag assignment
          tag.assigned = c(tag.assigned,tag.temp)
        }
      }      
    }
  }
  return(tag.assigned)
} 


#. Complete code that utilizes above function to test out 
#. prediction of tags for Stackoverflow post data


#. First process the data file (Stackoverflow posts)
#. We are working with Tags and Titles; not the Body
#. Note that users will have to set the correct paths and 
#. location of data files.
getwd()
setwd("SET CORRECT WORKING DIRECTORY")


#. Read in full data of all posts
posts = read.csv(".\\posts.csv", stringsAsFactors=FALSE)  #. contains text(body), title, tags and additional attributes
posts = posts[c(1, 5, 16, 19)]  #. create new data with only the relevant attributes text(body), title, tags and Id
posts = posts[!(posts$Title == "" | posts$Tags == ""), ] #. remove missing data from posts

#. Read in tags data
tags = read.csv(".\\tags.csv", stringsAsFactors=FALSE)  #. contains Id and tags

#. Create Titles data frame from posts 
Titles = posts[, c(2, 4)]

#. Processing tags
tags = tags[!(tags$Tags == ""), ]  #. remove missing
#. Separate/split the tags for each question
tag.split = gsub("><", " ", tags$Tags)
tag.split = gsub("<", "", tag.split)
tag.split = gsub(">", "", tag.split)
tag.split = strsplit(tag.split, " ")  #. list containing all words within each tag for all tags

#. Import and Process prepositions(from wikipedia)
single.prep = read.csv("SET CORRECT WORKING DIRECTORY")
names(single.prep) = gsub("...", "", names(single.prep) ) #. change column name to prepositions
names(single.prep) = gsub(".", "prepositions", names(single.prep) )
single.prep$prepositions = as.character(single.prep$prepositions)
class(single.prep$prepositions)
single.prep$prepositions = gsub("\\*''", "", single.prep$prepositions)  #. remove pattern beginning with(\\) *
single.prep$prepositions = gsub("\\[", "", single.prep$prepositions)  
single.prep$prepositions = gsub("\\]]''", "", single.prep$prepositions)
single.prep$prepositions = gsub("[[:punct:]]", " ", single.prep$prepositions) 

single.prep$prepositionsclean = vector("list", length(single.prep$prepositions))  #. create variable of interest
for(i in 1:length(single.prep$prepositions)){  #. loop to pick the last word in every string
  single.prep$prepositionsclean[[i]] = tail(strsplit(single.prep$prepositions, split=" ")[[i]],1) 
}
print(single.prep$prepositionsclean) 

#. Processing titles
library(tm)
Titles.corpus = Corpus(VectorSource(Titles$Title)) #. build a corpus and specify the source to be character vectors
Titles.corpus = tm_map(Titles.corpus, removeNumbers)  #. remove numbers 
Titles.corpus = tm_map(Titles.corpus, tolower) #. convert to lower case
Titles.corpus = tm_map(Titles.corpus, removePunctuation) #. remove punctuation 
compile.stopwords = as.character(c(stopwords("english"), single.prep$prepositionsclean))
Titles.corpus = tm_map(Titles.corpus, removeWords, compile.stopwords) #. remove stopwords
Titles.corpus = tm_map(Titles.corpus, stripWhitespace) #. remove extra whitespace
x = strsplit(unlist(Titles.corpus), " " ) #. create list of all the words within each title for all titles

#. Create the tagwords dictionary
result = Tagword.Construct(tagslist = tag.split, wordslist = x)

#. Compute the confidence for pairs of tags and words
confidence.list = Confidence(tagslist = result$tags.list, wordslist= result$words.list, tagwordslist = result$tagwords.list)

#. Compute the support for pairs of tags and words
support.list = Support(tagslist = result$tags.list, wordslist= result$words.list, tagwordslist = result$tagwords.list)

#. Print support and confidence examples
support.list[["visa"]][["europe"]]
confidence.list[["visa"]][["europe"]]

support.list[["visa"]][["schengen"]]
confidence.list[["visa"]][["schengen"]]

support.list[["passport"]][["visas"]]
confidence.list[["passport"]][["visas"]]

support.list[["wildlife"]][["australia"]]
confidence.list[["wildlife"]][["australia"]]

#. Call the tag prediction function
example.question = "what is the weather like in London in februrary?"
result.tags = TagPrediction(new.question = example.question, confidence.input = confidence.list )
print(result.tags)
