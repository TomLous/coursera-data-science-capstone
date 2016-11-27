source("utils.R")

#load Data
twitterEN <- sampleFile("data/final/en_US/en_US.twitter.txt")
newsEN <- sampleFile("data/final/en_US/en_US.news.txt")
blogsEN <- sampleFile("data/final/en_US/en_US.blogs.txt")


#lines
twitterEN$org.lines.count


# max length
twitterEN$org.lines.summary
newsEN$org.lines.summary
blogsEN$org.lines.summary


# love vs hate
loveLines <- twitterEN$sample.data[grepl("love",twitterEN$sample.data, ignore.case = FALSE)]
hateLines <- twitterEN$sample.data[grepl("hate",twitterEN$sample.data, ignore.case = FALSE)]

loveVhate <- length(loveLines) / length(hateLines)


# grep specific line from original source
system("grep biostat data/final/en_US/en_US.twitter.txt")

# grep 
system("grep \"A computer once beat me at chess, but it was no match for me at kickboxing\" data/final/en_US/en_US.twitter.txt")
