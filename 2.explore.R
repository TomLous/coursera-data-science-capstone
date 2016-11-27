source("utils.R")

twitterEN <- sampleFile("data/final/en_US/en_US.twitter.txt")
newsEN <- sampleFile("data/final/en_US/en_US.news.txt")
blogsEN <- sampleFile("data/final/en_US/en_US.blogs.txt")

twitterENTokenized <- tokenize(twitterEN$sample.data)
twitterENTokenized <- profanityFilter(twitterENTokenized, twitterEN$locale)
#twitterENTokenized <- stopwordFilter(twitterENTokenized, twitterEN$locale)
#twitterENTokenized <- shortTermFilter(twitterENTokenized, 2)

twitterENBiGrams <- transformNGram(twitterENTokenized, 2)
twitterENTriGrams <- transformNGram(twitterENTokenized, 3)

twitterENTermFrequency <- frequencyTable(twitterENTokenized)
twitterENBiGramsFrequency <- frequencyTable(twitterENBiGrams)
twitterENTriGramsFrequency <- frequencyTable(twitterENTriGrams)

