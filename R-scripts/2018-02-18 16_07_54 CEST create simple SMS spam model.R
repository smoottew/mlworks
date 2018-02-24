# Created 2018-02-18 16_07_54 CEST
#

# install.packages(c("data.table", "purrr", ...))

packageVersion("data.table"  ) # [1] ‘1.10.4.3’
packageVersion("purrr"       ) # [1] ‘0.2.4’

packageVersion("chR"         ) # [1] ‘0.1.0’
packageVersion("koR"         ) # [1] ‘0.1’

devtools::install_github("kongra/chR")
devtools::install_github("kongra/koR")

packageVersion("readr"       ) # [1] ‘1.1.1’
packageVersion("stringr"     ) # [1] ‘1.2.0’

packageVersion("ggplot2"     ) # [1] ‘2.2.1’
packageVersion("doSNOW"      ) # [1] ‘1.0.16’

packageVersion("quanteda"    ) # [1] ‘1.0.0’
packageVersion("caret"       ) # [1] ‘6.0.78’
packageVersion("pROC"        ) # [1] ‘1.10.0’

packageVersion("e1071"       ) # [1] ‘1.6.8’
packageVersion("irlba"       ) # [1] ‘2.3.2’
packageVersion("randomForest") # [1] ‘4.6.12’

# LET'S READ RAW SMS SPAM DATA
#
library(data.table)
library(purrr)
library(chR)
library(koR)

smsSpam <- fread("data-origin/2018-02-18 16_07_54 CEST sms spam/sms spam.csv")
# Error in fread("data-origin/2018-02-18 16_07_54 CEST sms spam/sms spam.csv") :
#   embedded nul in string: '\0\n\0h\0a\0m\0'

smsSpam <- read.csv("data-origin/2018-02-18 16_07_54 CEST sms spam/sms spam.csv",
                    fileEncoding     = "UTF-16LE",
                    stringsAsFactors = FALSE) %>% as.data.table

str (smsSpam)
View(smsSpam)

# DATA PRE-PROCESSING
#
summary(smsSpam[, X])
smsSpam[, X] %>% unique %>% sort

library(readr)
smsSpam[, X := parse_character(X)]

str (smsSpam)
View(smsSpam)

smsSpam[, X.1 := parse_character(X.1)]
smsSpam[, X.2 := parse_character(X.2)]

str (smsSpam)
View(smsSpam)

smsSpam[, X   := NULL]
smsSpam[, X.1 := NULL]
smsSpam[, X.2 := NULL]

setnames(smsSpam, c("v1", "v2"), c("Label", "Text"))
smsSpam %>% moveDTcols("Label", "after", "Text")

str (smsSpam)
View(smsSpam)

smsSpam[, Text  := parse_character(Text )]
smsSpam[, Label := parse_character(Label)]

str (smsSpam)
View(smsSpam)

smsSpam %>% nrow
# [1] 5572

smsSpam[is.na(Text) | is.na(Label)]
# Empty data.table (0 rows) of 2 cols: Text,Label

smsSpam[, Label := as.factor(Label)]

str (smsSpam)
View(smsSpam)

# A LOOK AT THE DISTRO
#
smsSpam[, Label] %>% table
#  ham spam
# 4825  747

smsSpam[, Label] %>% table %>% prop.table
#       ham      spam
# 0.8659368 0.1340632

library(stringr)
smsSpam[, TextLen := str_length(Text)]
smsSpam %>% moveDTcols("TextLen", "after", "Text")
smsSpam[, TextLen] %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 2.00   35.00   61.00   80.08  121.00  910.00

View(smsSpam)

library(ggplot2)
ggplot(data = smsSpam, aes(x = TextLen, fill = Label)) +
  geom_histogram(binwidth = 5) +
  labs(y = "Text Count", x = "Length of Text", title = "Distribution of Text Lengths with Class Labels")

# CREATE DATA PARTITIONS VIA STRATIFIED RANDOM SAMPLING
#
library(caret)
set.seed(12345)

indexes <- createDataPartition(
  smsSpam[, Label],
  times = 1, p = 0.7, list = FALSE)

trainData <- smsSpam[ indexes] %>% chDT
testData  <- smsSpam[-indexes] %>% chDT

trainData %>% nrow
testData  %>% nrow

testData  %>% nrow / trainData %>% nrow / (3 / 7)

trainData[, Label] %>% table %>% prop.table
testData [, Label] %>% table %>% prop.table

# TEXT ANALYTICS AND TOKENIZATION
#
View(trainData[str_detect(Text, "&")])
View(trainData[str_detect(Text, "http:")])

str2words <- function(s) chList({
  chStrings(s)
  quanteda::tokens(s, what = "word",
                   remove_numbers = TRUE,
                   remove_punct   = TRUE,
                   remove_symbols = TRUE,
                   remove_hyphens = TRUE)
})

str2words("This is it!!!")
typeof(str2words("This is it!!!"))
class (str2words("This is it!!!"))

trainTokens1 <- str2words(trainData[, Text])
View(trainTokens1)

trainTokens2 <- trainTokens1 %>% quanteda::tokens_tolower()
View(trainTokens2)

trainTokens3 <- trainTokens2 %>% quanteda::tokens_select(x = .,
  pattern   = quanteda::stopwords(),
  selection = "remove")
View(trainTokens3)

trainTokens4 <- trainTokens3 %>% quanteda::tokens_wordstem(x = ., language = "english")
View(trainTokens4)

trainTokens <- trainTokens4

# DFM - DOCUMENT(S) FREQUENCY MODEL
#
trainDFM <- trainTokens %>% quanteda::dfm(tolower = FALSE)

dim(trainDFM)
# [1] 3901 5665

osize(trainDFM)
# 1.03 MB

memuse()

trainDFMatrix <- as.matrix(trainDFM)
colnames(trainDFMatrix)[1:50]

str(trainDFM)

trainDT <- as.data.table(trainDFM)
str(trainDT)

View(colnames(trainDT))

trainDT %>% colnames %>% make.names
setnames(trainDT, trainDT %>% colnames, trainDT %>% colnames %>% make.names)

View(colnames(trainDT))

colNames <- colnames(trainDT)
colNames[duplicated(colNames)]
# [1] "s.i.m"

which(colNames == "s.i.m")
# [1] 1412 5028

which(trainDT[, 1412] != trainDT[, 5028])
# [1]  369 3058 3510 3864
# Conclusion: we can safely remove the 2nd duplicated column
set(trainDT, j = 5028L, value = NULL)

model1Props <- colnames(trainDT)

# ADD Label TO trainDT
trainDT[, Label := trainData[, Label]]
trainDT %>% moveDTcols("Label", "first")

# TRAINING PROCESS
#
set.seed(12345)
cvFolds <- createMultiFolds(trainData[, Label], k = 10, times = 3)
cvCntrl <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3,
                        index = cvFolds)

library(doSNOW)

catimela({
  cl <- makeCluster(3, type = "SOCK")
  registerDoSNOW(cl)

  model1 <- train(Label ~ ., data = trainDT, method = "rpart", trControl = cvCntrl, tuneLength = 7)

  stopCluster(cl)

}, msg = "model1 training")


model1
osize(model1)
# 1.32 GB

# SAVE AND (RE)LOAD
#
catimela({
  model1      %>% saveRDS(file = "data/sms spam/2018-02-18 16_07_54 CEST sms spam model1.rds")
  model1Props %>% saveRDS(file = "data/sms spam/2018-02-18 16_07_54 CEST sms spam model1 - props.rds")

  # save(model1, trainDFMProps1, file = "data/sms spam/2018-02-18 16_07_54 CEST model1.Rdata")
}, msg = "model1 saving")

memuse()
# rm(list = ls())
# memuse()

# load("data/sms spam/2018-02-18 16_07_54 CEST model1.Rdata")
# memuse()
# osize(model1)

model1 <- readRDS(file = "data/sms spam/2018-02-18 16_07_54 CEST sms spam model1.rds")
# model1Props <- readRDS(file = "data/sms spam/2018-02-18 16_07_54 CEST sms spam model1 - props.rds")
# osize(model1)

source("R/sms spam/2018-02-19 11_49_02 CEST core.R")

# TEST PREDICTIONS
#
model1Props
model1

View(testData)

testDT <- testData[, Text] %>% str2dfm(props = model1Props)
osize(testDT)

setdiff(model1Props,   colnames(testDT))
all    (model1Props == colnames(testDT))
# OK

preds1 <- predict(model1, newdata = testDT)

oks1 <- sum(testData[, Label] == preds1)
oks1 / nrow(testData)
# [1] 0.9443447

# CREATE A SIMPLIFIED MODEL ON TRAIN DATA
#
model2 <- rpart::rpart(Label ~ ., data = trainDT, cp = 0.01210962)
preds2 <- predict(model2, newdata = testDT) %>% as.data.table
View(preds2)
preds2[, Label := ifelse(ham > spam, "ham", "spam")]

oks2 <- sum(testData[, Label] == preds2[, Label])
oks2 / nrow(testData)

preds2  [, Label] %>% table
testData[, Label] %>% table

preds2  [, Label] %>% table %>% prop.table
testData[, Label] %>% table %>% prop.table

testData[Label != preds2[, Label]] %>% View

# CREATE THE FINAL MODEL USING ALL DATA (TRAIN + TEST)
#
smsDT <- str2dfm(smsSpam[, Text])
smsDT[, Label := smsSpam[, Label]]
model3 <- rpart::rpart(Label ~ ., data = smsDT, cp = 0.01210962)

colNames <- colnames(smsDT)
colNames[duplicated(colNames)]
# [1] "s.i.m"

which(colNames == "s.i.m")
# [1] 1773 6122

which(smsDT[, 1773] != smsDT[, 6122])
# [1]  531 4387 5027 5527
# Conclusion: we can safely remove the 2nd duplicated column
set(smsDT, j = 6122L, value = NULL)

model3 <- rpart::rpart(Label ~ ., data = smsDT, cp = 0.01210962)
osize(model3)
# 193 MB

preds3 <- predict(model3, newdata = smsDT) %>% as.data.table
View(preds3)
preds3[, Label := ifelse(ham > spam, "ham", "spam")]

oks3 <- sum(smsSpam[, Label] == preds3[, Label])
oks3 / nrow(smsSpam)

preds3 [, Label] %>% table
smsSpam[, Label] %>% table

preds3 [, Label] %>% table %>% prop.table
smsSpam[, Label] %>% table %>% prop.table

model3 %>%
  saveRDS(file = "cloud data/sms spam/2018-02-18 16_07_54 CEST model3.rds")
smsDT %>% colnames %>% setdiff("Label") %>%
  saveRDS(file = "cloud data/sms spam/2018-02-18 16_07_54 CEST props3.rds")

# smsSpam[preds3[, Label] == "spam", Text] %>% smsHamSpamDT(model, props) %>% View
