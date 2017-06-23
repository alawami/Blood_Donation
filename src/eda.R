
### load packages ----------------------------------------------------------------------------------------
purrr::walk(c("stringr", "caret", "magrittr", "tidyverse"), library, character.only = TRUE)


### read in data -----------------------------------------------------------------------------------------

train <- read_csv("data/train.csv")
test <- read_csv("data/test.csv")

submissionFormat <- read_csv("~/Documents/Driven_Data/Blood_Donation/predictions/BloodDonationSubmissionFormat.csv")
names(submissionFormat)[1] <- ""

names(train) <- c("id", str_replace_all(names(train)[-1], " ", "_"))
names(test) <- names(train)[1:dim(train)[2]-1]

names(train)[4] <- "Total_Volume_Donated"
names(test)[4] <- "Total_Volume_Donated"

glimpse(train)

# validation set
set.seed(1234)
validationIndex <- sample(nrow(train), round(nrow(train) * 0.10))
validate <- train[validationIndex, ]
train <- train[-validationIndex, ]

### Format data

train %<>% mutate(Made_Donation_in_March_2007 = factor(Made_Donation_in_March_2007, 
                                                       levels = c(1, 0), labels = c("Donated", "Did_not_donate")))
validate %<>% mutate(Made_Donation_in_March_2007 = factor(Made_Donation_in_March_2007, 
                                                          levels = c(1, 0), labels = c("Donated", "Did_not_donate")))

### Data summary
source("src/features.R")
set.seed(123)
dataSample <- train %>% sample_frac(0.6)

dataSample %>% select(Made_Donation_in_March_2007) %>% table %>% prop.table()

summary(train)

ggplot(dataSample, aes(Number_of_Donations, Made_Donation_in_March_2007, col = Made_Donation_in_March_2007)) +
  geom_point(size = 0.5, position = position_jitter())

ggplot(dataSample, aes(Months_since_First_Donation, Made_Donation_in_March_2007, col = Made_Donation_in_March_2007)) +
  geom_point(size = 0.5, position = position_jitter())

ggplot(dataSample, aes(Months_since_Last_Donation, Made_Donation_in_March_2007, col = Made_Donation_in_March_2007)) +
  geom_point(size = 0.5, position = position_jitter())

ggplot(dataSample, aes(frequency_of_donation, Made_Donation_in_March_2007, col = Made_Donation_in_March_2007)) +
  geom_point(size = 0.5, position = position_jitter())
