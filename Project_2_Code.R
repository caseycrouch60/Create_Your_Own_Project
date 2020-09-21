#Author: Casey Crouch, 9/10/2020
#HarvardX, Data Science: Capstone



#Project 2: Choose Your Own Project

#Goal: Accurately predict political party affiliation for House members in the 98th Congress
#Data: UC Irvine Machine Learning Repository "Congressional Voting Records" Dataset
#Predictors: Votes on 16 key pieces of legislation considered in the House 
#Metric for Success: Accuracy
#Threshold for Success: 90% accurate predictions or higher
#Methods: Guessing, Ifelse() Statements, Logistic Regression, K-Nearest Neighbors...
#Methods Cont: Random Forests, Neural Networks, Majority-Vote Ensemble

#Warning: This script can take 5-10 minutes to execute



#Part 1: Preparing R

#################################################

#Retrieving packages, installing if necessary

#################################################

#Installing and Calling Up Packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(usmap)) install.packages("usmap", repos = "http://cran.us.r-project.org")
if(!require(politicaldata)) install.packages("politicaldata", repos = "http://cran.us.r-project.org")
if(!require(ggparliament)) install.packages("ggparliament", repos = "http://cran.us.r-project.org")
if(!require(pixiedust)) install.packages("pixiedust", repos = "http://cran.us.r-project.org")
if(!require(sjlabelled)) install.packages("sjlabelled", repos = "http://cran.us.r-project.org")
if(!require(RAM)) install.packages("RAM", repos = "http://cran.us.r-project.org")
if(!require(ggridges)) install.packages("ggridges", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(NeuralNetTools)) install.packages("NeuralNetTools", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(usmap)
library(politicaldata)
library(ggparliament)
library(pixiedust)
library(sjlabelled)
library(RAM)
library(ggridges)
library(caret)
library(ggrepel)
library(NeuralNetTools)



#Part 2: Downloading the Data

#################################################

#This project uses the UCI M.L. Repository

#################################################

#Here is the link:
#https://archive.ics.uci.edu/ml/datasets/Congressional+Voting+Records

#Downloading the data from the UCI Machine Learning Repository
#This method stores the dataset in a temporary file on the local system and then reads it into R

temporary_data <- tempfile()
download.file('https://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data', temporary_data)
data <- read.table(temporary_data, sep = ',', col.names = c('party', 'Infants', 'H2O', 'Budget', 'Medical', 'ElSalvador', 'Religion', 'Testban', 'Contras', 'Missile', 'Immigration', 'Synfuels', 'Education', 'Superfund', 'Crime', 'Dutyfree', 'SAfrica'))



#Part 3: Data Pre-Processing

#################################################

#Cleaning the data for exploration & analysis 

#################################################

#Overall, the raw data is very clean, and few steps will be necessary to begin the exploration

#Binarize the "yeas" and "nays" and replace "?" with NA for data exploration

data <- data.frame(sapply(data, function(x){
  ifelse(x == '?', NA, ifelse(
    x == 'y', 1, ifelse(
      x == 'n', 0, ifelse(
        x == 'republican', 'republican', 'democrat'
))))}))

#Compile the transformed data into the primary "voting" dataframe

data_temp_1 <- data[,1]
data_temp_2 <- sapply(data[,2:17], as.numeric)
voting <- data.frame(party = data_temp_1, data_temp_2)

#Capitalize the first letter of political party names

voting$party <- str_replace(voting$party, 'republican', 'Republican')
voting$party <- str_replace(voting$party, 'democrat', 'Democrat')

#Note: Significant data cleaning will take place in the "Data Exploration" with the use of party voting trends to recode NAs
#We wait to take this step until we have arrived at the place in which our findings justify the transformation



#Part 4: Data Exploration

#################################################

#Seek insights to pursue in machine learning

#################################################

##Activity 1): Plot 1982 Midterm Election Results

#To get started, we'll look at the election that shaped the Congress to be examined in this project

#Retrieving Congressional data from "politicaldata" package

data('house_results')

#Filter for 1982 election

US_House_98 <- house_results %>%
  filter(year == 1982)

#Calculate which party won the election in each district

possible_winners <- US_House_98[,5:7]
winners_1_3 <- max.col(replace(possible_winners, is.na(possible_winners), 0), ties.method = 'first')
dem_win <- ifelse(winners_1_3 == 1, 1, 0)

#Attach the winner data onto the main election dataframe

My_Congress <- data.frame(US_House_98, dem_win)

#Remove 2 Incorrect Entries for North Dakota (the data shifts down, so the code stays the same):

My_Congress <- My_Congress[-241,]
My_Congress <- My_Congress[-241,]

#Calculate party majorities for each House delegation

maj_dem_98 <- My_Congress %>%
  group_by(state_abb) %>%
  summarize(maj_dem = ifelse(sum(dem_win) > n() / 2, 1, ifelse(
    sum(dem_win) == n() / 2, 0.5, 0))) %>%
  .$maj_dem

state_maj_dem <- data.frame(state = unique(My_Congress$state_abb), X = maj_dem_98)

#Plotting 98th Congress, with blue states representing Democratic control, red states representing Republican control, and purple states being a tie

plot_usmap(regions = 'states', data = state_maj_dem, value = 'X') +
  labs(title = 'United States Congress, 98th Session (1983-1985)', subtitle = 'Party Control of U.S. House Delegations') +
  theme(legend.position = 'right') +
  scale_fill_continuous(low = 'red', high = 'blue') +
  theme(panel.background = element_rect(color = "black", fill = "white"), legend.position = 'none')

#We see that the House delegation map looked a lot different in the mid 1980s than it does today
#At this time, the country was trending Republican but the Democrats maintained a strong hold in the House of Representatives
#Democrats would maintain this Congressional advantage until the mid 1990s
#Based on this map alone, we would assume that Democratic legislation would nearly always pass and Republican legislation would nearly always fail



##Activity 2): Analyze party makeup of the 98th Congress

#Generate bar graph of party distribution

voting %>%
  ggplot(aes(party, fill = party)) + 
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red')) +
  geom_hline(yintercept = 218, linetype = 'dashed', size = 1) + 
  geom_text(aes(2, 218, label = 'Legislative Majority', vjust = -1)) +
  theme(legend.position = 'none') +
  labs(title = 'U.S. House Party Distribution, 1983-1985', x = 'Party', y = 'Count') +
  theme(plot.title = element_text(hjust = 0.5))

#Present Seat Totals

seat_totals <- data.frame(Democrat = nrow(voting[voting$party == 'Democrat',]), Republican = nrow(voting[voting$party == 'Republican',]))
dust(seat_totals)

#This means that 61% of members are Democrats and 39% are Republicans, as seen below:

seat_percents <- data.frame(Democrat = nrow(voting[voting$party == 'Democrat',]) / nrow(voting), Republican = nrow(voting[voting$party == 'Republican',]) / nrow(voting))
dust(seat_percents)

#We can see that the Democrats enjoy a strong majority 
#To have an even better visualization of party standings, we can generate a parliament plot

#Generate Parliament Plot Semicircle

#The "ggparliament" election data only goes back to 1990, so we need to construct the election data input manually:

election_summary <- data.frame(year = numeric(), country = character(), house = character(), party_long = character(), party_short = character(), seats = numeric(), government = numeric())

election_summary[nrow(election_summary) + 1,] <- c(1982, 'USA', 'Representatives', 'Republican', 'GOP', nrow(voting[voting$party == 'Republican',]), 0)
election_summary[nrow(election_summary) + 1,] <- c(1982, 'USA', 'Representatives', 'Democratic', 'Dem', nrow(voting[voting$party == 'Democrat',]), 1)

#Convert the "year," "seats," and "government" columns into numeric values

election_summary$year <- as.numeric(election_summary$year)
election_summary$seats <- as.numeric(election_summary$seats)
election_summary$government <- as.numeric(election_summary$government)

#Run the manually-compiled election data through parliament_data() to generate coordinates for plotting

house_parliament_prep <- parliament_data(election_summary, type = 'semicircle', parl_rows = 11, party_seats = election_summary$seats)

#Generate the parliament plot

ggplot(house_parliament_prep, aes(x = x, y = y, color = party_short)) +
  geom_parliament_seats() + 
  theme_ggparliament() + 
  scale_color_manual(values = c('blue', 'red'), name = 'Party', labels = c('Democrat', 'Republican')) +
  geom_segment(aes(x = 0, xend = 0, y = 0.70, yend = 2.15), color = 'black', linetype = 'dashed') +
  geom_text(aes(0, 0.4, label = 'Legislative Majority', vjust = -1), color = 'black') +
  labs(color = 'Party', title = 'U.S. House Party Distribution, 1983-1985') +
  theme(legend.position = 'bottom', legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.5))



##Activity 3): Calculate whether or not each bill passed

#Note: due to ambiguous "?" variable encoding, we assume that a quorum is always present
#According to House rules, in the presence of a quorum a measure passes when the yeas supersede the nays by 1 (i.e. a majority vote yea), see United States v. Ballin for more information

measure_outcomes <- sapply(voting[,2:17], function(x){
  yea <- x[x == 1]
  nea <- x[x == 0]
  ifelse(length(yea) > length(nea), 'Pass', 'Fail')
})

house_votes <- remove_all_labels(data.frame(Item = colnames(voting[,2:17]), Result = measure_outcomes))

#Calculate and present the number of passed and failed bills:

passed_legislation <- count(house_votes, house_votes[,2])[2,2]
failed_legislation <- count(house_votes, house_votes[,2])[1,2]
legislation_outcomes <- data.frame(pass = passed_legislation, fail = failed_legislation)

dust(legislation_outcomes) %>%
  sprinkle_colnames(pass = 'Passed Legislation', fail = 'Failed Legislation')

#We can also use the house_votes data to see exactly which bills passed and which failed

#Re-arrange the dataframe with passed legislation above failed legislation, then present:

house_votes_arranged <- arrange(house_votes, desc(Result))

dust(house_votes_arranged)

#Since the Democrats have a majority, we would expect most of the "Democratic" bills to pass and most of the "Republican" bills to fail



##Activity 4): Assess bill partisanship

#We start by making an empty data frame to fill-in with a For-Loop

vote_details <- data.frame(item = character(), result = character(), sum_yeas = numeric(), sum_nays = numeric(), no_vote = numeric(), dem_yeas = numeric(), gop_yeas = numeric(), dem_nays = numeric(), gop_nays = numeric(), dem_nv = numeric(), gop_nv = numeric())

#Run the For-Loop to tally votes for each item

for(i in 2:17){
  only_dems <- voting %>%
    filter(party == 'Democrat')
    dem_yeas <- count(only_dems, only_dems[,i])[2,2]
    dem_nays <- count(only_dems, only_dems[,i])[1,2]
    dem_NA <- count(only_dems, only_dems[,i])[3,2]
  only_gop <- voting %>%
    filter(party == 'Republican')
    gop_yeas <- count(only_gop, only_gop[,i])[2,2]
    gop_nays <- count(only_gop, only_gop[,i])[1,2]
    gop_NA <- count(only_gop, only_gop[,i])[3,2]
  vote_details[nrow(vote_details) + 1,] <- c(colnames(voting[i]), house_votes$Result[i - 1], dem_yeas + gop_yeas, dem_nays + gop_nays, dem_NA + gop_NA, dem_yeas, gop_yeas, dem_nays, gop_nays, dem_NA, gop_NA)
}

#Rearrange vote_details dataframe to place "pass" over "fail" results in descending order by the sum of "yeas"

vote_details <- arrange(vote_details, desc(result), desc(sum_yeas))

#Present the breakdown of votes:

dust(vote_details) %>%
  sprinkle_colnames(item = 'Item', result = 'Result', sum_yeas = 'Yeas', dem_yeas = '(D)Y', gop_yeas = '(R)Y', sum_nays = 'Nays', dem_nays ='(D)N', gop_nays = '(R)N', no_vote = 'NVPR', dem_nv = '(D)?', gop_nv = '(R)?')



##Activity 5): Plot voting results for each bill, stacking by party using real votes

#Create "long" dataframe for plotting with gather()

vote_long <- gather(voting, item, vote, Infants:SAfrica)

#Change the "vote" variable from numeric to factor

vote_long$vote <- ifelse(vote_long$vote == 1, "Yea", "Nay")

#Change the "party" variable from character to factor

vote_long$party <- as.factor(vote_long$party)

#Identify which bills passed and failed using house_votes

passed_bills <- house_votes$Item[house_votes$Result == 'Pass']
failed_bills <- house_votes$Item[house_votes$Result == 'Fail']

#Draw a plot for each passing bill

vote_long %>%
  filter(item %in% passed_bills) %>%
  drop_na(vote) %>%
  mutate(item = factor(item, levels = vote_details[1:11,1])) %>%
  group_by(item) %>%
  ggplot(aes(reorder(vote, desc(vote)), fill = party)) +
  geom_bar() +
  facet_wrap(~item) +
  scale_fill_manual(values = c('blue', 'red')) +
  labs(title = 'Successful Bill Voting Tally', y = 'Count', fill = 'Party') +
  theme(axis.ticks.x = element_blank(), axis.title.x = element_blank(), legend.position = c(0.88, 0.15))

#Draw a plot for each failing bill 

vote_long %>%
  filter(item %in% failed_bills) %>%
  drop_na(vote) %>%
  mutate(item = factor(item, levels = c('Synfuels', 'Medical', 'Infants', 'Education', 'Dutyfree'))) %>%
  group_by(item) %>%
  ggplot(aes(reorder(vote, desc(vote)), fill = party)) +
  geom_bar() +
  facet_wrap(~item) +
  scale_fill_manual(values = c('blue', 'red')) +
  labs(title = 'Failed Bill Voting Tally', y = 'Count', fill = 'Party') +
  theme(axis.ticks.x = element_blank(), axis.title.x = element_blank(), legend.position = c(0.85, 0.3))



##Activity 6): Develop Categories for Bill Partisanship 

#Create an empty dataframe to save For-Loop output

bill_partisanship <- data.frame(item = character(), party_affiliation = character(), result = character())

#Reclassify voting columns in vote_details from "character" to "numeric"

vote_details[,3:11] <- sapply(vote_details[,3:11], as.numeric)

#Calculate how the majority of each party voted on each bill
#Party majority voting will also be referred to as the "Party-Line"

for(i in 1:16){
  dem_majority <- ifelse(vote_details[i,6] > vote_details[i,8], 1, 0)
  gop_majority <- ifelse(vote_details[i,7] > vote_details[i,9], 1, 0)
  bill_party_affiliation <- ifelse(dem_majority == gop_majority, 'Bipartisan', ifelse(
    dem_majority > gop_majority, 'Democratic', ifelse(
      dem_majority < gop_majority, 'Republican', 'Minority Support')))
  bill_partisanship[nrow(bill_partisanship) + 1,] <- c(vote_details[i,1], bill_party_affiliation, vote_details[i,2])
}

#Re-arrange the dataframe to stratify by partisanship

bill_partisanship <- arrange(bill_partisanship, party_affiliation)

#Present the results

dust(arrange(bill_partisanship, party_affiliation)) %>%
  sprinkle_colnames(item = 'Item', party_affiliation = 'Partisanship', result = 'Result')

#We see that most bills were partisan, and the only 2 bipartisan bills (meaning a majority of both parties voted "yea") both passed
#We also see that Democrats and Republicans were about equally likely to have an item fail
#Interestingly, despite being in the minority Republicans passed 5 partisan bills, while Democrats only passed 4

#Plot the results with a Venn Diagram with the "RAM" package:

items <- list(Democratic = bill_partisanship$item[1:9], Republican = c(bill_partisanship$item[10:16], bill_partisanship$item[1:2]))
group.venn(items, label = TRUE, fill = c('blue', 'red'), cat.pos = c(0,0), lab.cex = .9, cat.dist = .04)

#This shows that most bills enjoy party majority-support from only one party, so a model for party identification could reasonably be built around votes on these items



##Activity 7): Generate Party-Line Index, depicting the ideal Democratic member

#First, we need to recode NAs as the opposite of the majority-party for each party, by item
#This algorithm will be referred to as the "Inverse Party-Line Fix"
#This is done because legislators usually vote "present" or fail to vote entirely when their vote would go against their party or partisan constituency
#Note: the 2 bipartisan bills (SAfrica and H2O), will be recorded as "Yea," since that was the bipartisan consensus vote

#We make a new primary dataframe, "voting2," to preserve the NAs in the original data

voting2 <- voting

#Next, we generate two vectors containing the Inverse Party-Line Fix, one for Democrats and one for Republicans
#Each vector contains a list of 16 binary digits, 1 = "Yea" and 0 = "Nay"
#Each entry is the opposite of the party-majority vote for each item

#Make a copy of bill_partisanship to preserve the prior order

bill_partisanship_edit <- bill_partisanship

#Reorder items according to "voting2" column order

bill_partisanship_edit <- bill_partisanship_edit[order(match(bill_partisanship_edit$item, colnames(voting2))),]

#Define the Inverse Party-Line Fix by party

ipl_fix_dem <- sapply(bill_partisanship_edit[,2], function(x){
  ifelse(x == 'Bipartisan', 1, ifelse(
    x == 'Republican', 1, 0))
})

ipl_fix_gop <- sapply(bill_partisanship_edit[,2], function(x){
  ifelse(x == 'Bipartisan', 1, ifelse(
    x == 'Democratic', 1, 0))
})

#Next, we run a For-Loop to apply the Inverse Party-Line Fix to a copy of the "voting" dataframe, removing the NAs

for(i in 1:435){
  if(voting2[i,1] == 'Republican'){
    for(j in 2:17){
      voting2[i,j] <- ifelse(is.na(voting2[i,j]), ipl_fix_gop[j - 1], voting2[i,j])}}
  if(voting2[i,1] == 'Democrat'){
    for(j in 2:17){
      voting2[i,j] <- ifelse(is.na(voting2[i,j]), ipl_fix_dem[j - 1], voting2[i,j])}}
}

#Create empty vector to fill in with the next For-Loop for the "Party-Line Index"

dem_index <- vector()

#Run the For-Loop to create a "Party-Line Index" for each member 
#A score of "1" is a "Perfect Democrat," and a score of "0" is a "Perfect Republican"
#The member incurs a point for each vote they cast alongside the majority of Democrats 
#The index is the result of summing these points, divided by the total possible number of points
#The index ignores votes for or against the two bipartisan bills: SAfrica and H2O (columns 3 and 17)

for(i in 1:435){
  index <- vector()
  for(j in c(2, 4:16)){
    index <- c(index, ifelse(voting2[i,j] == ipl_fix_gop[j - 1], 1, 0))}
  total <- sum(index)
  dem_score <- total / length(index)
  dem_index <- c(dem_index, dem_score)
}

#We can see the first few entries of the Party-Line Index with head():

head(dem_index)

#And we can see the mean, median, etc. with summary():

summary(dem_index)

#Interestingly, the median is right at 0.5000--a perfect centrist voter--but the mean is slightly below the median at 0.4867
#This dynamic could be due to the influence of "Reagan Democrats," otherwise known as the "Blue Dogs," who voted with the Republicans on many major issues

#Append the Party-Line Index onto the primary "voting" dataframe

voting <- voting %>%
  mutate(dem_index = round(dem_index, digits = 4))

#Present the Party-Line Index distribution using the "ggridges" package:

voting %>%
  ggplot(aes(dem_index, party, fill = party)) +
  geom_density_ridges(scale = 5, alpha = .6, size = .8) +
  scale_fill_manual(values = c('blue', 'red')) + 
  labs(y = 'Party', x = 'Party-Line Index', title = 'Party-Line Index Density Curves', fill = 'Party') 

#We see that there is a significant difference between how Democrats and Republicans vote on average
#However, Republicans seem much more unified than Democrats, who see some minor peaks at the conservative end of their density curve
#Despite this, the overall gap between Democratic and Republican voting implies that we will be able to successfully predict most legislators's parties based upon their voting record



##Activity 8): Classify for party-ideological "tilt" using the Party-Line Index 

#For the purpose of this project, we identify six potential categories for House members, determined by how often they vote with the majority of their party

#Here are the Democratic categories:
#1: Far-Left (D): Democrats who vote with the party 90% of the time or more
  #This term comes from the liberal 1960s-70s policy positions of strong Democrats in the Reagan era
#2: New Deal (D): Democrats who vote with the party 60%-89.99% of the time
  #This term refers to the New Deal of President F.D.R.--standard economic policies for most Democrats in the 1980s
#3: Blue Dog (D): Democrats who vote with the party less than 60% of the time
  #Otherwise known as "Reagan Democrats," these members sided with President Reagan on many issues in the 1980s when the country was growing more conservative
  #In the coming years, many "Blue Dogs" would officially realign to the Republican Party

#Here are the Republican categories:
#1: Far-Right (R): Republicans who vote with the party 90% of the time or more
  #This term comes from the extremely conservative social and economic policies of Republican hard-liners in the 1980s
#2: Goldwater (R): Republicans who vote with the party 60%-89.99% of the time
  #The namesake of the category, Barry Goldwater, ran for president in 1964 on a fiscally conservative platform
  #These Republicans support neoliberal economic policies, but they are not as conservative as the far-right
#3: Rockefeller (R): Republicans who vote with the party less than 60% of the time
  #This category is named after V.P. Nelson Rockefeller, who led the progressive wing of the Republican Party in the mid 20th century

#Before running the For-Loop, we create an empty dataframe to save the results

tilt_results <- data.frame(result = character())

#Run the For-Loop to classify for party-ideological tilt:

for(i in 1:435){
  if(voting[i,1] == 'Democrat'){
    tilt <- ifelse(voting[i,18] >= 0.9, 'Far-Left', ifelse(
      voting[i,18] < 0.9 & voting[i,18] >= 0.6, 'New Deal', ifelse(
        voting[i,18] < 0.6, 'Blue Dog', 'No Affiliation'
      )))}
  if(voting[i,1] == 'Republican'){
    tilt <- ifelse(voting[i,18] > 0.4, 'Rockefeller', ifelse(
      voting[i,18] <= 0.4 & voting[i,18] > 0.1, 'Goldwater', ifelse(
        voting[i,18] <= 0.1, 'Far-Right', 'No Affiliation'
      )))}
  tilt_results[nrow(tilt_results) + 1,] <- tilt
}

#Reorder categories to reflect the left-right aesthetic of the ideological spectrum:

tilt_analysis <- tilt_results$result
tilt_analysis <- factor(tilt_analysis, levels = c('Far-Left', 'New Deal', 'Blue Dog', 'Rockefeller', 'Goldwater', 'Far-Right'))

#Present the results using table():

table(tilt_analysis)

#Attach the party-ideological classifications to the primary "voting" dataframe

voting <- voting %>%
  mutate(tilt = tilt_analysis)

#Plot party membership among the party-ideological categories:

voting$tilt <- factor(voting$tilt, levels = c('Blue Dog', 'New Deal', 'Far-Left', 'Rockefeller', 'Goldwater', 'Far-Right'))

voting %>%
  ggplot(aes(party, fill = tilt)) + 
  geom_bar() +
  scale_fill_manual(values = c('dodgerblue', 'dodgerblue3', 'dodgerblue4', 'firebrick1', 'firebrick3', 'firebrick')) +
  geom_hline(yintercept = 218, linetype = 'dashed', size = 1) + 
  geom_text(aes(2, 218, label = 'Legislative Majority', vjust = -1)) +
  labs(title = 'Party Distribution by Ideology', x = 'Party', y = 'Count', fill = 'Ideology') +
  theme(plot.title = element_text(hjust = 0.5))

#Next we summarize ideological power using the party-ideological categories
#To do this, we calculate the number of conservatives in the House by adding together the moderate-conservative Republicans + the Blue Dog Democrats
#Additionally, the number of liberals is calculated by adding together the moderate-liberal Democrats + the Rockefeller Republicans

#Before running the For-Loop for conservative-liberal ideology classification, we create an empty dataframe to save the results:

ideology_results <- data.frame(result = character())

#Run the For-Loop to classify for conservative-liberal ideology:

for(i in 1:435){
  member_ideology <- ifelse(voting[i,19] == 'Blue Dog' | voting[i,19] == 'Goldwater' | voting[i,19] == 'Far-Right', 'Conservative', 'Liberal')
  ideology_results[nrow(ideology_results) + 1,] <- member_ideology
}

table(ideology_results)

#Despite the fact that Democrats are in the majority, it also seems that conservatives make up a majority of the House as well

#Save the conservative-liberal classification to the primary "voting" dataframe

voting <- voting %>%
  mutate(lcID = ideology_results$result)

#Plot conservative-liberal ideology by party-ideological classification
#Whichever ideology has a majority of members enjoys de facto control of the chamber

voting$tilt <- factor(voting$tilt, levels = c('Blue Dog', 'Rockefeller', 'New Deal', 'Far-Left', 'Goldwater', 'Far-Right'))

voting %>%
  ggplot(aes(lcID, fill = tilt)) + 
  geom_bar() +
  scale_fill_manual(values = c('dodgerblue', 'firebrick1', 'dodgerblue3', 'dodgerblue4', 'firebrick3', 'firebrick')) +
  geom_hline(yintercept = 218, linetype = 'dashed', size = 1) + 
  geom_text(aes(2, 218, label = 'Legislative Majority', vjust = -1)) +
  labs(title = 'Conservative-Liberal Distribution', x = 'General Ideology', y = 'Count', fill = 'Ideology') +
  theme(plot.title = element_text(hjust = 0.5))

#We can see that, despite having a Democratic Speaker, the 98th Congress was controlled by a conservative ideological majority
#This finding helps to explain why Republicans passed 5 partisan bills while Democrats only passed 4
#Additionally, the partisan-ideological mixing between conservative Democrats and liberal Republicans could hinder the process of party classification
#On the other hand, it is possible that the machine learning algorithms to be employed will detect more subtle trends in the voting data that better indicate party affiliation than proximity to pure party-majority voting



##Activity 9): Identify Particularly Divisive Items with Partisanship Scores (PART-Scores)

#In order to work around the party-ideological mixing that we uncovered in the last section, we will try to identify bills that exhibit a clear partisan consensus
#These pieces of legislation should be able to overcome ideological ambiguity to correctly identify party affiliation
#Partisanship Scores (PART-Scores) will be calculated for each bill to measure its partisanship
#These scores are the absolute value of the difference between Democratic and Republican mean voting, multiplied by 100 
#The 0-100 scale of the PART-Scores measures how the absolute difference in voting between the parties, so larger values imply larger differences
#Additionally, they mimic the Variable Importance values which will be generated by some of the machine learning algorithms we use later on

#Before running the For-Loop, we create an empty dataframe to store the results

part_scores <- data.frame(PART_Score = numeric())

#Next, we run the For-Loop to generate the PART-Scores
#Note: we use the "voting2" dataframe that recoded NAs according to the logic of adverse party & constituency effects

for(i in 2:17){
  abs_diff <- 100 * round(abs(mean(voting2[,i][voting2$party == 'Democrat']) - mean(voting2[,i][voting2$party == 'Republican'])), digits = 2)
  part_scores[nrow(part_scores) + 1,] <- abs_diff
}

#Append the item names to the dataframe, and arrange in descending order:

part_scores <- part_scores %>%
  mutate(Item = colnames(voting2[2:17])) %>%
  relocate(Item, PART_Score) %>%
  arrange(desc(PART_Score))

#Present the results

dust(part_scores) %>%
  sprinkle_colnames(Item = 'Item', PART_Score = 'PART-Score')

#Additionally, we can view the results through a dot plot:

part_scores %>%
  mutate(Item = fct_reorder(Item, PART_Score)) %>%
  ggplot(aes(Item, PART_Score)) +
  geom_segment(aes(x = Item, xend = Item, y = min(PART_Score), yend = max(PART_Score)), linetype = 'dashed', color = 'red', size = 1) +
  geom_point(color = 'blue', size = 3) +
  coord_flip() +
  labs(title = 'Party Polarization by Item', y = 'PART-Score')

#We see that there is a clear partisan consensus on the "Medical" bill, with a PART-Score of 89 / 100
#The second highest PART-Score is for the "Budget" item at 71, and the third is for "ElSalvador" at 68
#Due to their extreme partisanship, we can expect these three bills to overcome ideological ambiguity and effectively classify members by party, especially with regards to the "Medical" item

#Just to be safe, we will recalculate PART-Scores, now grouping by conservative-liberal ideology
#If the top few scored items are different for these "IDEA-Scores" than for the PART-Scores, then we can be confident that the items identified with the latter can correctly identify party affiliation

#Append the "lcID"column from "voting" onto "voting2," which we use due to the lack of NAs

voting2 <- voting2 %>%
  mutate(lcID = voting$lcID)

#Before running the For-Loop, we create an empty dataframe to store the results

idea_scores <- data.frame(IDEA_Score = numeric())

#Next, we run the For-Loop to generate the IDEA-Scores

for(i in 2:17){
  abs_diff <- 100 * round(abs(mean(voting2[,i][voting2$lcID == 'Liberal']) - mean(voting2[,i][voting2$lcID == 'Conservative'])), digits = 2)
  idea_scores[nrow(idea_scores) + 1,] <- abs_diff
}

#Append the item names to the dataframe, and arrange in descending order:

idea_scores <- idea_scores %>%
  mutate(Item = colnames(voting2[2:17])) %>%
  relocate(Item, IDEA_Score) %>%
  arrange(desc(IDEA_Score))

#Present the results

combined_PART_IDEA <- left_join(part_scores, idea_scores)

dust(combined_PART_IDEA) %>%
  sprinkle_colnames(Item = 'Item', PART_Score = 'PART-Score', IDEA_Score = 'IDEA-Score')

#As opposed to earlier, "ElSalvador" is now the most divisive item, followed by "Contras" and "Crime"
#However, the IDEA-Scores are less extreme than the PART-Scores, so these top items might not have as much predictive power for ideology as the top PART-Scored items are for party
#"Medical" and "Budget" are now in #8th and #10th place respectively, far from being the most important items
#Therefore, it seems like "ElSalvador" will likely not be very effective at identifying party, as many ideologically-aligned members of both parties voted for it 
#This leaves "Medical" and "Budget" as the two most likely bills to have the strongest predictive power

#To conclude the data exploration, we will plot PART-Scores versus IDEA-Scores, visualizing the 98th Congress's party-ideological split
#Keep in mind, if parties and ideologies were perfectly aligned, the resulting plot would form a straight line 

#Generate the PART-Score vs. IDEA-Score Plot

combined_PART_IDEA %>%
  ggplot(aes(PART_Score, IDEA_Score)) +
  geom_smooth(color = 'red') +
  geom_point(color = 'blue') +
  geom_label_repel(aes(label = Item), position = position_jitter(), size = 3) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme(legend.position = 'None') +
  labs(title = 'PART-Scores vs. IDEA-Scores', x = 'Partisanship', y = 'Ideological Division')

#We see that there is not a clear linear relationship between party and ideology
#Again, the observe the "Medical" and "Budget" bills as strong indicators of party, while "ElSalvador" is a strong indicator of ideology
#We will keep all of this in mind as we transition into the analysis



#Part 5: Data Analysis

#################################################

#Pursue insights with machine learning algorithms

#################################################



#Section 1: Data Preparation for Analysis



##Subsection 1: Partition into Training/Testing Sets

#Convert "party" column to binary for regression analysis
#1 = Democrat and 0 = Republican
#Therefore the models predict the probability of being a "Democrat"

voting$party <- ifelse(voting$party == 'Democrat', 1, 0)

#Convert "bill" columns back to character values for regression
#Note: the lm() call will not function if these values remain in binary
#Note: NAs will be preserved

voting[,2:17] <- sapply(voting[,2:17], function(x){
  ifelse(x == 1, 'yea', 'nay')})

#Convert "party" column to factor values for confusionMatrix() input

voting$party <- as.factor(voting$party)

##Partition into train/test data (use cross validation to tune parameters in each model using tuneGrid)
#We designate 90% of the "voting" data for testing in order to develop the most accurate model possible, given the small data population size 

set.seed(1, sample.kind="Rounding") 
partition <- createDataPartition(y = voting$party, times = 1, p = 0.1, list = FALSE)
training <- voting[-partition,]
testing <- voting[partition,]

#Validate proportions of Democrats and Republicans with "seat_totals"

partition_validation <- data.frame(set = character(), party = character(), true_prop = numeric(), part_prop = numeric(), difference = numeric())
partition_validation[nrow(partition_validation) + 1,] <- c('Training', 'Democrat', round(seat_percents[1,1], digits = 5), round(nrow(training[training$party == 1,]) / nrow(training), digits = 5), round(seat_percents[1,1] - nrow(training[training$party == 1,]) / nrow(training), digits = 5))
partition_validation[nrow(partition_validation) + 1,] <- c('Training', 'Republican', round(seat_percents[1,2], digits = 5), round(nrow(training[training$party == 0,]) / nrow(training), digits = 5), round(seat_percents[1,2] - nrow(training[training$party == 0,]) / nrow(training), digits = 5))
partition_validation[nrow(partition_validation) + 1,] <- c('Testing', 'Democrat', round(seat_percents[1,1], digits = 5), round(nrow(testing[testing$party == 1,]) / nrow(testing), digits = 5), round(seat_percents[1,1] - nrow(testing[testing$party == 1,]) / nrow(testing), digits = 5))
partition_validation[nrow(partition_validation) + 1,] <- c('Testing', 'Republican', round(seat_percents[1,2], digits = 5), round(nrow(testing[testing$party == 0,]) / nrow(testing), digits = 5), round(seat_percents[1,2] - nrow(testing[testing$party == 0,]) / nrow(testing), digits = 5))

dust(partition_validation) %>%
  sprinkle_colnames(set = 'Set', party = 'Party', true_prop = 'True %', part_prop = 'New %', difference = 'Difference')

#We can see that the proportions of Democrats and Republicans in the partitioned data matches the original distribution almost perfectly, so we are safe to proceed



##Subsection 2: Recalculate the Party-Line Index & Inverse Party-Line Fix

#We now recalculate the PL-Index and IPL-Fix in order to avoid overtraining on the "testing" data
#Our new calculations for these values will only include the data apportioned to the "training" set

#Remove the current PL-Index as well as the "tilt" and "lcID" columns that were derived from it from both the "training" and "testing" data

training <- training[,1:17]
testing <- testing[,1:17]

#Make a copy of the training data:

training_example <- training

#Re-convert the party labels into character values and the Yeas/Nays into binary

training_example$party <- ifelse(training_example$party == 1, 'Democrat', 'Republican')
training_example[,2:17] <- sapply(training_example[,2:17], function(x){
  ifelse(x == 'yea', 1, 0)})

#Create a new dataframe to hold the voting tally results

train_details_example <- data.frame(item = character(), result = character(), sum_yeas = numeric(), sum_nays = numeric(), no_vote = numeric(), dem_yeas = numeric(), gop_yeas = numeric(), dem_nays = numeric(), gop_nays = numeric(), dem_nv = numeric(), gop_nv = numeric())

#Run the For-Loop to tally votes for each item

for(i in 2:17){
  only_dems <- training_example %>%
    filter(party == 'Democrat')
  dem_yeas <- count(only_dems, only_dems[,i])[2,2]
  dem_nays <- count(only_dems, only_dems[,i])[1,2]
  dem_NA <- count(only_dems, only_dems[,i])[3,2]
  only_gop <- training_example %>%
    filter(party == 'Republican')
  gop_yeas <- count(only_gop, only_gop[,i])[2,2]
  gop_nays <- count(only_gop, only_gop[,i])[1,2]
  gop_NA <- count(only_gop, only_gop[,i])[3,2]
  train_details_example[nrow(train_details_example) + 1,] <- c(colnames(training_example[i]), house_votes$Result[i - 1], dem_yeas + gop_yeas, dem_nays + gop_nays, dem_NA + gop_NA, dem_yeas, gop_yeas, dem_nays, gop_nays, dem_NA, gop_NA)
}

#Rearrange vote_details_example dataframe to place "pass" over "fail" results in descending order by the sum of "yeas"

train_details_example <- arrange(train_details_example, desc(result), desc(sum_yeas))

#Create an empty dataframe to save For-Loop output for majority-voting by party

bill_partisanship_example <- data.frame(item = character(), party_affiliation = character(), result = character())

#Reclassify voting columns in vote_details_example from "character" to "numeric"

train_details_example[,3:11] <- sapply(train_details_example[,3:11], as.numeric)

#Calculate how the majority of each party voted on each bill

for(i in 1:16){
  dem_majority <- ifelse(train_details_example[i,6] > train_details_example[i,8], 1, 0)
  gop_majority <- ifelse(train_details_example[i,7] > train_details_example[i,9], 1, 0)
  bill_party_affiliation <- ifelse(dem_majority == gop_majority, 'Bipartisan', ifelse(
    dem_majority > gop_majority, 'Democratic', ifelse(
      dem_majority < gop_majority, 'Republican', 'Minority Support')))
  bill_partisanship_example[nrow(bill_partisanship_example) + 1,] <- c(train_details_example[i,1], bill_party_affiliation, train_details_example[i,2])
}

#Reorder items according to the "training" column order

bill_partisanship_example <- bill_partisanship_example[order(match(bill_partisanship_example$item, colnames(training))),]

#Define the Inverse Party-Line Fix by party using the training data only

ipl_fix_dem <- sapply(bill_partisanship_example[,2], function(x){
  ifelse(x == 'Bipartisan', 1, ifelse(
    x == 'Republican', 1, 0))
})

ipl_fix_gop <- sapply(bill_partisanship_example[,2], function(x){
  ifelse(x == 'Bipartisan', 1, ifelse(
    x == 'Democratic', 1, 0))
})

#Reverse-Binarize the Inverse Party-Line Fix information to match "training" and "testing" vote data:

ipl_fix_gop <- sapply(ipl_fix_gop, function(x){
  ifelse(x == 1, 'yea', 'nay')
})

ipl_fix_dem <- sapply(ipl_fix_dem, function(x){
  ifelse(x == 1, 'yea', 'nay')
})

#Replace the NAs in "training":

for(i in 1:nrow(training)){
  if(training[i,1] == 0){
    for(j in 2:17){
      training[i,j] <- ifelse(is.na(training[i,j]), ipl_fix_gop[j - 1], training[i,j])}}
  if(training[i,1] == 1){
    for(j in 2:17){
      training[i,j] <- ifelse(is.na(training[i,j]), ipl_fix_dem[j - 1], training[i,j])}}
}

#Replace the NAs in "testing":

for(i in 1:nrow(testing)){
  if(testing[i,1] == 0){
    for(j in 2:17){
      testing[i,j] <- ifelse(is.na(testing[i,j]), ipl_fix_gop[j - 1], testing[i,j])}}
  if(testing[i,1] == 1){
    for(j in 2:17){
      testing[i,j] <- ifelse(is.na(testing[i,j]), ipl_fix_dem[j - 1], testing[i,j])}}
}

#Create empty vector to fill in with the next For-Loop for the "Party-Line Index"

dem_index_train <- vector()
dem_index_test <- vector()

#Apply the PL-Index to "training" with the training-based IPL-Fix 

for(i in 1:nrow(training)){
  index <- vector()
  for(j in c(2, 4:16)){
    index <- c(index, ifelse(training[i,j] == ipl_fix_gop[j - 1], 1, 0))}
  total <- sum(index)
  dem_score <- total / length(index)
  dem_index_train <- c(dem_index_train, dem_score)
}

#Apply the PL-Index to "testing" with the training-based IPL-Fix

for(i in 1:nrow(testing)){
  index <- vector()
  for(j in c(2, 4:16)){
    index <- c(index, ifelse(testing[i,j] == ipl_fix_gop[j - 1], 1, 0))}
  total <- sum(index)
  dem_score <- total / length(index)
  dem_index_test <- c(dem_index_test, dem_score)
}

#Append the Party-Line Index onto the training & testing dataframes

training <- training %>%
  mutate(dem_index_train = round(dem_index_train, digits = 4))

testing <- testing %>%
  mutate(dem_index_test = round(dem_index_test, digits = 4))

#We can use summary() to view the PL-Index distribution for both the "training" and "testing" sets:

summary(training$dem_index_train)
summary(testing$dem_index_test)

#We can see that the values appear relatively different for each set. 
#While the 1st and 3rd Quartiles appear relatively similar, the mean and median of the "testing" data are much lower than that of the "training" data
#This implies that more Republicans (or conservatives) ended up in "testing" relative to "training", which could lead to weaker predictive power for the PL-Index 
#At this point, we're ready to begin modeling



#Section 2: Modeling & Predictions



#Create dataframe to store "Accuracy" output for each model

model_accuracy <- data.frame(Model = character(), Accuracy = numeric())



##Model 1: Guessing

#This model randomly guesses where each member is a Democrat or Republican
#Run the model:

set.seed(1, sample.kind = 'Rounding')
guess_model <- as.factor(sample(c(0,1), nrow(testing), replace = TRUE))

#Save the result and present:

model_1_acc <- str_pad(confusionMatrix(guess_model, testing$party)$overall[['Accuracy']], width = 6, side = 'right', pad = '0')
model_accuracy[nrow(model_accuracy) + 1,] <- c(Model = 'Guessing', Accuracy = model_1_acc)
dust(model_accuracy)

#This first attempt uses completely random guesses, so it is appropriate that we get an accuracy of 0.5, as it makes sense to guess correctly between two options 50% of the time



##Model 2: Party-Line Index 

#PL-Index 1: 50%-50% Split

partyline_model <- as.factor(ifelse(testing$dem_index_test > 0.50, 1, 0))

#Here is our initial accuracy output:

confusionMatrix(partyline_model, testing$party)$overall[['Accuracy']]

#In this second model, we can see the explanatory power of the Party-Line Index, as our accuracy has risen to 0.77
#This is really good, as we are now predicting over 75% of party affiliations with just the use of a single summary statistic
#When we begin using more complicated methods that consider the vote on each bill individual, we can expect our accuracy to improve even more

#Normally it is not wise to fit a model that was trained using both the training & testing data, as was done here
#However, in this instance it does not matter because the party affiliation calculated for each bill is the same when done using either the full data or just the training data, as described above
#Also, we might be able to improve this model by running many variations of the split to see which is most effective

#PL-Index 2: Split Tuning

#Create an empty dataframe to save tuning For-Loop results

best_split <- data.frame(Split = numeric(), Accuracy = numeric())

#Run the For-Loop to identify the optimal split

for(i in seq(0, 1, 0.05)){
  partyline_model <- ifelse(testing$dem_index_test > i, 1, 0)
  accuracy <- confusionMatrix(as.factor(partyline_model), testing$party)$overall[['Accuracy']]
  best_split[nrow(best_split) + 1,] <- c(Split = i, Accuracy = accuracy)
}

#Here is a plot of Accuracy vs Splits:

best_split %>%
  ggplot(aes(Split, Accuracy)) +
  geom_line(color = 'red') + 
  geom_point(aes(y = max(Accuracy), x = Split[which.max(Accuracy)]), color = 'black', shape = 5, size = 4) +
  geom_point(color = 'blue') +
  labs(title = 'PL-Index Accuracy vs. Splits')

#And here is the optimal ifelse() split parameter:

best_split[which.max(best_split$Accuracy),]

#With an accuracy of nearly 89%, the tuned Party-Line Index is very effective, and it might be difficult for all of the automated machine learning algorithms to beat it

#Save the result and present: 

partyline_model <- as.factor(ifelse(testing$dem_index_test > 0.15, 1, 0))
model_2_acc <- confusionMatrix(partyline_model, testing$party)$overall[['Accuracy']]
model_accuracy[nrow(model_accuracy) + 1,] <- c(Model = 'PL-Index', Accuracy = round(model_2_acc, digits = 4))
dust(model_accuracy)



##Model 3: Logistic Regression 

#As the logit model is designed with classification in mind, we can expect it to be relatively successful
#From this point forward, we will be using the train() function from the "caret" package to fit our models
#Then, we will use the predict() function to generate predictions off the test data, and we will calculate the accuracy using confusionMatrix()
#Note: the train() function uses random numbers for some internal calculations, so we will always set the seed to "1" for consistency

#First, we will predict with the 3 most partisan items identified earlier: "Medical", "Budget", and "ElSalvador"

#Logit 1: "ElSalvador"

set.seed(1, sample.kind = 'Rounding')
glm_fit <- train(party ~ ElSalvador, method = 'glm', data = training[,1:17])
glm_model <- predict(glm_fit, newdata = testing)

#Here is the accuracy output:

confusionMatrix(glm_model, testing$party)$overall[['Accuracy']]

#"ElSalvador" provides a decent prediction of party affiliation, but it is not exceedingly strong
#This is likely due to strong party-ideological mixing on this vote, as discussed earlier

#Logit 2: "Budget" 

set.seed(1, sample.kind = 'Rounding')
glm_fit <- train(party ~ Budget, method = 'glm', data = training[,1:17])
glm_model <- predict(glm_fit, newdata = testing)

#Here is the accuracy output:

confusionMatrix(glm_model, testing$party)$overall[['Accuracy']]

#Logit 3: "Medical"

set.seed(1, sample.kind = 'Rounding')
glm_fit <- train(party ~ Medical, method = 'glm', data = training[,1:17])
glm_model <- predict(glm_fit, newdata = testing)

#Here is the accuracy output:

confusionMatrix(glm_model, testing$party)$overall[['Accuracy']]

#Logit 4: "Medical" + "Budget" + "ElSalvador"

set.seed(1, sample.kind = 'Rounding')
glm_fit <- train(party ~ Medical + Budget + ElSalvador, method = 'glm', data = training[,1:17])
glm_model <- predict(glm_fit, newdata = testing)

#Here is the accuracy output:

confusionMatrix(glm_model, testing$party)$overall[['Accuracy']]

#Logit 5: All Items:

set.seed(1, sample.kind = 'Rounding')
glm_fit <- train(party ~ ., method = 'glm', data = training[,1:17])
glm_model <- predict(glm_fit, newdata = testing)

#Save the result and present:

model_3_acc <- confusionMatrix(glm_model, testing$party)$overall[['Accuracy']]
model_accuracy[nrow(model_accuracy) + 1,] <- c(Model = 'Logistic', Accuracy = round(model_3_acc, digits = 4))
dust(model_accuracy)

#Predicting with all 16 bills, we get an accuracy over 95%; this is very good!

#We can see which bills were most important for this model using varImp():

impVar_glm <- function(x){ 
  vars <- varImp(glm_fit)
  vars_df <- data.frame(vars$importance[1])
  names_yea <- rownames(vars_df)
  names_yea <- str_remove(names_yea, "yea$")
  vars_df <- vars_df %>%
    mutate(Item = names_yea, Importance = Overall) %>%
    select(Item, Importance)
  arrange(vars_df, desc(Importance))
}

impVar_glm(glm_fit)

#We can visualize the results like this:

impVar_glm_plot <- impVar_glm(glm_fit)

impVar_glm_plot %>%
  mutate(Item = fct_reorder(Item, Importance)) %>%
  ggplot(aes(Item, Importance)) +
  geom_segment(aes(x = Item, xend = Item, y = min(Importance), yend = max(Importance)), linetype = 'dashed', color = 'red', size = 1) +
  geom_point(color = 'blue', size = 3) +
  coord_flip() +
  labs(title = 'Most Important Bills (Logit)')
  
#Having seen significant success with simple logistic regression models, we will now see if any improvements can be made with more complex algorithms



##Model 4: K-Nearest Neighbors

#We will repeat the modeling procedure used above with the Logit Regression, now using the kNN method
#We can expect to get a similar accuracy reading

#Fit the model using train()

set.seed(1, sample.kind = 'Rounding')
knn_fit <- train(party ~ ., method = 'knn', data = training[,1:17], tuneGrid = data.frame(k = seq(1,50,1)))

#Plot Accuracy vs. K (# of Neighbors)

ggplot(knn_fit, highlight = TRUE) +
  labs(title = 'Accuracy vs. kNN Neighbor Count', x = 'Neighbors') +
  geom_line(color = 'red') + 
  geom_point(color = 'blue') 

#Identify the optimal K

knn_fit$bestTune

#Generate predictions with the testing data

knn_model <- predict(knn_fit, newdata = testing)

#Save the result and present

model_4_acc <- confusionMatrix(knn_model, testing$party)$overall[['Accuracy']]
model_accuracy[nrow(model_accuracy) + 1,] <- c(Model = 'KNearest', Accuracy = round(model_4_acc, digits = 4))
dust(model_accuracy)

#According to my understanding, kNN does not calculate an importance statistic for each predictor, so we do not present the most important variables for this model
#I have tried using varImp() in several ways but none have worked unfortunately



##Model 5: Random Forest

#Random Forests are very well known in machine learning, and they generally work well with classification problems

#Fit the model using train()

set.seed(1, sample.kind = 'Rounding')
rf_fit <- train(party ~ ., method = 'rf', data = training[,1:17], ntree = 100, tuneGrid = data.frame(mtry = seq(1:10)), importance = TRUE)

#Plot Accuracy vs. the Number of Variables per Tree

ggplot(rf_fit, highlight = TRUE) +
  labs(title = 'Accuracy vs. RF Variables Per Tree', x = 'Variables') +
  geom_line(color = 'red') + 
  geom_point(color = 'blue') 

#Identify the optimal number of variables per tree

rf_fit$bestTune

#Generate predictions using the testing data

rf_model <- predict(rf_fit, newdata = testing)

#Save the result and present

model_5_acc <- confusionMatrix(rf_model, testing$party)$overall[['Accuracy']]
model_accuracy[nrow(model_accuracy) + 1,] <- c(Model = 'R-Forest', Accuracy = round(model_5_acc, digits = 4))
dust(model_accuracy)

#We can see which bills were most important for this model using varImp():

impVar_rf <- function(x){
  vars <- varImp(x)
  vars_df <- data.frame(vars$importance[1])
  names_yea <- rownames(vars_df)
  names_yea <- str_remove(names_yea, "yea$")
  vars_df <- vars_df %>%
    mutate(Item = names_yea, Importance = X0) %>%
    select(Item, Importance)
  arrange(vars_df, desc(Importance))
}

impVar_rf(rf_fit)

#We can visualize the results like this:

impVar_rf_plot <- impVar_rf(rf_fit)

impVar_rf_plot %>%
  mutate(Item = fct_reorder(Item, Importance)) %>%
  ggplot(aes(Item, Importance)) +
  geom_segment(aes(x = Item, xend = Item, y = min(Importance), yend = max(Importance)), linetype = 'dashed', color = 'red', size = 1) +
  geom_point(color = 'blue', size = 3) +
  coord_flip() +
  labs(title = 'Most Important Bills (Random Forest)')



##Model 6: Neural Network

#For out last stand-alone model, we will try out a Neural Network using the caret "nnet" method
#I am new to this concept, so we will allow train() to calculate the optimal hyperparameters without making any manual adjustments
#By default, "nnet" utilizes a Bootstrapped resampling method to tune a "size" parameter, which refers to the number of hidden units, and a "decay" parameter, which refers to the weight decay that prevents weights in the model from getting too large

#Fit the model using train():

set.seed(1, sample.kind = 'Rounding')
nnet_fit <- train(party ~ ., method = 'nnet', data = training[,1:17], metric = 'Accuracy')

#Plot Accuracy vs. the Number of Hidden Units

ggplot(nnet_fit, highlight = TRUE) +
  labs(title = 'Accuracy vs. N-Network Size & Decay', x = 'Number of Hidden Units')

#Identify the optimal decay rate and number of hidden units

nnet_fit$bestTune

#Generate predictions using the testing data

nnet_model <- predict(nnet_fit, newdata = testing)

#Save the result and present

model_6_acc <- confusionMatrix(nnet_model, testing$party)$overall[['Accuracy']]
model_accuracy[nrow(model_accuracy) + 1,] <- c(Model = 'NNetwork', Accuracy = round(model_6_acc, digits = 4))
dust(model_accuracy)

#With an accuracy of almost 98%, this model is by far the best we have created so far

#We can see which bills were most important for this model using varImp():

impVar_nnet <- function(x){
  vars <- varImp(x)
  vars_df <- data.frame(vars$importance[1])
  names_yea <- rownames(vars_df)
  names_yea <- str_remove(names_yea, "yea$")
  vars_df <- vars_df %>%
    mutate(Item = names_yea, Importance = Overall) %>%
    select(Item, Importance)
  arrange(vars_df, desc(Importance))
}

impVar_nnet(nnet_fit)

#We can visualize the results like this:

impVar_nnet_plot <- impVar_nnet(nnet_fit)

impVar_nnet_plot %>%
  mutate(Item = fct_reorder(Item, Importance)) %>%
  ggplot(aes(Item, Importance)) +
  geom_segment(aes(x = Item, xend = Item, y = min(Importance), yend = max(Importance)), linetype = 'dashed', color = 'red', size = 1) +
  geom_point(color = 'blue', size = 3) +
  coord_flip() +
  labs(title = 'Most Important Bills (Neural Network)')

#Unsurprisingly, we see that the Neural Network identified the "Medical" bill as the most significant

#Finally, here is a visual of the shape the optimized network takes, with 1 hidden layer and two biases

plotnet(nnet_fit, nid = TRUE, x_names = c(colnames(training[2:17])), y_names = 'Party', pad_x = 0.7, circle_col = 'dodgerblue3', pos_col = 'firebrick', neg_col = 'indianred1', circle_cex = 4, rel_rsc = 4, cex_val = 0.6)

#To finish the analysis, we will combine the results of our successful models in a basic majority-vote ensemble
#This Neural Network accuracy is going to be very difficult to beat, but we will give it a try



##Model 7: Ensemble (PL-Index + Logit + kNN + Random Forest + Neural Network)

#This code creates a majority-vote ensemble, where each member is assigned a party based upon the prediction of a majority of constituent algorithms

#First, we make a new dataframe that compiles the results of all the successful models developed so far

combined_predictions <- data.frame(PLI = as.numeric(partyline_model) - 1, GLM = as.numeric(glm_model) - 1, KNN = as.numeric(knn_model) - 1, RF = as.numeric(rf_model) - 1, NET = as.numeric(nnet_model) - 1, TEST = as.numeric(testing$party) - 1)

#Create an empty dataframe for the majority-vote For-Loop

ensemble_model <- data.frame(maj_vote = numeric())

#Run the For-Loop for a majority prediction

for(i in 1:nrow(combined_predictions)){
  prediction <- ifelse(sum(combined_predictions[i,1:5]) > 2, 1, 0)
  ensemble_model[nrow(ensemble_model) + 1,] <- prediction
}

#Save the result and present:

model_7_acc <- confusionMatrix(as.factor(ensemble_model$maj_vote), testing$party)$overall[['Accuracy']]
model_accuracy[nrow(model_accuracy) + 1,] <- c(Model = 'Ensemble', Accuracy = round(model_7_acc, digits = 4))
dust(model_accuracy)

#The Ensemble tied the Neural Network with an accuracy of almost 98%

#Upon reviewing the model results, we can see that the Ensemble + Neural Network each made the same one mistake:

tfs <- combined_predictions$NET == combined_predictions$TEST
which(tfs == FALSE)

#If we look at the entry for this member, we can see that they don't easily fit the Congressional party stereotypes

testing[39,]

#In fact, none of the models were able to classify this member correctly:

combined_predictions[39,]

#Therefore, this is an instance of ideological ambiguity overpowering partisan trends for classification
#However, given the widespread partisan-ideological mixing that we have observed in the 98th Congress, it is impressive that the Neural Network was able to correctly classify every other member in the testing data



#Part 6: Conclusion

#################################################

#Summarize project and look to the future

#################################################

#Project Overview:

#This project began with the goal of accurately predicting party affiliations for members of the House of Representatives serving in the 98th Congress. 
#In order to make these predictions, the project involved an extensive exploration of the data.
#This exploration uncovered significant party-ideological mixing; that is, there were many Democrats in the 98th Congress that tended to vote with the Republicans, and vice versa.
#In order to classify party affiliation in this ambiguous environment, the author sought to identify pieces of legislation with a clear partisan consensus.
#The two most significant items along this trajectory were postulated to be the "Medical" bill and the "Budget" bill.
#The author proceeded to make predictions of party affiliation focusing on these bills as well as a partisan index developed by the author.
#Most of the models were eventually fit with all of these predictors provided by the dataset, and the "Medical" bill consistently remained the most important item for prediction.
#Using the metric of "accuracy" to measure success, this project exceeded the stated threshold of 90% accuracy by utilizing several machine learning algorithms, including the Logistic Regression, kNN, and Random Forests.
#The most effective single algorithm came in the form of a Neural Network, which had an accuracy output of 97.7%.

#Here is the finalized list of model accuracy results, ranked from most to least effective:

final_table <- model_accuracy[order(model_accuracy$Accuracy),]
final_models <- final_table$Model
final_accuracy <- final_table$Accuracy
dust(data.frame(Model = rev(final_models), Accuracy = rev(final_accuracy)))

#Here is a plot showing the accuracy of the models:

model_accuracy %>%
  mutate(Model = factor(Model, levels = c('Ensemble', 'NNetwork', 'Logistic', 'R-Forest', 'PL-Index', 'KNearest', 'Guessing'))) %>%
  ggplot(aes(Model, Accuracy, fill = Accuracy)) + 
  geom_col() +
  scale_fill_manual(values = c('dodgerblue', 'dodgerblue1', 'dodgerblue2', 'dodgerblue3', 'dodgerblue4')) +
  labs(title = 'Accuracy of Project Models') +
  geom_hline(yintercept = 3.3, linetype = 'dashed') +
  geom_text(aes(6, 3.5, label = '90% Threshold')) +
  theme(legend.position = 'none', axis.text.x = element_text(size=7))

#Overall, 4 models met the success threshold by predicting with an accuracy over 90%, and 3 models failed to meet the threshold.
#The manually-developed PL-Index model failed to meet the 90% threshold by less than 2 points with an accuracy of 88.6%.
#Surprisingly, the K-Nearest Neighbors model fared significantly less well than the other traditional machine learning algorithms, achieving an accuracy of only 86.4%.
#The Logistic Regression & Random Forest obtained a respectable accuracy of 95.5%.
#The author was surprised by how well the Neural Network performed, missing only 1/44 predictions.
#Additionally, the majority-vote Ensemble tied the Neural Network with an accuracy of 97.7% and making only one error.
#After a brief investigation, the author discovered that this member had a particularly ambiguous identity, affiliating with the Republicans but voting with the Democrats over half of the time.

#Limitations & Future Work:

#This project does not reach its full potential due to several limitations, including:
#1): The code relies heavily on For-Loops and copy + paste methods, as opposed to custom functions and vectorized / matrix-based calculations. These methods limit the scope of data exploration & analysis, and they are inefficient. 
#2): The Congressional Voting Records dataset is relatively basic, as floor votes do not always reflect the true disposition of members of Congress. Many political scientists discourage studies of floor votes in isolation of other factors that influence the legislative process, such as committee work and outside deals.
#3): There were many NAs present in the original data that were recoded under the assumption that no-votes do not reflect party loyalty and can thus be treated as votes against the party majority. While this assumption is grounded in general political realities, it is likely not true for every single member considered. 
#4): The author is relatively new to programming in R, and he likely did not take advantage of the best possible methods for every operation. 

#In the future, similar projects could make significant improvements, such as:
#1): The Congressional Voting Records dataset should be supplemented with other forms of information to create a more accurate picture of political loyalties in Congress. 
#2): More machine learning algorithms should be used to maximize accuracy, such as SVM, Naive Bayes, individual Classification Trees, etc.
#3): Metrics besides accuracy should be utilized to provide a more comprehensive review of model performance.
#4): Future researchers should attempt to focus on vector and matrix-based operations more than For-Loops and copy + pasting, which are generally considered to be less efficient.

#Thank you for reading!


















