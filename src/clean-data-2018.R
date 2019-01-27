# title: Clean freeCodeCamp's 2018 New Coder Survey
# description: This script cleans specifically freeCodeCamp's 2018 New Coder
#   Survey.
# author: Eric Leung (@erictleung)
# date: 2018-09-03
# last_updated: 2018-12-19


# Overview -------------------------------------------------------------
# - Load packages               Load necessary packages for cleaning
# - Useful Functions            Sub-components for processing data
# - Main Processing Functions   Big, main components of cleaning
# - Main Function               Run entire script to clean data


# Load Packages --------------------------------------------------------

# Used packages in tidyverse
# - dplyr
# - tidyr
# - stringr
library(tidyverse)
library(here)


# Sub-Process Functions -----------------------------------
# Description:
#   These functions perform larger grouped data transformations

# Title:
#   Change All Undefined Values to NA
# Description:
#   The second dataset contains values from the first part of the survey.
#   Those were passed as values and the missing values (i.e. NA) were passed
#   as "undefined" and need to be transformed back.
# Input:
#   Designed for the second dataset
# Output:
#   The second dataset with all the undefined changed to NA
# Usage:
#   > part2 <- undefined_to_NA(part2)
undefined_to_NA <- function(part2, changeCols) {
  fixedPart2 <- part2
  for (col in changeCols) {
    varval <- lazyeval::interp(~ ifelse(colName == "undefined",
                                        yes = NA,
                                        no = colName),
                               colName = as.name(col))
    fixedPart2 <- fixedPart2 %>%
      mutate_(.dots = setNames(list(varval), col))
  }
  fixedPart2
}


# Title:
#   Change All Yes/No to 1/0
# Description:
#   The second dataset contains values from the first part of the survey. Some
#   of the yes/no questions were encoded as yes/no and 1/0. This function
#   serves to make them consistently into 1/0.
# Input:
#   Designed for the second dataset
# Output:
#   The second dataset with all yes/no answers changed to 1/0
# Usage:
#   > part2 <- yesNo_to_oneZero(part2)
yesNo_to_oneZero <- function(part2, changeCols) {
  fixedPart2 <- part2
  for (col in changeCols) {
    varvalYes <- lazyeval::interp(~ ifelse(colName == "Yes",
                                           yes = "1",
                                           no = colName),
                                  colName = as.name(col))
    varvalNo <- lazyeval::interp(~ ifelse(colName == "No",
                                          yes = "0",
                                          no = colName),
                                 colName = as.name(col))
    fixedPart2 <- fixedPart2 %>%
      mutate_(.dots = setNames(list(varvalYes), col)) %>%
      mutate_(.dots = setNames(list(varvalNo), col))
  }
  fixedPart2
}


# Title:
#   Change Characters to One
# Description:
#   Lots of columns need to be changed from characters to just 1, indicated that
#   the respondent checked this option. Check if input is NA and changes it to a
#   character 1.
#
#   Note: vchar_to_one is meant to be used on vectors.
# Input:
#   Vector of characters, can have NA
# Output:
#   Vector of characters, with just "1"'s and NAs
# Usage:
#   > test_vec <- c(sample(letters, 10), rep(NA, 3))
#   > vchar_to_one(tes)
#   #   x    f    r    w    i    v    q    a    l    h <NA> <NA> <NA>
#   # "1"  "1"  "1"  "1"  "1"  "1"  "1"  "1"  "1"  "1"   NA   NA   NA
char_to_one <- function(x) {
  if (!is.na(x)) {
    "1"
  } else {
    x
  }
}
vchar_to_one <- Vectorize(char_to_one)


# Title:
#   Change to Character
# Description:
#   Changes the data type of the given column names into character
# Input:
#   part1 = the part 1 dataset
#   toChr = string or vector of strings with column names needing change
# Output:
#   Changed part 1 dataset
# Usage:
#   > part1 <- change_to_chr(part1, toChr)
change_to_chr <- function(part1, toChr) {
  for (colName in toChr) {
    varval <- lazyeval::interp(~ as.character(colHere),
                               colHere = as.name(colName))
    part1 <- part1 %>%
      mutate_(.dots = setNames(list(varval), colName))
  }
  part1
}


# Title:
#   Change to Double
# Description:
#   Changes the data type of the given column names into double
# Input:
#   part2 = the part 2 dataset
#   toDbl = string or vector of strings with column names needing change
# Output:
#   Changed part 2 dataset
# Usage:
#   > part2 <- change_to_dbl(part2, toDbl)
change_to_dbl <- function(part2, toDbl) {
  for (colName in toDbl) {
    varval <- lazyeval::interp(~ as.double(colHere),
                               colHere = as.name(colName))
    part2 <- part2 %>%
      mutate_(.dots = setNames(list(varval), colName))
  }
  part2
}


# Title:
#   Substitution function [WIP]
# Description:
#   Used directly on a dplyr data frame to grep substitute
# Input:
#   dplyr data frame
# Output:
#   dplyr data frame
sub_and_rm <- function(dirtyDat, colName, findStr, replaceStr) {
  varval <- lazyeval::interp(~ gsub(f, r, c),
                             f = findStr,
                             r = replaceStr,
                             c = as.name(colName))
  
  dirtyIdx <- dirtyDat %>% select_(colName) %>%
    mutate_each(funs(grepl(findStr, ., ignore.case = TRUE))) %>%
    unlist(use.names = FALSE)
  subDirty <- dirtyDat %>% filter(dirtyIdx) %>%
    mutate_(.dots = setNames(list(varval), colName))
  
  cleanDat <- dirtyDat %>% filter(!dirtyIdx) %>% bind_rows(subDirty)
  
  cleanDat
}


# Title:
#   Normalize Text
# Description:
#   Normalize text based on searching list and desired single replacement
#   using non-standard eval in dplyr: http://stackoverflow.com/a/26003971
# Input:
#   inData        = dplyr data frame,
#   columnName    = column you want to change,
#   search Terms  = search terms in a c() vector,
#   replaceWith   = replacement string
# Output:
#   dplyr data frame
# Usage:
#   > cleanPart1 <- normalize_text(inData = cleanPart1,
#   + columnName = "JobRoleInterestOther",
#   + searchTerms = undecidedWords,
#   + replaceWith = "Undecided")
# More on NSE:
#   https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html
# Adapted from:
#   http://stackoverflow.com/a/26766945
normalize_text <- function(inData, columnName, searchTerms, replaceWith) {
  # Setup dynamic naming of variables later in function
  varval <- lazyeval::interp(~ replaceText, replaceText = replaceWith)
  
  # Gets indices for rows that need to be changed
  searchStr <- paste(searchTerms, collapse = "|")
  wordIdx <- inData %>% select_(columnName) %>%
    mutate_each(funs(grepl(searchStr, ., ignore.case = TRUE))) %>%
    unlist(use.names = FALSE)
  
  # Change row values to intended words
  wordData <- inData %>% filter(wordIdx) %>%
    mutate_(.dots = setNames(list(varval), columnName))
  
  # Combine data back together
  cleanData <- inData %>% filter(!wordIdx) %>% bind_rows(wordData)
  
  cleanData
}


# Title:
#   Create New Column Based on Grep
# Description:
#   This function will search for terms in a given column in the rows. It will
#   then add a column with the name of your choosing and label rows that
#   contain your search term as having that term i.e. give a value of "1".
# Input:
#   inData       = dplyr data frame,
#   colName      = column you want to target,
#   searchTerms  = search terms in a c() vector,
#   newCol       = name for new column
# Output:
#   dplyr data frame with new column
# Usage:
#   > cleanPart1 <- search_and_create(inData = cleanPart1,
#   + colName = "CodeEventOther", searchTerms = c("meetup", "meetup"),
#   + newCol = "CodeEventMeetups")
search_and_create <- function(inData, colName, searchTerms, newCol) {
  # Create new column with new name
  makeNew <- lazyeval::interp(~ as.character(NA))
  cleanData <- inData %>% mutate_(.dots = setNames(list(makeNew), newCol))
  
  # Create search criteria
  searchStr <- paste(searchTerms, collapse = "|")
  varval <- lazyeval::interp(~ grepl(s,c, ignore.case = TRUE),
                             s = searchStr,
                             c = as.name(colName))
  
  # Label target rows as belonging to new column group
  mut <- lazyeval::interp(~ ifelse(test = grepl(s, c, ignore.case = TRUE),
                                   yes = "1",
                                   no = NA),
                          s = searchStr,
                          c = as.name(colName))
  cleanData <- cleanData %>%
    mutate_(.dots = setNames(list(mut), newCol))
  
  cleanData
}


# Title:
#   Helper Function
# Description:
#   Temporary function to use to check if regular expression is targeting the
#   rows we think it should be targeting.
# Input:
#   part      = dplyr data frame,
#   col       = column you want to target,
#   words     = string or vector of strings you want to search for,
#   printYes  = default to NA to just view data, set to 1 to print our data
#               frame, and set to anything else to count number of instances
# Usage:
#   > part <- part2
#   > col <- "MoneyForLearning"
#   > words <- c("[^0-9]")
#   > helper_filter(part, col, words)     # To View
#   > helper_filter(part, col, words, 1)  # To print data to console
#   > helper_filter(part, col, words)     # To print number of instances
helper_filter <- function(part, col, words, printYes = NA) {
  # Helper code to look at data being filtered to be changed
  columnToLookAt <- col # Column name you want to examine
  wordSearch <- words %>% # Array of regular expressions to search
    paste(collapse = "|")
  charIdx <- part %>% select_(columnToLookAt) %>%
    mutate_each(funs(grepl(wordSearch, ., ignore.case = TRUE))) %>%
    unlist(use.names = FALSE)
  if (is.na(printYes)) {
    part %>% filter(charIdx) %>% count_(columnToLookAt) %>% View
  } else if (printYes == 1) {
    part %>% filter(charIdx) %>% select_(columnToLookAt) %>%
      distinct() %>% as.data.frame
  } else {
    part %>% filter(charIdx) %>% count_(columnToLookAt) %>%
      summarise(total = sum(n))
  }
}


# Sub-Cleaning Functions ----------------------------------

# Title:
#   Clean Other Job Role Interest
# Description:
#   This function targets the other job interests people put down and performs
#   some cleaning:
#   - Normalize variants of "Undecided"
#   - Normalize variants of "Cyber Security"
#   - Normalize variants of "Game Developer"
#   - Normalize variants of "Software Engineer"
# Usage:
#   > cleanPart <- clean_job_interest_other(part)
clean_job_interest_other <- function(part) {
  cat("Cleaning responses for other job interests...\n")
  
  ## Title case answers for other job interests
  ##  See if I can simplify this by just mutating
  jobRoleOtherYes <- part %>% filter(!is.na(JobInterestOther)) %>%
    mutate(JobInterestOther = simple_title_case(JobInterestOther))
  jobRoleOtherNo <- part %>% filter(is.na(JobInterestOther))
  cleanPart <- jobRoleOtherNo %>% bind_rows(jobRoleOtherYes)
  
  ## Change uncertain job roles to "Undecided"
  undecidedWords <- c("not sure", "don't know", "not certain",
                      "unsure", "dont know", "undecided",
                      "all of the above", "no preference", "not",
                      "any", "no idea", "idk", "dunno")
  cleanPart <- normalize_text(inData = cleanPart,
                              columnName = "JobInterestOther",
                              searchTerms = undecidedWords,
                              replaceWith = "Undecided")
  
  cat("Finished cleaning responses for other job interests.\n")
  cleanPart
}


# Title:
#   Clean Expected Earnings
# Description:
#   For the expected earnings part of the survey, this function performs all
#   the necessary cleaning on that part of the data
clean_expected_earnings <- function(cleanPart1) {
  cat("Cleaning responses for expected earnings...\n")
  
  # Change all values to numeric for easier manipulation
  cleanPart1 <- cleanPart1 %>%
    mutate(ExpectedEarning = as.integer(ExpectedEarning))
  
  # Expected values < 19 set to NA
  # Too weird to be monthly income and too small for yearly
  below19 <- cleanPart1 %>%
    filter(ExpectedEarning < 19)
  change19 <- below19 %>%
    mutate(ExpectedEarning = NA)
  cleanPart1 <- cleanPart1 %>% setdiff(below19) %>% bind_rows(change19)
  
  # Multiply expected 20--200 by 1000
  # Too small for monthly, large enough to be annual if 1000x
  values20to200 <- cleanPart1 %>%
    filter(ExpectedEarning >= 20) %>%
    filter(ExpectedEarning <= 200)
  change20to200 <- values20to200 %>%
    mutate(ExpectedEarning = ExpectedEarning * 1000)
  cleanPart1 <- cleanPart1 %>% setdiff(values20to200) %>%
    bind_rows(change20to200)
  
  # Remove expected values 201--499
  # Too high for annual, too small for monthly
  values201to499 <- cleanPart1 %>%
    filter(ExpectedEarning >= 201) %>%
    filter(ExpectedEarning <= 499)
  change201to499 <- values201to499 %>%
    mutate(ExpectedEarning = NA)
  cleanPart1 <- cleanPart1 %>% setdiff(values201to499) %>%
    bind_rows(change201to499)
  
  # Multiply values 500--5999 by 12
  # Looks like monthly salary for poor and middle-rich countries
  values500to5999 <- cleanPart1 %>%
    filter(ExpectedEarning >= 500) %>%
    filter(ExpectedEarning <= 5999)
  change500to5999 <- values500to5999 %>%
    mutate(ExpectedEarning = ExpectedEarning * 12)
  cleanPart1 <- cleanPart1 %>% setdiff(values500to5999) %>%
    bind_rows(change500to5999)
  
  # Change to correct integers e.g. change 0000000 to just 0
  cleanPart1 <- cleanPart1 %>%
    mutate(ExpectedEarning = as.character(ExpectedEarning))
  
  cat("Finished cleaning responses for expected earnings.\n")
  cleanPart1
}


# Title:
#   Clean Code Events
# Description:
#   The function performs various transformations to coding event data to make
#   it consistent (e.g. fix spelling) and normalize instances of answers to be
#   the same. Also, new columns are made for mentions that appear more than
#   1.5% of all other code events.
# Note: honorable mentions to:
#   - "LaunchCode"
# Usage:
#   > cleanPart <- clean_code_events(cleanPart)
clean_code_events <- function(cleanPart1) {
  cat("Cleaning responses for coding events...\n")
  
  # Title case other coding events
  codingEvents <- cleanPart1 %>% filter(!is.na(CodeEventOther)) %>%
    mutate(CodeEventOther = simple_title_case(CodeEventOther))
  codeEventsElse <- cleanPart1 %>% filter(is.na(CodeEventOther))
  cleanPart1 <- codeEventsElse %>% bind_rows(codingEvents)
  
  # Normalize variations of "None"
  nones <- c("non", "none", "haven't", "havent", "not", "nothing",
             "didn't", "n/a", "\bna\b", "never", "nil", "nope")
  searchStr <- paste(nones, collapse = "|")
  nonesIdx <- cleanPart1 %>% select(CodeEventOther) %>%
    mutate_each(funs(grepl(searchStr, ., ignore.case = TRUE))) %>%
    unlist(use.names = FALSE)
  nonesData <- cleanPart1 %>% filter(nonesIdx) %>%
    mutate(CodeEventOther = NA) %>%
    mutate(CodeEventNone = "1")
  cleanPart1 <- cleanPart1 %>% filter(!nonesIdx) %>% bind_rows(nonesData)
  
  cat("Finished cleaning responses for coding events.\n")
  cleanPart1
}


# Title:
#   Clean Podcasts
# Description:
#   Cleans the podcasts answers in general but mostly cleans people's answers
#   for "Other". Performs the following:
#   - Convert Podcasts to binary/boolean
#   - Normalize variations of "None" in Podcast Other back to designated col
#   - Normalize variations of "Software Engineering Daily" in Podcast Other
#     back to designated column
#   - Add new columns for other podcasts greater than 1.5% of mentions in Other
#       - "Ruby Rogues"
#       - "Shop Talk Show"
#       - "Developer Tea"
#       - "Programming Throwdown"
#       - ".NET Rocks"
#       - "Talk Python to Me"
#       - "JavaScript Air"
#       - The Web Ahead"
#   - NOTE: honorable mentions to get their own column
#       - "Code Pen Radio"
#       - "Trav and Los"
#       - "Giant Robots Smashing into other Giant Robots"
clean_podcasts <- function(cleanPart) {
  cat("Cleaning responses for other podcasts...\n")
  
  # Normalize variations of "None" in PodcastOther
  nonePod <- c("non", "none", "haven't", "havent", "not a",
               "nothing", "didn't", "n/a", "\bna\b", "never",
               "\bnil\b", "nope", "not tried", "have not", "do not",
               "don't", "\bno\b", "\bno one\b", "iduntlitsentopodcasts",
               "does not", "no idea", "not applicable")
  searchStr <- paste(nonePod, collapse = "|")
  nonesPodIdx <- cleanPart %>% select(PodcastOther) %>%
    mutate_each(funs(grepl(searchStr, ., ignore.case = TRUE))) %>%
    unlist(use.names = FALSE)
  nonesPodData <- cleanPart %>% filter(nonesPodIdx) %>%
    mutate(PodcastOther = NA) %>%
    mutate(PodcastNone = "1")
  cleanPart <- cleanPart %>% filter(!nonesPodIdx) %>%
    bind_rows(nonesPodData)
  
  cat("Finished cleaning responses for other podcasts.\n")
  cleanPart
}


# Title:
#   Clean Mortgage Amount
# Usage:
#   > cleanPart <- clean_mortgage_amt(cleanPart)
clean_mortgage_amt <- function(cleanPart) {
  # Make values to integer so that it'll remove odd answers like 00000
  cleanPart <- cleanPart %>%
    mutate(HomeMortgageOwe = as.integer(HomeMortgageOwe))
  
  # Make minumum mortgage amount $1000
  cleanPart <- cleanPart %>%
    mutate(HomeMortgageOwe = ifelse(HomeMortgageOwe < 1000,
                                    yes = NA,
                                    no = HomeMortgageOwe))
  
  # Make maximum mortgage amount $1000000
  cleanPart <- cleanPart %>%
    mutate(HomeMortgageOwe = ifelse(HomeMortgageOwe > 1000000,
                                    yes = NA,
                                    no = HomeMortgageOwe))
  
  cleanPart
}


# Title:
#   Clean Income
# Usage:
#   > cleanPart <- clean_income(cleanPart)
clean_income <- function(cleanPart) {
  cat("Cleaning responses for income...\n")
  
  # Change all values to numeric for easier manipulation
  cleanPart <- cleanPart %>%
    mutate(Income = as.integer(Income))
  
  # Expected values < 19 set to NA
  # Too weird to be monthly income and too small for yearly
  below19 <- cleanPart %>%
    filter(Income < 19)
  change19 <- below19 %>%
    mutate(Income = NA)
  cleanPart <- cleanPart %>% setdiff(below19) %>% bind_rows(change19)
  
  # Multiply expected 20--200 by 1000
  # Too small for monthly, large enough to be annual if 1000x
  values20to200 <- cleanPart %>%
    filter(Income >= 20) %>%
    filter(Income <= 200)
  change20to200 <- values20to200 %>%
    mutate(Income = Income * 1000)
  cleanPart <- cleanPart %>% setdiff(values20to200) %>%
    bind_rows(change20to200)
  
  # Remove expected values 201--499
  # Too high for annual, too small for monthly
  values201to499 <- cleanPart %>%
    filter(Income >= 201) %>%
    filter(Income <= 499)
  change201to499 <- values201to499 %>%
    mutate(Income = NA)
  cleanPart <- cleanPart %>% setdiff(values201to499) %>%
    bind_rows(change201to499)
  
  # Multiply values 500--5999 by 12
  # Looks like monthly salary for poor and middle-rich countries
  values500to5999 <- cleanPart %>%
    filter(Income >= 500) %>%
    filter(Income <= 5999)
  change500to5999 <- values500to5999 %>%
    mutate(Income = Income * 12)
  cleanPart <- cleanPart %>% setdiff(values500to5999) %>%
    bind_rows(change500to5999)
  
  # Set limit to 1,000,000
  values1m <- cleanPart %>%
    filter(Income > 1000000)
  change1m <- values1m %>%
    mutate(Income = 1000000)
  cleanPart <- cleanPart %>% setdiff(values1m) %>%
    bind_rows(change1m)
  
  cat("Finished cleaning income.\n")
  cleanPart
}


# Title:
#   Clean Student Debt Amount
# Description:
#   Remove commas, set minimum to $1000, set maximum to $500000, and make
#   individuals' answers who have zero debt consistent with `HasStudentDebt`
# Usage:
#   cleanPart <- clean_student_debt(cleanPart)
clean_student_debt <- function(cleanPart) {
  cat("Cleaning responses for student debt owed...\n")
  
  # Make mimimum student debt to $100
  cleanPart <- cleanPart %>%
    mutate(StudentDebtOwe = ifelse(StudentDebtOwe < 100,
                                   yes = NA,
                                   no = StudentDebtOwe))
  
  # Make maximum student debt to $500000
  cleanPart <- cleanPart %>%
    mutate(StudentDebtOwe = ifelse(StudentDebtOwe > 500000,
                                   yes = NA,
                                   no = StudentDebtOwe))
  
  # Change back to character
  cleanPart <- cleanPart %>%
    mutate(StudentDebtOwe = as.character(StudentDebtOwe))
  
  cat("Finished cleaning responses for student debt owed.\n")
  cleanPart
}


# Title:
#   Clean Children
# Description:
#   Make sure there's consistency in answering you have no children. In other
#   words, if you answered "No" for not having children, you should not have a
#   response to how many children you have (i.e. NA).
# Usage:
#   cleanPart <- clean_children(cleanPart)
clean_children <- function(cleanPart) {
  cat("Cleaning responses for number of children...\n")
  
  naChildren <- cleanPart %>%
    filter(is.na(ChildrenNumber))
  children <- cleanPart %>%
    filter(!is.na(ChildrenNumber)) %>%
    mutate(HasChildren = ifelse(ChildrenNumber == 0, 0, HasChildren)) %>%
    mutate(ChildrenNumber =
             ifelse(ChildrenNumber == 0, NA, ChildrenNumber))
  cleanPart <- bind_rows(naChildren, children)
  
  # Remove outlier children
  cleanPart <- cleanPart %>%
    mutate(ChildrenNumber = ifelse(ChildrenNumber > 10, NA, ChildrenNumber))
  
  cat("Finished cleaning responses for number of children.\n")
  cleanPart
}


# Main Process Functions ----------------------------------
# Description:
#   These functions encompass the bulk work of the cleaning and transformation

# Title:
#   Clean Survey
# Usage:
#   > final <- clean_part(allData)
clean_part <- function(part) {
  cat("Beginning cleaning of data...\n")
  
  # Clean each column that needs it
  cleanPart <- clean_job_interest_other(cleanPart)  # Clean Job Role Interests
  cleanPart <- clean_expected_earnings(cleanPart)  # Clean expected earnings
  cleanPart <- clean_code_events(cleanPart)   # Clean other coding events
  cleanPart <- clean_podcasts(cleanPart)   # Clean Podcasts Other
  cleanPart <- clean_money_learning(cleanPart)  # Clean money for learning
  cleanPart <- clean_mortgage_amt(cleanPart)  # Clean mortgage amount
  cleanPart <- clean_income(cleanPart)  # Clean income
  cleanPart <- clean_student_debt(cleanPart)  # Clean student debt amount
  cleanPart <- clean_children(cleanPart)  # Clean children responses
  
  # Remove unnecessary columns
  cleanPart <- cleanPart %>%
    select(-OneTwoDiff, -Resources, -Podcast, -CodeEvent, -YouTube)
  
  cat("Finished cleaning survey data.\n")
  cleanPart
}

# Title:
#   Rename Survey Variables
# Usage:
#   > renamed_data <- rename_data_vars(dat)
rename_data_vars <- function(dat) {
  renamed_data <- dat %>%
    rename(
      is_software_dev = "Are you already working as a software developer?",
      is_first_dev_job = "Is this your first software development job?",
      months_job_search = "Before you got this job, how many months did you spend looking for a job?",
      job_pref = "Would you prefer to...",

      # Job interests
      job_intr_fllstck = "Full-Stack Web Developer",
      job_intr_backend = "Back-End Web Developer",
      job_intr_frntend = "Front-End Web Developer",
      job_intr_mobile = "Mobile Developer",
      job_intr_devops = "DevOps / SysAdmin",
      job_intr_datasci = "Data Scientist",
      job_intr_teacher = "Teacher / Trainer / Developer Evangelist",
      job_intr_qa_engn = "Quality Assurance Engineer",
      job_intr_ux_engn = "User Experience Designer",
      job_intr_projm = "Product Manager",
      job_intr_gamedev = "Game Developer",
      job_intr_infosec = "Information Security",
      job_intr_dataengn = "Data Engineer",
      job_intr_other = "Other",

      when_appl_job = "When do you plan to start applying for developer jobs?",
      expected_earn = "About how much money do you expect to earn per year at your first developer job (in US Dollars)?",
      job_lctn_pref = "Would you prefer to work...",
      job_relocate = "Are you willing to relocate for a job?",

      # Reasons to code
      reasons_to_code = "What is your biggest reason for learning to code?",
      reasons_to_code_other = "Other_1", # Very inspiring column to read

      # Learning resources
      rsrc_fcc = "freeCodeCamp",
      rsrc_mdn = "Mozilla Developer Network (MDN)",
      rsrc_so = "Stack Overflow",
      rsrc_edx = "EdX",
      rsrc_coursera = "Coursera",
      rsrc_khan_acdm = "Khan Academy",
      rsrc_pluralsght = "Pluralsight",
      rsrc_codeacdm = "Codecademy",
      rsrc_udacity = "Udacity",
      rsrc_udemy = "Udemy",
      rsrc_code_wars = "Code Wars",
      rsrc_treehouse = "Treehouse",
      rsrc_hackerrank = "HackerRank",
      rsrc_frntendmstr = "Front End Masters",
      rsrc_lynda = "Lynda.com",
      rsrc_egghead = "Egghead.io",
      rsrc_css_tricks = "CSS Tricks",
      rsrc_other = "Other_2",

      # Coding events attended
      codeevnt_fcc = "freeCodeCamp study groups",
      codeenvt_hackthn = "hackathons",
      codeenvt_confs = "conferences",
      codeenvt_workshps = "workshops",
      codeenvt_startupwknd = "Startup Weekend",
      codeenvt_nodeschl = "NodeSchool",
      codeenvt_womenwc = "Women Who Code",
      codeenvt_girldevit = "Girl Develop It",
      codeenvt_coderdojo = "CoderDojo",
      codeenvt_meetup = "Meetup.com events",
      codeenvt_railsbrdg = "RailsBridge",
      codeenvt_gamejam = "Game Jam",
      codeenvt_railsgrls = "Rails Girls",
      codeenvt_djangogrls = "Django Girls",
      codeenvt_wkndbtcmp = "weekend bootcamps",
      codeenvt_other = "Other_3",

      # Podcasts listened to
      podcast_fcc = "The freeCodeCamp Podcast",
      podcast_codenewbie = "Code Newbie",
      podcast_changelog = "The Changelog",
      podcast_sedaily = "Software Engineering Daily",
      podcast_js_jabber = "JavaScript Jabber",
      podcast_syntaxfm = "Syntax.fm",
      podcast_ltcwm = "Learn To Code With Me",
      podcast_fullstckrd = "Full Stack Radio",
      podcast_frnthppyhr = "Front End Happy Hour",
      podcast_codingblcks = "Coding Blocks",
      podcast_shoptalk = "Shop Talk Show",
      podcast_devtea = "Developer Tea",
      podcast_progthrwdwn = "Programming Throwdown",
      podcast_geekspeak = "Geek Speak",
      podcast_hanselmnts = "Hanselminutes",
      podcast_talkpythonme = "Talk Python To Me",
      podcast_rubyrogues = "Ruby Rogues",
      podcast_codepenrd = "CodePen Radio",
      podcast_seradio = "Software Engineering Radio",
      podcast_other = "Other_4",

      # YouTube channels
      yt_mit_ocw = "MIT Open Courseware",
      yt_fcc = "freeCodeCamp's YouTube channel",
      yt_computerphile = "Computerphile",
      yt_devtips = "DevTips",
      yt_cs_dojo = "CS Dojo",
      yt_engn_truth = "Engineered Truth",
      yt_learncodeacdm = "LearnCode.Academy",
      yt_lvluptuts = "LevelUpTuts",
      yt_funfunfunct = "Fun Fun Function",
      yt_codingtuts360 = "Coding Tutorials 360",
      yt_codingtrain = "Coding Train",
      yt_derekbanas = "Derek Banas",
      yt_simplilearn = "Simplilearn",
      yt_simpleprog = "Simple Programmer (Bulldog Mindset)",
      yt_mozillahacks = "Mozilla Hacks",
      yt_googledevs = "Google Developers",
      yt_other = "Other_5",

      # Learning information
      hours_learning = "About how many hours do you spend learning each week?",
      months_programming = "About how many months have you been programming for?",

      # Bootcamps
      bootcamp_attend = "Have you attended a full-time coding bootcamp?",
      bootcamp_name = "Which one?",
      bootcamp_finished = "Have you finished yet?",
      bootcamp_have_loan = "Did you take out a loan to pay for the bootcamp?",
      bootcamp_recommend = "Based on your experience, would you recommend this bootcamp to your friends?",

      money_for_learning = "Aside from university tuition, about how much money have you spent on learning to code so far (in US dollars)?",
      age = "How old are you?",

      # Individual's gender
      gender = "What's your gender?",
      gender_other = "Other_6",

      # Demographics
      country_citizen = "Which country are you a citizen of?",
      country_live = "Which country do you currently live in?",
      live_city_population = "About how many people live in your city?",
      is_ethnic_minority = "Are you an ethnic minority in your country?",
      lang_at_home = "Which language do you you speak at home with your family?",

      # Education
      school_degree = "What's the highest degree or level of school you have completed?",
      school_major = "What was the main subject you studied in university?",

      # Personal and family information
      marital_status = "What's your marital status?",
      has_finance_depends = "Do you financially support any dependents?",
      has_children = "Do you have children?",
      num_children = "How many children do you have?",
      do_finance_support = "Do you financially support any elderly relatives or relatives with disabilities?",
      debt_amt = "Do you have any debt?",
      home_mrtg_has = "Do you have a home mortgage?",
      home_mrtg_owe = "About how much do you owe on your home mortgage (in US Dollars)?",
      student_debt_has = "Do you have student loan debt?",
      student_debt_amt = "About how much do you owe in student loans (in US Dollars)?",

      # Employment status information
      curr_emplymnt = "Regarding employment status, are you currently...",
      curr_emplymnt_other = "Other_7",
      curr_field = "Which field do you work in?",
      last_yr_income = "About how much money did you make last year (in US dollars)?",
      communite_time = "About how many minutes does it take you to get to work each day?",
      is_self_employed = "Do you consider yourself under-employed?",
      has_served_military = "Have you served in your country's military before?",
      is_recv_disab_bnft = "Do you receive disability benefits from your government?",
      has_high_spd_ntnet = "Do you have high speed internet at your home?",

      # Miscellaneous
      time_start = "Start Date (UTC)",
      time_end = "Submit Date (UTC)",
      network_id = "Network ID"
    )
  renamed_data
}


# Title:
#   Standardize Data Types
# Description:
#   There were some inconsistent encoding of data between the two datasets.
#       - Values passed from first part of survey were passed in as strings. So
#         empty answers were passed on as "undefined"
#       - The second dataset encoded yes/no answers as those strings, versus
#         the first dataset encoding it as 1/0
#       - For the question on when you plan on applying for jobs, some of the
#         answers were truncated at the apostrophe, so the answers were fixed
#         to what they were.
#       - Lastly, some of the numeric values were read into R as numeric, but
#         some were read in as double. So these numeric data types were
#         standardized to either character or double for ease of use later.
std_data_type <- function(part1, part2) {
  cat("Standardizing variables between data for joining...\n")
  
  # Change "Yes"/"No" to "1"/"0"
  changeCols <- c("IsSoftwareDev", "JobRelocate", "BootcampYesNo",
                  "BootcampFinish", "BootcampLoan", "BootcampRecommend")
  part2 <- yesNo_to_oneZero(part2, changeCols)
  
  # Fix truncated answers in when you're applying to jobs question
  part2 <- part2 %>%
    mutate(JobApplyWhen = fix_truncate_job_apply(JobApplyWhen))
  
  # Standardize data types between data sets to allow joining
  toChr <- c("IsSoftwareDev", "JobRelocate", "BootcampYesNo",
             "BootcampFinish", "BootcampLoan", "BootcampRecommend",
             "ExpectedEarning", "MonthsProgramming", "MoneyForLearning",
             "HoursLearning")
  part1 <- change_to_chr(part1, toChr)
  
  cat("Finished standardizing variables between data.\n")
  list(part1 = part1, part2 = part2)
}


# Title:
#   Joining Data Sets (WIP)
# Description:
#   Uses shared information in the columns between the data sets to join them
#   together
join_data <- function(part1, part2) {
  # Different shared columns between the two parts
  sharedCols1 <- c("JobInterestOther", "ResourceOther", "CodeEventOther",
                   "PodcastOther", "YouTubeOther")
  sharedCols2 <- c("JobRoleInterest")
  
  # Static answers for job interest
  jobInterests <- c("Full-Stack Web Developer", "Back-End Web Developer",
                    "Front-End Web Developer", "Mobile Developer",
                    "DevOps / SysAdmin", "Data Scientist",
                    "Quality Assurance Engineer", "User Experience Designer",
                    "Project Manager", "Game Developer",
                    "Information Security", "Data Engineer")
  part2 %>% separate_rows(sharedCols2[1], sep = ",")
}


# Title:
#   Survey Parts Sanity Check
# Description:
#   After joining the two datasets together, there were some inconsistencies
#   with the survey times starting and ending. For example, for some joined
#   rows, the first part end time was more than 5 minutes before the second
#   part of the survey started.
#
#   This function first changes the Part 1 end time and the Part 2 start time
#   to the appropriate date datatype before manipulating them.
#
#   Cases:
#       - Remove negative time differences where 2nd part of survey started
#         before the 1st part ended
#       - Multiple first part ID's used in conjunction with second part ID's
#       - Multiple second part ID's used in conjunction with first part ID's
# Input:
#   dplyr data frame with joined datasets
# Output:
#   dplyr data frame with joined datasets
# Usage:
#   > allData <- time_diff_check(allData)
time_diff_check <- function(allData) {
  cat("Checking for inconsistencies within survey after joining...\n")
  
  # Change data type to date so we can easily compare times
  newData <- allData %>%
    mutate(Part1EndTime = as.POSIXct(Part1EndTime)) %>%
    mutate(Part2StartTime = as.POSIXct(Part2StartTime)) %>%
    mutate(OneTwoDiff = Part2StartTime - Part1EndTime)
  newData <- newData %>%
    select(noquote(order(colnames(newData))))
  
  # Separate data to focus on ones that we can look at differences
  newDataNA <- newData %>% filter(is.na(OneTwoDiff))
  newDataData <- newData %>% filter(!is.na(OneTwoDiff))
  
  # Remove neg times i.e. 2nd part of survey started before 1st part ended
  newDataData <- newDataData %>% filter(OneTwoDiff > 0)
  
  # Check for multiple first ID's put to second part ID's
  id1Unique <- newDataData %>% group_by(ID.x) %>%
    filter(n() == 1)
  id1Good <- newDataData %>% group_by(ID.x) %>%
    filter(n() > 1) %>%
    mutate(minDiff = min(OneTwoDiff)) %>%
    filter(OneTwoDiff == minDiff)
  newDataData <- bind_rows(id1Unique, id1Good) %>% select(-one_of("minDiff"))
  
  # Check for multiple second ID's put to first part ID's
  id2Unique <- newDataData %>% group_by(ID.y) %>%
    filter(n() == 1)
  id2Good <- newDataData %>% group_by(ID.y) %>%
    filter(n() > 1) %>%
    mutate(minDiff = min(OneTwoDiff)) %>%
    filter(OneTwoDiff == minDiff)
  newDataData <- bind_rows(id2Unique, id2Good) %>% select(-one_of("minDiff"))
  
  # Join two pieces back together
  newData <- bind_rows(newDataData, newDataNA)
  
  # Ungroup data from grouping (group_by()) done above
  newData <- newData %>% ungroup()
  
  cat("Finished checking inconsistencies within survey after joining.\n")
  newData
}


# Title:
#   Final Polish
# Description:
#   This function does some final polishing like arranging the columns, making
#   sure the columns are of a certain type, and renaming columns.
# Usage:
#   > final <- polish_data(cleanData)
polish_data <- function(cleanData) {
  cat("Putting on finishing touches...\n")
  
  # Rename and confirm desired data type
  cleanData <- cleanData %>%
    mutate(Age = as.integer(Age)) %>%
    mutate(BootcampFinish = as.integer(BootcampFinish)) %>%
    mutate(BootcampLoan = as.integer(BootcampLoan)) %>%
    rename(BootcampLoanYesNo = BootcampLoan) %>%
    mutate(BootcampRecommend = as.integer(BootcampRecommend)) %>%
    mutate(BootcampYesNo = as.integer(BootcampYesNo)) %>%
    rename(AttendedBootcamp = BootcampYesNo) %>%
    mutate(ExpectedEarning = as.integer(ExpectedEarning)) %>%
    rename(HasDebt = DebtAmount) %>%
    mutate(Income = as.integer(Income)) %>%
    mutate(IsSoftwareDev = as.integer(IsSoftwareDev)) %>%
    rename(JobRelocateYesNo = JobRelocate) %>%
    mutate(JobRelocateYesNo = as.integer(JobRelocateYesNo)) %>%
    mutate(MoneyForLearning = as.integer(MoneyForLearning)) %>%
    mutate(StudentDebtOwe = as.integer(StudentDebtOwe)) %>%
    mutate(MonthsProgramming = as.integer(MonthsProgramming)) %>%
    mutate(HoursLearning = as.integer(HoursLearning))
  
  # Polish Job Interests
  cleanData <- cleanData %>%
    mutate(JobInterestBackEnd = as.integer(JobInterestBackEnd)) %>%
    mutate(JobInterestDataEngr = as.integer(JobInterestDataEngr)) %>%
    mutate(JobInterestDataSci = as.integer(JobInterestDataSci)) %>%
    mutate(JobInterestDevOps = as.integer(JobInterestDevOps)) %>%
    mutate(JobInterestFrontEnd = as.integer(JobInterestFrontEnd)) %>%
    mutate(JobInterestFullStack = as.integer(JobInterestFullStack)) %>%
    mutate(JobInterestGameDev = as.integer(JobInterestGameDev)) %>%
    mutate(JobInterestInfoSec = as.integer(JobInterestInfoSec)) %>%
    mutate(JobInterestMobile = as.integer(JobInterestMobile)) %>%
    mutate(JobInterestProjMngr = as.integer(JobInterestProjMngr)) %>%
    mutate(JobInterestQAEngr = as.integer(JobInterestQAEngr)) %>%
    mutate(JobInterestUX = as.integer(JobInterestUX))
  
  # Polish Code Events
  cleanData <- cleanData %>%
    mutate(CodeEventConferences = as.integer(CodeEventConferences)) %>%
    mutate(CodeEventFCC = as.integer(CodeEventFCC)) %>%
    mutate(CodeEventGirlDev = as.integer(CodeEventGirlDev)) %>%
    mutate(CodeEventHackathons = as.integer(CodeEventHackathons)) %>%
    mutate(CodeEventNodeSchool = as.integer(CodeEventNodeSchool)) %>%
    mutate(CodeEventNone = as.integer(CodeEventNone)) %>%
    mutate(CodeEventRailsBridge = as.integer(CodeEventRailsBridge)) %>%
    mutate(CodeEventStartUpWknd = as.integer(CodeEventStartUpWknd)) %>%
    mutate(CodeEventWomenCode = as.integer(CodeEventWomenCode)) %>%
    mutate(CodeEventMeetup = as.integer(CodeEventMeetup)) %>%
    mutate(CodeEventWkdBootcamps = as.integer(CodeEventWkdBootcamps)) %>%
    mutate(CodeEventRailsGirls = as.integer(CodeEventRailsGirls)) %>%
    mutate(CodeEventDjangoGirls = as.integer(CodeEventDjangoGirls)) %>%
    mutate(CodeEventGameJam = as.integer(CodeEventGameJam)) %>%
    mutate(CodeEventWorkshops = as.integer(CodeEventWorkshops))
  
  # Polish Resources
  cleanData <- cleanData %>%
    mutate(ResourceCodeWars = as.integer(ResourceCodeWars)) %>%
    mutate(ResourceCodecademy = as.integer(ResourceCodecademy)) %>%
    mutate(ResourceCoursera = as.integer(ResourceCoursera)) %>%
    mutate(ResourceCSS = as.integer(ResourceCSS)) %>%
    mutate(ResourceEdX = as.integer(ResourceEdX)) %>%
    mutate(ResourceFCC = as.integer(ResourceFCC)) %>%
    mutate(ResourceKA = as.integer(ResourceKA)) %>%
    mutate(ResourceOdinProj = as.integer(ResourceOdinProj)) %>%
    mutate(ResourcePluralSight = as.integer(ResourcePluralSight)) %>%
    mutate(ResourceUdacity = as.integer(ResourceUdacity)) %>%
    mutate(ResourceUdemy = as.integer(ResourceUdemy)) %>%
    mutate(ResourceTreehouse = as.integer(ResourceTreehouse)) %>%
    mutate(ResourceLynda = as.integer(ResourceLynda)) %>%
    mutate(ResourceSO = as.integer(ResourceSO)) %>%
    mutate(ResourceW3S = as.integer(ResourceW3S)) %>%
    mutate(ResourceSkillcrush = as.integer(ResourceSkillcrush)) %>%
    mutate(ResourceHackerRank = as.integer(ResourceHackerRank)) %>%
    mutate(ResourceMDN = as.integer(ResourceMDN)) %>%
    mutate(ResourceEgghead = as.integer(ResourceEgghead))
  
  # Polish Podcasts
  cleanData <- cleanData %>%
    mutate(PodcastChangeLog = as.integer(PodcastChangeLog)) %>%
    mutate(PodcastCodeNewbie = as.integer(PodcastCodeNewbie)) %>%
    mutate(PodcastCodePen = as.integer(PodcastCodePen)) %>%
    mutate(PodcastDevTea = as.integer(PodcastDevTea)) %>%
    mutate(PodcastDotNET = as.integer(PodcastDotNET)) %>%
    mutate(PodcastGiantRobots = as.integer(PodcastGiantRobots)) %>%
    mutate(PodcastJSAir = as.integer(PodcastJSAir)) %>%
    mutate(PodcastJSJabber = as.integer(PodcastJSJabber)) %>%
    mutate(PodcastNone = as.integer(PodcastNone)) %>%
    mutate(PodcastRubyRogues = as.integer(PodcastRubyRogues)) %>%
    mutate(PodcastProgThrowdown = as.integer(PodcastProgThrowdown)) %>%
    mutate(PodcastSEDaily = as.integer(PodcastSEDaily)) %>%
    mutate(PodcastSERadio = as.integer(PodcastSERadio)) %>%
    mutate(PodcastShopTalk = as.integer(PodcastShopTalk)) %>%
    mutate(PodcastTalkPython = as.integer(PodcastTalkPython)) %>%
    mutate(PodcastTheWebAhead = as.integer(PodcastTheWebAhead))
  
  # Polish YouTube Channels
  cleanData <- cleanData %>%
    mutate(YouTubeCodeCourse = as.integer(YouTubeCodeCourse)) %>%
    mutate(YouTubeCodingTrain = as.integer(YouTubeCodingTrain)) %>%
    mutate(YouTubeCodingTut360 = as.integer(YouTubeCodingTut360)) %>%
    mutate(YouTubeComputerphile = as.integer(YouTubeComputerphile)) %>%
    mutate(YouTubeDerekBanas = as.integer(YouTubeDerekBanas)) %>%
    mutate(YouTubeDevTips = as.integer(YouTubeDevTips)) %>%
    mutate(YouTubeEngineeredTruth = as.integer(YouTubeEngineeredTruth)) %>%
    mutate(YouTubeFCC = as.integer(YouTubeFCC)) %>%
    mutate(YouTubeFunFunFunction = as.integer(YouTubeFunFunFunction)) %>%
    mutate(YouTubeGoogleDev = as.integer(YouTubeGoogleDev)) %>%
    mutate(YouTubeLearnCode = as.integer(YouTubeLearnCode)) %>%
    mutate(YouTubeLevelUpTuts = as.integer(YouTubeLevelUpTuts)) %>%
    mutate(YouTubeMIT = as.integer(YouTubeMIT)) %>%
    mutate(YouTubeMozillaHacks = as.integer(YouTubeMozillaHacks)) %>%
    mutate(YouTubeSimplilearn = as.integer(YouTubeSimplilearn)) %>%
    mutate(YouTubeTheNewBoston = as.integer(YouTubeTheNewBoston))
  
  # Order columns alphabetically
  cleanData <- cleanData %>% select(noquote(order(colnames(cleanData))))
  
  cat("Finished last polish of data.\n")
  cleanData
}


# Main Function -------------------------------------------

# Title:
#   Main Cleaning Function
# Description:
#   This is the main cleaning and transformation function. It will write a new
#   file in the `clean-data/` directory.
# Usage:
#   > main()
main <- function(dataPath1, dataPath2) {
  # Read in data
  data_path <- here("raw-data", "2018-New-Coders-Survey.csv")
  dat <- data_path %>%
    read_csv() %>%
    rename(ID = "#")

  # Rename variables with easier names
  renamed_data = rename_data_vars(dat)

  # Change variables (jobs,rsrc,codeevnt,podcast,yt) to boolean
  bool_changed_data <- renamed_data %>%
    # Change job interest
    mutate_at(vars(starts_with("job_intr_"), -job_intr_other),
              vchar_to_one) %>%

    # Change resources
    mutate_at(vars(starts_with("rsrc_"), -rsrc_other),
              vchar_to_one) %>%

    # Change coding events
    mutate_at(vars(starts_with("codeenvt_"), -codeenvt_other),
              vchar_to_one) %>%

    # Change podcasts
    mutate_at(vars(starts_with("podcast_"), -podcast_other),
              vchar_to_one) %>%

    # Change YouTube channels
    mutate_at(vars(starts_with("yt_"), -yt_other),
              vchar_to_one)

  # Remove outliers for age, but keep NA values
  # Oldest living is 116, so filtering on that age
  # https://en.wikipedia.org/wiki/List_of_the_oldest_living_people
  age_outlier_removed <- bool_changed_data %>%
    filter(age < 116 | is.na(age))

  # Remove questionable months learning by cross-checking age
  # Here, convert age to months and take the difference between age
  # and months programming. Here, remove entries that claim you've
  # programmed more than you've been alive.
  # Counts:
  # - 5 years =  60
  # - 10 years = 120
  # - 50 years = 600
  age_checked_learning <- age_outlier_removed %>%
    mutate(months_age = age * 12) %>%
    mutate(prog_age_diff = months_age - months_programming) %>%
    filter(prog_age_diff > 0) %>%
    select(-c(prog_age_diff, months_age))

  # Remove excess money for learning
  # Keep NA values as well
  remove_excess_money_spent <- age_checked_learning %>%
    filter(money_for_learning < 250000 |
             is.na(money_for_learning))

  # Remove high number of children
  rm_high_num_kids <- remove_excess_money_spent %>%
    filter(num_children < 20)

  # Remove irrelevant values for others
  # - job interest
  # - resource
  # - coding events
  # - podcast
  # - YouTube
  # - gender
  # - employment status
  # Remove high number of children

  # Remove survey-year specific outliers
  allXs <- part2 %>% filter(ExpectedEarning == "xxxxx")
  part2 <- part2 %>% setdiff(allXs)

  # Make variables between datasets consistent for joining
  consistentData <- std_data_type(part1, part2)

  # Clean both parts of the data
  cleanData <- clean_part(allData)

  # Polish data with small changes e.g. give correct data types to columns
  final <- polish_data(cleanData)

  # Combine data and create cleaned data
  out_path <- here("clean-data", "2018-fCC-New-Coders-Survey-Data.csv")
  write_csv(x = final, path = out_path)

  write.csv(x = final,
            file = "clean-data/2017-fCC-New-Coders-Survey-Data.csv",
            na = "NA",
            row.names = FALSE)
}