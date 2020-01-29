# Homework 8                                              Data Programming in R
# Due by 6:00 PM on Tue Oct 31              Business Analytics Graduate Program
# via GitHub                                  MSCI:6060 Fall 2017 (Quad Cities)

###############################################################################
#                                                                             #
#                               INSTRUCTIONS                                  #
#                                                                             #
###############################################################################

# This homework corresponds to the Oct 17 class; please refer to
# the corresponding course materials. Please also follow all of the
# guidelines given on prior homeworks, referring to them if necessary.

###############################################################################
#                                                                             #
#                                EXERCISES                                    #
#                                                                             #
###############################################################################

# Clear workspace

rm(list = ls())

###############################################################################

# 1. When processing text data, it is often important to recognize the
# presence of certain keywords. Write a function that takes a character
# string as input and returns a vector of the days of the week that are
# found in the character string. The function should have the following
# characteristics:
#
#     Its name is "days.found".
#
#     It has exactly one formal argument called string with default
#     value "".
#
#     It returns a vector of class character. Note that the function
#     character(...) can be used to create a character vector. More
#     generally, vector(...) can be used to create a "blank" vector of
#     any class and any length.
#
#     If more than one day of the week is found, then the return vector
#     contains all of the days found in the typical order, i.e.,
#     "Sunday" would come before "Monday", which would come before
#     "Tuesday", and "Saturday" would come last. Note that the days
#     returned should have full spelling and proper capitalization.
#
#     If no matching days are found, then it returns a vector of length
#     zero.
#
#     If multiple instances of the same day of the week are found, then
#     it only returns that day of the week once.
#
#     Its search is case insensitive.
#
#     It does not match partial days of the week, e.g., "Sat" and
#     "thurs" would *not* be matches for "Saturday" and "Thursday". It
#     does not match misspellings.
#
# Some examples of the function's output are included in the file
# examples.html.


# Build function to determine days mentioned in a string

days.found <- function(string = "") {

    # Setup days vector (in typical order)

    days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
        "Friday", "Saturday")

    # Setup vector to return

    found <- character()

    # Loop over days vector

    for(day in days) {

        # Use grepl to determine if day occurs in string (case insensitive)

        result <- grepl(pattern = tolower(day), x = tolower(string),
            fixed = TRUE)

        # Add to found if day occurs

        if(result == TRUE) found <- c(found, day)
    }

    # Return

    found
        
}


###############################################################################

# 2. The CSV file babynames.csv contains 258,000 rows of data for the
# four following variables (note the capitalization):
#
#     NAME = first name, e.g., "Emma" or "John"
#
#     SEX = sex associated with that first name, i.e., "boy" or "girl"
#
#     YEAR = calendar year
#
#     RANK = integer from 1 to 1,000 indicating the rank (or popularity)
#     of the first name for babies of that sex in that year. For
#     example, in row 257001, we see that Emma was the most popular name
#     for girls in 2008, and in row 221000, we see that Tate was the
#     1000th most popular name for boys in 1990.
#
# Write a function that takes a name and a sex and returns a ggplot2
# scatter plot of RANK against YEAR for just that name and sex. The
# function should have the following characteristics:
#
#     The data babynames.csv is read outside and before the function
#     definition.
#    
#     The function name is "rank.plot".
#    
#     It has exactly two formal arguments called name and sex with
#     default values "Emma" and "girl", respectively. Note the
#     capitalization of name and sex.
#    
#     It returns the plot object. It does *not* explicitly print (or
#     show) the plot itself, although R might "autoprint" the plot
#     object when the function returns.
#    
#     It matches the values of name and sex exactly, i.e., case
#     sensitive and no partial matching.
#    
#     If there are no exact matches to name and sex, then an empty plot
#     is still returned.
#
# Some examples of the function's output are included in the file
# examples.html.


# Load graphics library

library(ggplot2)

# Load data set

df <- read.csv("babynames.csv", stringsAsFactors = FALSE)

# Build function to plot rank of baby name rank

rank.plot <- function(name = "Emma", sex = "girl") {

    # Subset the data frame by name and sex

    df <- subset(df, NAME == name & SEX == sex)

    # Create plot

    p <- qplot(x = YEAR, y = RANK, data = df, geom = "point")

    # Return plot

    p

}

