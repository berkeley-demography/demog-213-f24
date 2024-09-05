## title: Some tips for reading and managing big datasets into R
## author: Joshua R. Goldstein

## - how big is big data? what have you used?
## - why worry about how long it will take, since you only have to read in once, right?
## - how about just selecting variables we want?
## - how about just selecting a sample of the data?

## Our case study:

## The Berkeley Unified Numident Mortality Database (BUNMD) has 10s of
## millions of individual death records, with names and social
## security numbers and dates of birth and dates of death.

## It's great for doing individual-level mortality analysis,
## particularly for looking at small groups.

## It's been linked to the 1940 census -- but here we'll just used the
## unlinked file which has more records but fewer variables.

## For our demo, we'll try to find out whether people who applied for
## Social Security cards using nicknames (a proxy for lower social
## class?) lived shorter lives.  Specifically, we're going to compare
## "JOSH" to "JOSHUA". Often you don't want to analyze the whole
## dataset -- just a select part.



## our plan

## 0. Prelimaries: setting things up

## 1. Explore file without reading whole thing in

## 2. Try reading whole thing in

## 3. Setting up a database and just read in what we want

## 4. Conclusions?

## 5. Things to try

#######################################
## 0. Prelimaries: setting things up ##
#######################################


## packages (install if you don't have them)
library(data.table) ## a workhorse for working with large data sets
library(DBI) ## database tools for creating and reading

## download data from internet
## www.censoc.org --> harvard data.verse

## get in the right directory
setwd("~/Downloads/bunmd_v2")
## change on your machine if needed
filename <- "bunmd_v2.csv"

## create database for later

create_sqlite <- function(csv.name, ...)
{
    ## Note: I think you may have to have R session running in the directory
    ## where you have the data for this to work well

    ## read in data
    dt <- fread(csv.name)
    ## set names for database and table
    db.name <- gsub("\\.csv", ".sqlite", csv.name) # foo.sqlite
    table.name <- gsub("\\.csv", "", csv.name) ## foo
    ## connect to data.base
    mydb <- DBI::dbConnect(RSQLite::SQLite(), db.name)
    ## write the data to database
    DBI::dbWriteTable(mydb, table.name, dt, ...)
    ## close connection
    DBI::dbDisconnect(mydb)
    return(NULL)
}



## The following just need to be run once
## so it's commented out, in case we want to source() this file
## create the database --
## ---
## system.time(
##     create_sqlite(filename)
##     )
## ---
## took 2 or 3 minutes

## check to see if database ok
db <- dbConnect(RSQLite::SQLite(), "bunmd_v2.sqlite")
dbListTables(db)
## should return
## [1] "bunmd_v2"

##############################################################
## 1. Explore file on system without reading whole thing in ##
##############################################################

## let's find out something more about our data

## 1. How big is it?
file.size(filename)
system(paste("ls -hl", filename)) ## human readable size
system(paste("wc -l", filename)) ## lines in dataset
## how many lines? (5 million, 50 million?)

## 2. What does it look like?

## raw data (extracting just first 5 lines and displaying)
system("head -5 bunmd_v2.csv > bunmd_v2_head.csv")
system("cat bunmd_v2_head.csv")



## using base-R read.csv()
head.df <- read.csv("bunmd_v2_head.csv")


## using data.table
head.dt <- data.table::fread(filename, nrows = 5)
dim(head.dt) ## how many variables?

## 3. Brute force

## Note: to avoid taking a super long-time, using base-R "read.table"
## I'm only going to read in 1million lines

## not run this
## system.time(
##     df <- read.csv(filename)
## )
## rm(df)
## gc() ## cleans memory up ("garbage collection")
## took 7 minutes for me


## 4. fread()

## ok, to try this
system.time(
    dt <- fread(filename)
)
rm(dt)
## should take a bit over a minute on my new laptop


## 5. fread() with just some variables

## for our analysis, we just want first names, byear, and death_age
## (and we'll add race)

## so we want fname, byear, race_last, bpl_string, and death_age

myvars = c("fname", "byear", "race_last", "death_age", "ssn")


system.time(
   select.dt <- fread(filename, select = myvars)
)
## ok, that seemed to take about half the time ...

## not sure if it scales with number of variables



## we can extract just the records we want in
## data.table syntax (which is very SQL-like)
josh_1.dt <- select.dt[fname %in% c("JOSH", "JOSHUA")]
dim(josh_1.dt) ## about 10k records

#######################################################################
## 5. data base approach: create a relational database and only read ##
## in what we want                                                   ##
#######################################################################

db <- dbConnect(RSQLite::SQLite(), "bunmd_v2.sqlite")

system.time(
    df <- DBI::dbGetQuery(db,
                          "SELECT *
                           FROM bunmd_v2
                           WHERE fname IN ('JOSH','JOSHUA')")
)
## just a few secs


## convert to data.table (if you want)

josh_2.dt <- data.table(df)

identical(josh_1.dt, josh_2.dt)

dim(josh_1.dt)
## [1] 10577     5
dim(josh_2.dt)
## [1] 10577    35


## let's see who lives longer using regression
josh.dt <- josh_2.dt

m_bad <- lm(death_age ~ fname, data = josh.dt)
## should give same answer as
## josh.dt[, mean(death_age), by = fname]
## now control for year of birth
m_better <- lm(death_age ~ fname + as.factor(byear), data = josh.dt)
m_even_better <- lm(death_age ~ fname + as.factor(byear), data = josh.dt,
                    subset = byear %in% 1910:1920)

## regression table output
library(memisc)
mtable(m_bad, m_better, m_even_better, digits = 1)


######################
## 4. Conclusions?  ##
######################

######################
## 5. Things to try ##
######################

## a. Come up with two other names and compare those (maybe nickname and fullname versions of some other name)

## b. Rerun "Josh vs. Joshua" analysis just for people born in New York

## c. Try vroom() or readr()?



