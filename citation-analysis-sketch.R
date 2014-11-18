# sources:
# http://www.jgoodwin.net/?p=1223
# http://orgtheory.wordpress.com/2012/05/16/the-fragile-network-of-econ-soc-readings/
# http://nealcaren.web.unc.edu/a-sociology-citation-network/
# http://kieranhealy.org/blog/archives/2014/11/15/top-ten-by-decade/
# http://www.jgoodwin.net/lit-cites.png

# go to http://apps.webofknowledge.com.offcampus.lib.washington.edu/
# refine search by journal... perhaps archaeolog* OR antiquity OR archeolog* in 'publication title', 
# 
# hit 'search'
# must have <10k results to enable citation data
# click 'create citation report' tab at the top

####vignette('RSelenium-basics')
# setup broswer and selenium
library(RSelenium)
checkForServer()
startServer()
remDr <- remoteDriver()
remDr$open()
# go to http://apps.webofknowledge.com.offcampus.lib.washington.edu/
# refine search by journal... perhaps arch?eolog* in 'publication title'
# then: 'Research Areas' -> archaeology -> refine
# then: 'Document types' -> article 
# must have <10k results to enable citation data
# click 'create citation report' tab at the top
# do the first page manually to set the 'save file' and 'do this automatically', 
# then let loop do the work after that

# before running the loop, get URL of first page that we already saved,
# and paste in next line
remDr$navigate("http://apps.webofknowledge.com/CitationReport.do?product=UA&search_mode=CitationReport&SID=4CvyYFKm3SC44hNsA2w&page=1&cr_pqid=7&viewType=summary")

# Here's the loop to automate collecting data from the next 600-odd pages...
# Loop to get citation data for each page of results, each iteration will save a txt file
for(i in 1:1000){
  # click on 'save to text file'
  result <- try(
  webElem <- remDr$findElement(using = 'id', value = "select2-chosen-1")
  ); if(class(result) == "try-error") next;
webElem$clickElement()
  # click on 'send' on pop-up window
  result <- try(
  webElem <- remDr$findElement(using = "css", "span.quickoutput-action")
   ); if(class(result) == "try-error") next;
webElem$clickElement()
  # refresh the page to get rid of the pop-up
remDr$refresh()
  # advance to the next page of results
result <- try(
  webElem <- remDr$findElement(using = 'xpath', value = "(//form[@id='summary_navigation']/table/tbody/tr/td[3]/a/i)[2]")
  ); if(class(result) == "try-error") next;
webElem$clickElement()
print(i) 
}

### get all text files into R (move them manually into a folder of their own)
setwd("/home/two/Downloads/WoS")
# get text file names
my_files <- list.files(pattern = ".txt")
# make list object to store all text files in R
my_list <- vector(mode = "list", length = length(my_files))
# loop over file names and read each file into the list
my_list <- lapply(seq(my_files), function(i) read.csv(my_files[i], 
                                                      skip = 4, 
                                                      header = TRUE,                            
                                                      comment.char = " "))
# check to see it worked
my_list[1:5]

## combine list of dataframes into one big dataframe
# use data.table for speed
library(data.table)
my_df <- rbindlist(my_list)
setkey(my_df)
# filter only a few columns to simplify
my_cols <- c('Title', 'Publication.Year', 'Total.Citations', 'Source.Title')
my_df <- my_df[,my_cols, with=FALSE]
# remove duplicates
my_df <- unique(my_df)
# what journals do we have?
unique(my_df$Source.Title)

## create new column that is 'decade'
# first make a lookup table to get a decade for each individual year
year1 <- 1900:2050
my_seq <- seq(year1[1], year1[length(year1)], by = 10)
indx <- findInterval(year1, my_seq)
ind <- seq(1, length(my_seq), by = 1)
labl1 <- paste(my_seq[ind], my_seq[ind + 1], sep = "-")[-42]
dat1 <- data.table(data.frame(Publication.Year = year1, 
                              decade = labl1[indx], 
                              stringsAsFactors = FALSE))
setkey(dat1, 'Publication.Year')
# merge the decade column onto my_df
my_df <- merge(my_df, dat1, by = 'Publication.Year')

## find the most citated paper by decade of publication

df_top <- my_df[ave(-my_df$Total.Citations, my_df$decade, FUN = rank) <= 10, ] 

# Draw the plot...
# code from from Jonathan Goodwin <joncgoodwin@gmail.com>
# format of data: title,cites,group,journal
# THE WRITERS AUDIENCE IS ALWAYS A FICTION,205,1974-1979,PMLA

library(ggplot2)
ws <- df_top

ws <-  ws[order(ws$decade,-ws$Total.Citations),]
ws$Title <- factor(ws$Title, levels = unique(ws$Title)) #to preserve order in plot, maybe there's another way to do this

g <- ggplot(ws, aes(x=Total.Citations, 
                    y=Title, 
                    label=Source.Title, 
                    group=decade, 
                    colour=Source.Title))

g <- g + geom_text(size=4) + 
  facet_grid (decade ~.,
              drop=TRUE,
              scales="free_y") + 
  theme_bw(base_family="Helvetica") +
  theme(axis.text.y=element_text(size=8)) +
  xlab("Number of Web of Science Citations") + ylab("") +
  labs(title="Archaeology's Ten Most-Cited Articles Per Decade (1970-)", size=7) + 
  scale_colour_discrete(name="Journals")

g #adjust sizing, etc.










# if working manually: 
# scroll to bottom of search results, where 'Output Records' section is
# Step 1: Select records -> 1-500 since 500 is max 
# Step 2: Select content -> Full Record & Cited References (this seems to have changed)
# Step 3: Select destination -> save to plain text

# Once we've got the data, use or adapt this python script
# http://www.unc.edu/~ncaren/cite_network/citenet.py
# and note the endnote here: http://www.jgoodwin.net/?p=1223

# then make network diagrams and calculate typical sna stats...

