# 00_common.R


# source ("00_common.R")
#0. Global settings ----
if (T) {  
  library(magrittr); 
  library(ggplot2); 
  library(dtplyr)
  library(lubridate,  quietly=T); options(lubridate.week.start =  1)
  library(data.table); options(datatable.print.class=TRUE)
  library(dygraphs)
  library(plotly); library(DT); 
  # library(heatmaply);  
  library(ggpubr)
  library(stringr); library(forcats) 
  
  options(digits = 3)
  # options(max.print = 100) # 1000
  # options(scipen = 999)
  
  dateToday <- format(Sys.time(), '%d %B, %Y') %>% dmy; dateToday
  
  "%wo%" <- function(x, y) setdiff(x,y) 
  `%ni%` <-  Negate(`%in%`)
  as.dt <- as.data.table
  
  theme_set(theme_bw())
  # theme_set(theme_minimal())
  # library(ggthemes); 
  # theme_set(theme_economist())
  # theme_set(theme_economist_white())
  
}



of <- function (x, range)  { # use negative range to remove
  if (is.numeric(range)) {
    # if (bKeep) {
    return(x[range])
    # } else {
    #   #return(x[ which ( (1:length(x)) %ni% range ) ])
    #   return(x[-range])
    # }
  }else { # - for now  assume it is data.table 
    dt.cols(x, range)
    # return(x[, range, with=F]) 
  }
}


#Found in: stackoverflow:
# Efficient (in place) rows deletion from data.table 

#' Title
#'
#' @param DT 
#' @param del.idxs 
#'
#' @return
#' @export
#'
#' @examples
dt.rmRow <- function(DT, del.idxs) {  # pls note 'del.idxs' vs. 'keep.idxs'
  if (!is.data.table(dt))
    dt <- as.data.table(dt)
  
  keep.idxs <- setdiff(DT[, .I], del.idxs);  # select row indexes to keep
  cols = names(DT);
  DT.subset <- data.table(DT[[1]][keep.idxs]); # this is the subsetted table
  setnames(DT.subset, cols[1]);
  for (col in cols[2:length(cols)]) {
    DT.subset[, (col) := DT[[col]][keep.idxs]];
    DT[, (col) := NULL];  # delete
  }
  return(DT.subset); # NB: Original DT is also changed  by reference !
}

if (F) {
  dt <- readRDS(paste0("13100810.Rds"))
  dt
  dt %>% dt.rmRow(nrow(dt))
}


colsSelect <- function (dt, cols)  {
  dt[, cols, with=F]
  # Note also other ways to select column(s) in data.table
  # dt[, ..cols] # FOR ONE COLUMN ONLY
  # dt[,.SD, .SDcols=cols] 
}

colsRm <- function (dt, cols)  {
  dt[, (cols) := NULL] 
}

setcolorder.fromLast <- function( dt, neworder) {
  # TBD - useful for cast.
  
  # setcolorder.fromLast(dtCached, c("Cause of death (ICD-10)", "value"))
  # setcolorder(dtCached, c("Date",  "GEO", "Cause of death (ICD-10)", "value"))
}

### Automatically finding / removing common parts in strings 


# https://stackoverflow.com/questions/48701107/find-length-of-overlap-in-strings

#' Title
#'
#' @param str1 
#' @param str2 
#' @param ignore.case 
#'
#' @return
#' @export
#'
#' @examples
str_find_overlap <- function(str1, str2, ignore.case = FALSE) { # , verbose = FALSE
  
  if(ignore.case) {
    str1 <- tolower(str1);    str2 <- tolower(str2)
  }
  if(nchar(str1) < nchar(str2)) {
    x <- str2;    str2 <- str1;    str1 <- x
  }
  
  x <- strsplit(str2, "")[[1L]]
  n <- length(x)
  s <- sequence(seq_len(n))
  s <- split(s, cumsum(s == 1L))
  s <- rep(list(s), n)
  
  for(i in seq_along(s)) {
    s[[i]] <- lapply(s[[i]], function(x) {
      x <- x + (i-1L)
      x[x <= n]
    })
    s[[i]] <- unique(s[[i]])
  }
  
  s <- unlist(s, recursive = FALSE)
  s <- unique(s[order(-lengths(s))])
  
  i <- 1L
  len_s <- length(s)
  while(i < len_s) {
    lcs <- paste(x[s[[i]]], collapse = "")
    # if(verbose) cat("now checking:", lcs, "\n")
    check <- grepl(lcs, str1, fixed = TRUE)
    if(check) {
      # if(verbose) cat(paste0("Found: '",lcs,"' (length =", nchar(lcs), ") \n")) 
      break
    } else {
      i <- i + 1L 
    }
  }
  return (lcs)
}
# 
# str_remove_overlap <- function(aStr) {
#   str0 <- str_find_overlap( aStr[1],  aStr[2]); str0
#   str_replace(aStr, str0, "")
# }
# 
# if (F) {
#   library(data.table)
#   # dt <- cansim::get_cansim("13-10-0810-01") %>% setDT(dt) 
#   dt <- data.table::data.table(
#     GEO=c( # From CANSIM Table
#       "Newfoundland and Labrador, place of occurrence",
#       "Prince Edward Island, place of occurrence",     
#       "Nova Scotia, place of occurrence"
#     ))
#   
#   aStr <- dt$GEO
#   
#   
#   dt[, GEO:=str_remove_overlap(GEO)][]
#   #                         GEO
#   #                      <char>
#   #1: Newfoundland and Labrador
#   #2:      Prince Edward Island
#   #3:               Nova Scotia
# }
# 

# Consider also: paged_table(dt), reactable() and https://gt.rstudio.com/

datatable.title <- function(dt, title=NULL) {
  # https://rstudio.github.io/DT/options.html
  dt %>% DT::datatable (
    filter = "top",  
    caption = title,
    rownames=F,   
    extensions =  c('ColReorder', 'Buttons'),
    options = list(
      dom = 'Blfrtip',
      # paging = FALSE, 
      # pageLength=50, scrollX='400px', # horizontal scroll
      scrollX = TRUE,       scrollY = "600px",
      
      colReorder = TRUE,
      lengthMenu = list(c(10,25,100,-1), c(10,25,100,"All")),
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print') 
    ) )
}




dygraph.title <- function(dts, title=NULL, group="1st group") {
  dygraph(dts, main = title, group = group) %>%
    # dySeries(input$var1, color = input$color1, strokePattern = input$stroke1,  axis = input$axis1 )  %>% 
    dyOptions(fillGraph = F, stepPlot = F, drawGrid = T, drawPoints = TRUE, pointSize = 2) %>%
    dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
    dyAxis("y", label="Deaths / week") %>%
    dyAnnotation("2021-5-1", text = "3%", tooltip = "Fully Vaccinated Rate 3%") %>%
    dyAnnotation("2021-4-6", text = "2%", tooltip = "Fully Vaccinated Rate 2%") %>%
    dyAnnotation("2021-6-10", text = "10%", tooltip = "Fully Vaccinated Rate 10%") %>%
    dyAnnotation("2021-2-18", text = "1%", tooltip = "Fully Vaccinated Rate 1%") %>%
    dyRangeSelector() 
}




