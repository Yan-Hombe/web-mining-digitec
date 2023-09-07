# code for the web mining project
# please not that during the working process some 
# code was added and some data was additionally scraped
# therefore some scraping sessions where redundant and some
# intermidiate data frames contain redundant data

# however the final data frames is clean and contains no redundant data

# load the required libraries
library(rvest)
library(RSelenium)
library(dplyr)
library(xml2)
library(stringr)
library(rlang)
library(psych)
library(stargazer)

# Start a Selenium Server
driver <- rsDriver(browser = "firefox", chromever = NULL)
remote_driver <- driver$client

get_links <- function(search_word, num_links = NULL , wright_csv = FALSE, file_name, print_output = FALSE) {
  # this function scrapes the links of a product in a digitec search result
  #
  # Arguments:
  # search_word: the search word for which the links should be scraped
  # num_links: number of links to be scraped. If NULL all links are scraped (default)
  # wright_csv: if TRUE the links are wrighten to a csv file (default = FALSE)
  # file_name: name of the csv file (default = "links.csv")
  # print_output: if TRUE the function prints the progress (default = FALSE)
  #####
  # Navigate to the webpage. "&so=7" sortds the webpage by comments because only the products with comments are of
  # interest.
  remote_driver$navigate(paste("https://www.digitec.ch/de/search?q=", search_word, "&so=7", sep = ""))
  # Navigate to the webpage
  print("went through here_start")
  Sys.sleep(2)
  # scroll down so the content loads. "asdf" is used as a dirty fix to make the function work
  remote_driver$executeScript("window.scrollTo(0, 500);", args = list("asdf"))
  # Locate the button by its CSS selector.
  button_element <- remote_driver$findElement(using = "css selector",
  "button.sc-1olg58b-0:nth-child(2)")

  # initialize the button_position variable (position of the first time the page is loaded)
  # get to the right position
  remote_driver$executeScript("arguments[0].scrollIntoView(true);", args = list(button_element))

  # get the position in terms of pixel and substract some pixel
  # in order to get the button in to view. Otherwise it won't be executed.
  button_position <- remote_driver$executeScript("return window.pageYOffset;", args = list("asdf"))
  button_position <- as.numeric(button_position)-300

  # initialize current position
  # because first page is all loaded we can skip the
  # srolling part in the first step and safe some time
  current_pos <- button_position + 1

  # scrape page source
  page_source <- remote_driver$getPageSource()[[1]]
  # Parse the HTML
  html_doc <- read_html(page_source)
  if (is.null(num_links)) {
    # check how many products there exist to srape
    tot_num_products <- html_doc %>% html_nodes(".sc-iucp1o-2") %>% html_text()
    # take only the right hand side of a string to a certain position
    tot_num_products <- substr(tot_num_products, (nchar(tot_num_products)-14),(nchar(tot_num_products)-10))
    tot_num_products <- stringr::str_replace_all(tot_num_products, "’", "")
    tot_num_products <- stringr::str_extract_all(tot_num_products, "\\d+")
    tot_num_products <- as.integer(tot_num_products)
  } else {
    tot_num_products <- num_links
  }
  # intialize the data frame in which the products are stored
  df_links <- data.frame()

  # Extract the href attributes from the a tags
  links <- html_doc %>%
    html_nodes("a") %>%    # find all 'a' tags
    html_attr("href")
  # only get the links to the products
  staubsauger_links <- links[grepl(paste("-", search_word, "-", sep = ""), links, ignore.case = TRUE)]
  staubsauger_links <- staubsauger_links[grepl("/de/s1/product/", staubsauger_links, ignore.case = TRUE)]

  # add scraped products to the dataframe
  new_link <- data.frame(staubsauger_links)
  df_links <- rbind(df_links, new_link)

  # initialize the product count with 24 on first page
  prod_count <- 24
  
  j <- 0
  start_prod_count_increase <- FALSE

  while (prod_count < tot_num_products) {
    while (current_pos < button_position) {
      Sys.sleep(0.5)
      # scrape the page again
      # scrape page source
      page_source <- remote_driver$getPageSource()[[1]]
      # Parse the HTML
      html_doc <- read_html(page_source)
      # Extract the href attributes from the a tags
      links <- html_doc %>%
        html_nodes("a") %>%    # find all 'a' tags
        html_attr("href")
      # only get the links to the products
      staubsauger_links <-   staubsauger_links <- links[grepl(paste("-", search_word, "-", sep = ""),
       links, ignore.case = TRUE)]

      staubsauger_links <- staubsauger_links[grepl("/de/s1/product/", staubsauger_links,
       ignore.case = TRUE)]

      # add scraped products to the dataframe. Data frame handling is
      # inefficient and needs some cumputing power
      # but it is necessary because the wep page is loaded dynamically
      new_link <- data.frame(staubsauger_links)
      df_links <- rbind(df_links, new_link)
      # delete duplicates
      df_links <- distinct(df_links)

      if (print_output == TRUE) {
        print(paste("went through here_inner loop"))
      }

      # increse counter for current_pos in order to scrol down ####### was able to fetch 95% of products. may increase the scrol a bit in order to make it faster
      current_pos <- current_pos + 700
      # scrol down in order to scrape next product links
      remote_driver$executeScript(paste("window.scrollTo(0, ", current_pos, ");",
      sep = ""), args = list("asdf"))
    }
    try({
      button_element <- remote_driver$findElement(using = "css selector",
      "button.sc-1olg58b-0:nth-child(2)")
    
    Sys.sleep(1)
    if(print_output == TRUE) {
      print("went through here_outer loop")
    }
    # load the new products and adjust the button position which is now further down the webpage
    # go to button position
    remote_driver$executeScript("arguments[0].scrollIntoView(true);", args = list(button_element))
    # increase the counter for inner while loop
    button_position <- remote_driver$executeScript("return window.pageYOffset;", args = list("asdf"))
    button_position <- as.numeric(button_position)-300
    button_position <- as.numeric(button_position)
    # scrol a couple of pixels upwards bacause the button position is not exact
    remote_driver$executeScript(paste("window.scrollTo(500, ", button_position, ");", sep = ""), args = list("asdf"))
    }, silent = TRUE)
    # make the function more robust with implemented while loop
    # sometimes clickElement breaks down function when loaded too fast
    n <- 1
    while (n <= 4) {
      result <- try({
        button_element$clickElement() # click on button to activate new comments
      }, silent = TRUE)
      if (inherits(result, "try-error")) {
        Sys.sleep(0.5)
        n <- n + 1 # increase counter
      } else {
        n <- 5 # break loop
      }
    }
    try({
    # have to find the element again because the page is reloaded
    button_element <- remote_driver$findElement(using = "css selector",
    "button.sc-1olg58b-0:nth-child(2)")
    Sys.sleep(2)
    # go to button position
    remote_driver$executeScript("arguments[0].scrollIntoView(true);", args = list(button_element))
    # get the position of the button
    Sys.sleep(1.5)
    }, silent = TRUE)
    button_position <- remote_driver$executeScript("return window.pageYOffset;", args = list("asdf"))
    button_position <- as.numeric(button_position)

    # increase prod_count by 60 on every page after first iteration
    if (start_prod_count_increase) {
      prod_count <- prod_count + 60
    }
    j <- j + 5
    start_prod_count_increase <- TRUE # set to true after first iteration
    if(print_output == TRUE) {
      print(prod_count)
      print(c("this is the button position",  button_position))
      print(c("this is the current_pos",  current_pos))
    }

    # write csv every 4 iterations in order to save the data
    if (j%%20 == 0 && wright_csv == TRUE) {
      write.csv(df_links, file_name, row.names = FALSE)
    }
    print("after the csv write")
  }
  if (!is.null(num_links)) {
    df_links <- df_links[1:num_links,]}
  return(df_links)
}

get_comments <- function(df_links, max_comment_load, wright_csv = FALSE, file_name, print_output = FALSE) {
  df <- data.frame(matrix(NA, nrow = nrow(df_links), ncol = 18))
  year_vec <- paste(seq(0, 4), "year_ago", sep = "_")
  month_vec <- paste(seq(0, 12), "month_ago", sep = "_")
  time <- c(year_vec, month_vec)

  names(df) <- NULL
  colnames(df) <- time
  
  j <- 1

  for (link in df_links[,]) {
    url <- paste("https://www.digitec.ch/de/s1/product/ratings/", sub(".*/{1,3}", "", link),"?sortOrder=Newest&loadNextPage=true", sep = "")
    print(url)

    # go to comment section
    remote_driver$navigate(url)
    if(print_output == TRUE) {
      print("after navigating to url new link")
    }
    suppressMessages(
    try({
      # define the button to load more comments
      button_element <- remote_driver$findElement(using = "css selector",
      ".sc-1olg58b-1")
    }, silent = TRUE)
    )
    if(print_output == TRUE) {
      print("after finding button element")
      print(j)
    }
    # click through the comments to later download them all
    n <- 1 # initialize number of loading comments
    while (n <= max_comment_load) {
      result <- try({
        button_element$clickElement() # click on button to activate new comments
      }, silent = TRUE)
      if (inherits(result, "try-error")) {
        break
      } else {
        Sys.sleep(0.5)
        n <- n + 1
      }
    }
    if(print_output == TRUE) {
      print("after clicking the button")
    }
    # download the page source of the comments page
    page_source <- remote_driver$getPageSource()[[1]]

    html_doc <- read_html(page_source)
    # Extract the comments
    comments <- html_doc %>% html_elements("article") %>% html_element("p") %>% html_text()
    comments <- comments[grepl("gekauft", comments, ignore.case = TRUE)]

    # one_year_agp
    df[j, "0_year_ago"] <- length(comments[grepl("Monat", comments, ignore.case = TRUE) | 
    grepl("Tag", comments, ignore.case = TRUE)])
    df[j, "1_year_ago"] <- length(comments[grepl("Jahr ", comments, ignore.case = TRUE)])
    df[j, "2_year_ago"] <- length(comments[grepl("2 Jahren", comments, ignore.case = TRUE)])
    df[j, "3_year_ago"] <- length(comments[grepl("3 Jahren", comments, ignore.case = TRUE)])
    df[j, "4_year_ago"] <- length(comments[grepl("4 Jahren", comments, ignore.case = TRUE)])

    df[j,"0_month_ago"] <- length(comments[grepl("einem Monat", comments, ignore.case = TRUE)])
    df[j,"1_month_ago"] <- length(comments[grepl("Tag", comments, ignore.case = TRUE)])

    for (i in 2:12) {
      df[j,paste(i, "month_ago", sep = "_")] <- length(comments[grepl(paste(i, "Monaten", sep = " "), comments, ignore.case = TRUE)])
    }
    
    if (j%%20 == 0 && wright_csv == TRUE) {
      write.csv(df, file_name, row.names = FALSE)
    }

    # go to second column
    j <- j + 1
  }
  return(df)
}

get_variables <- function(df_links, main_prod_type, wright_csv = FALSE, file_name, print_output = FALSE) {
  # define number of rows for the dataframe
  num_rows <- nrow(df_links)
  # definte empty dataframe for storage of scraped data
  df <- data.frame(
  prod_type = rep(NA, num_rows),
  spec_prod_type = rep(NA, num_rows),
  cleaning_type = rep(NA, num_rows),
  clean_surface = rep(NA, num_rows),
  power_KW = rep(NA_real_, num_rows),
  weight_kg = rep(NA_real_, num_rows),
  price = rep(NA_real_, num_rows),
  energy_label = rep(NA, num_rows)
  )

  # initialize counter for row position in dataframe
  j <- 1

  # start loop to iterate through each product link and scrape variables
  for (link in df_links[,]) {

    # adjust the link to get to the website
    url <- paste("https://www.digitec.ch/de/s1/product/", sub(".*/{1,3}", "", link), sep = "")

    # go to product section
    remote_driver$navigate(url)
    Sys.sleep(2)
    # download page source
    page_source <- remote_driver$getPageSource()[[1]]
    # convert to right html format for further manipulation
    html_doc <- read_html(page_source)
    # get the text within the table
    prod_speci <- html_doc %>% html_element("tbody") %>% html_elements("td") %>% html_text()
    if(print_output == TRUE) {
      print(url)
      print(prod_speci)
    }
    # fill the dataframe with the main product typ
    df[j, "prod_type"] <- main_prod_type
    # Check if "Staubsaugertyp" is in prod_speci
    if (any(grepl("Staubsaugertyp", prod_speci))) {
      # Find the position of "Staubsaugertyp" and add it to the dataframe
      position <- which(prod_speci == "Staubsaugertyp")
      df[j, "spec_prod_type"] <- prod_speci[(position + 1)]
    }
    # Check if "Staubsaugertyp" is in prod_speci
    if (any(grepl("Reinigungsart", prod_speci))) {
      # Find the position of "Staubsaugertyp" and add it to the dataframe
      position <- which(prod_speci == "Reinigungsart")
      df[j, "cleaning_type"] <- prod_speci[(position + 1)]
    }
    # Check if "Staubsaugertyp" is in prod_speci
    if (any(grepl("Geeignete Oberflächen", prod_speci))) {
      # Find the position of "Staubsaugertyp" and add it to the dataframe
      position <- which(prod_speci == "Geeignete Oberflächen")
      df[j, "clean_surface"] <- prod_speci[(position + 1)]
    }
    # Check if "Staubsaugertyp" is in prod_speci
    if (any(grepl("Leistungsaufnahme", prod_speci))) {
      # Find the position of "Staubsaugertyp" and add it to the dataframe
      position <- which(prod_speci == "Leistungsaufnahme")
      df[j, "power_KW"] <- as.numeric(paste(str_extract_all(prod_speci[(position + 1)], "\\d+")[[1]], collapse = "."))
    }
    # Check if "Staubsaugertyp" is in prod_speci
    if (any(grepl("Gewicht", prod_speci))) {
      # Find the position of "Staubsaugertyp" and add it to the dataframe
      position <- which(prod_speci == "Gewicht")
      df[j, "weight_kg"] <- as.numeric(paste(str_extract_all(prod_speci[(position + 1)], "\\d+")[[1]], collapse = "."))
    }
    # get the text with the price value
    price_value <- html_doc %>% html_elements(".sc-18ppxou-5") %>% html_text()
    # fill the dataframe with the main product typ
    try({if (!is.na(price_value)) {df[j, "price"] <- price_value}})

    # get the text with the energy label value
    label_value <- label_value <- html_doc %>%
    html_nodes(xpath="//span[starts-with(@aria-label, 'Energielabel:') and not(@aria-describedby)]") %>%
    html_text()
    # add energy label to dataframe
    try ({if (length(label_value) != 0) {df[j, "energy_label"] <- label_value[1]}})

    if(print_output == TRUE) {
      print(df[j,])
      print("added all variables")
    }
    # increase counter by one
    j <- j + 1

    # write csv file every 20 iterations in order to prevent loss of all data
    if (j%%20 == 0 && wright_csv == TRUE) {
      write.csv(df, file_name, row.names = FALSE)
    }
    Sys.sleep(1)
  }
  return(df)
}

#---------------------------------------------
# this is an example how staubsauger products can be scraped
# initally all the links are scraped and saved in a csv file
df_links <- get_links(search_word = "staubsauger", num_links = NULL, wright_csv = TRUE,
 file_name = "staubsauger_links.csv", print_output = TRUE)

write.csv(df_links, paste("staubsauger_links_", nrow(df_links), ".csv", sep = ""), row.names = FALSE)

# get comments for staubauger
all_com <- get_comments(df_links = df_links, max_comment_load = 100, wright_csv = TRUE,
 file_name = "saved_comments.csv", print_output = TRUE)

# save the comments as csv
write.csv(all_com, paste("saved_comments_", nrow(all_com), ".csv", sep = ""), row.names = FALSE)

# get the variables for the staubsauger
df_variables <- get_variables(df_links, main_prod_type = "Staubsauger", wright_csv = TRUE,
  file_name = "variables.csv", print_output = TRUE)

# save the variables in a csv file
write.csv(df_variables, paste("staubsauger_variables_", nrow(df_variables), ".csv", sep = ""), row.names = FALSE)
#---------------------------------------------

auto_scrape <- function(search_word_vec, num_links = NULL, wright_csv = TRUE,
 print_output = TRUE, max_comment_load = 100) {
  # this function scrapes the links of a product in a digitec search result
  # and then takes them to scrape the comments and variables and finally merge them
  # Note:
  # Has the advantage that it can scrape multiple products in a row
  # however always check which is the right search word for the product
  # this word can be found in the link of each product
  #
  # Arguments:
  # search_word_vec: the search words for which the links should be scraped
  # num_links: number of links to be scraped. If NULL all links are scraped (default)
  # wright_csv: if TRUE the links are wrighten to a csv file (default = FALSE)
  # file_name: name of the csv file (default = "links.csv")
  # print_output: if TRUE the function prints the progress (default = FALSE)
  #####
  # Loop to assign values to variables
  for (word in search_word_vec){
    a <- get_links(word, num_links = num_links, wright_csv = wright_csv,
      file_name = paste(word, "_links.csv", sep = ""), print_output = print_output)
    write.csv(a, paste(word ,"_links_", nrow(data.frame(a)), ".csv", sep = ""), row.names = FALSE)
    a <- data.frame(a)
    b <- get_comments(a, max_comment_load = max_comment_load, wright_csv = TRUE, file_name = paste(word, "_comments.csv", sep = ""),
      print_output = print_output)
    write.csv(b, paste(word ,"_comments_", nrow(a), ".csv", sep = ""), row.names = FALSE)
    c <- get_variables(a, main_prod_type = word, wright_csv = TRUE,
      file_name = paste(word, "_variables.csv", sep = ""), print_output = print_output)
    write.csv(c, paste(word ,"_variables_", nrow(a), ".csv", sep = ""), row.names = FALSE)

    d <- cbind(a, b, c)
    write.csv(d, paste(word ,"_merged_", nrow(a), ".csv", sep = ""), row.names = FALSE)
  }
}

# define how many links should be scraped for each product. Note that the first three are NULL
# which meand that all links from the website are scraped with around 80 - 90 % of the links
list_num_links <- list(NULL, NULL, NULL, 90, 140, 90, 100, 160, 90, 90, 300)

# define the search words. They have to match the search words in the links
# one can find them by looking at the links on each product page
# this words should match the list_num_links by position
# note always ü = ue, ä = ae, ö = oe
search_word_vec <- c("staubsauger", "backofen", "kuehlschrank", "geschirrspueler","wasserkocher","waschmaschine", "waeschetrockner", "toaster",
 "kaffeevollautomat", "siebtraegermaschine", "tv")

# scrape all the products. Sometimes it is necessary to run the code more than ones
# due to website blockage. In this case run it only with the remaining products
for(number in list_num_links){
  auto_scrape(search_word_vec = search_word_vec, num_links = number, wright_csv = TRUE,
  print_output = TRUE, max_comment_load = 100)
}

#---------------------------------------------
# data cleaning, merging and manipulation
# define the folder name the data is stored in
folder_path <- "data"

# get list of file names in the folder
file_names <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# load all CSV files
data_all <- lapply(file_names, read.csv)

# remove column names from each dataframe
data_all <- lapply(data_all, setNames, rep("", ncol(data_all[[1]])))

# read column names from a separate CSV file
namesdf <- read.csv("data/toaster_merged_160.csv")
col_names <- names(namesdf)
col_names[1] <- "link"

# Apply column names to each dataframe
data_all <- lapply(data_all, setNames, col_names)

# Combine all dataframes into a single dataframe
combined_data <- do.call(rbind, data_all)

# remove missing prices
combined_data <- filter(combined_data, !is.na(price))
# make price numeric
combined_data <- combined_data %>% mutate(price = as.numeric(str_extract(price, "\\d+")))

# define the columns that contain time information
comments_time <- col_names[2:19]
# remove duplicates regarding the comments this is important, since some products
# have the same comments for example a toaster that only differs
# by the color but has the same comments
combined_data <- combined_data %>% distinct(!!!syms(comments_time), .keep_all = TRUE)

# remove missing prices
combined_data <- filter(combined_data, !is.na(price))

# aggregate the comments over the different time periods
combined_data <- combined_data %>% rowwise() %>%mutate(sum_comments = sum(!!!syms(comments_time[1:5]))) %>% ungroup()

# delete row without comments
combined_data <- filter(combined_data, sum_comments > 5)

# create column that contains approximation for initial selling year of product
# 5 = 4 years or older, 4 = 3 years old, 3 = 2 years old, 2 = 1 year old, 1 = less than 1 year old
combined_data <- combined_data %>% mutate(selling_year = ifelse(X4_year_ago == 0,
 ifelse(X3_year_ago == 0, ifelse(X2_year_ago == 0, ifelse(X1_year_ago == 0, 1, 2), 3), 4),5))

#---------------------------------------------
# because there is additional need for variables as the reseach questions evolved
# for each product type (i.e. toester, waterboiler, vacuum cleaner)
# another scraping code is applied in order to get a bigger variety of variables
# the already cleaned dataframe from previous exercise is used in order to safe time
# and not scraping redundant data

# define the function to get the variables
variables_no_label <- function(df_links, print_output = TRUE){
  #-----
  # this function is used to get the variables for each product type
  # that are not EU energy labeled
  # Arguments:
  # df_links: dataframe with the links of the products
  # print_output: if TRUE the output will be printed
  # Returns:
  # return_df: dataframe with the variables for each product
  #-----
  # initialize counter for row position in dataframe
  j <- 1
  # define the initial iteration as TRUE so the names of the dataframe are defined
  intitial_iteration <- TRUE
  # start loop to iterate through each product link and scrape variables
  for (link in df_links[,]) {

    # adjust the link to get to the website
    url <- paste("https://www.digitec.ch/de/s1/product/", sub(".*/{1,3}", "", link), sep = "")


    # go to product section
    remote_driver$navigate(url)
    Sys.sleep(2)

    suppressMessages(
    try({
      # define the button to load more comments
      button_element <- remote_driver$findElement(using = "xpath",
       "//button[@data-test='showMoreButton-specifications']")
      # Click the button
      Sys.sleep(1)
      button_element$clickElement()
    }, silent = TRUE)
    )
    # get page source
    page_source <- remote_driver$getPageSource()[[1]]

    # convert to right html format for further manipulation
    html_doc <- read_html(page_source)
    # get the text within the table
    prod_speci <- html_doc %>% html_elements("tbody") %>% html_elements("td") %>% html_text()

    # print output in order to check if the function works
    if(print_output == TRUE) {
      print(url)
      print(prod_speci[1:15])
      print(paste("number of iteration:", j, sep = " "))
    }
    # define dataframe with the scraped variables 
    # every even number is a variable name and odd number is the value
    df <- data.frame(
    ColumnName = prod_speci[seq(1, length(prod_speci), by = 2)],
    ColumnValue = prod_speci[seq(2, length(prod_speci), by = 2)]
    )
    # initiate a dataframe in which the data is saved
    if (intitial_iteration == TRUE) {
      initial_dataframe <- distinct(df, ColumnName)
      return_df <- as.data.frame(matrix(NA, nrow = nrow(df_links), ncol = nrow(initial_dataframe)))
      names(return_df) <- NULL
      colnames(return_df) <- initial_dataframe$ColumnName
    }
    # remove duplicates from the dataframe by its column name
    df <- distinct(df, ColumnName, .keep_all = TRUE)

    if(print_output == TRUE) {
      print(df$ColumnName)
    }
    # save the values in the return dataframe if there is a match
    for(colname in initial_dataframe$ColumnName) {
      value_to_check <- colname
      # find a matching column name in the new iterated product
      value_from_column <- df %>%
      filter(ColumnName == value_to_check) %>%
      pull(ColumnValue)
      # insert the matching value into the return dataframe
      try({return_df[j, colname] <- value_from_column}, silent = TRUE)
    }
    # set the initial iteration to false so it won't define
    # the names of the return dataframe again
    intitial_iteration <- FALSE
    # increase counter by one
    j <- j + 1
  }
return(return_df)
}

# get the links for the toaster
toaster_links <- as.data.frame(combined_data %>% filter(prod_type == "toaster") %>% select(link))
toaster_df <- combined_data %>% filter(prod_type == "toaster")

# scrape the variables for the toaster and merge it with the already cleaned dataframe and save it in a csv file
new_var_toaster <- variables_no_label(toaster_links, print_output = TRUE)
merged_toaster <- cbind(toaster_df, new_var_toaster)
write.csv(merged_toaster, paste(toaster_df$prod_type[1] ,"_cleaned_merged_", nrow(merged_toaster), ".csv", sep = ""), row.names = FALSE)

# get links for wasserkocher
wasserkocher_links <- as.data.frame(combined_data %>% filter(prod_type == "wasserkocher") %>% select(link))
wasserkocher_df <- combined_data %>% filter(prod_type == "wasserkocher")

# scrape the variables for the wasserkocher and merge it with the already cleaned dataframe and save it in a csv file
new_var_wasserkocher <- variables_no_label(wasserkocher_links, print_output = TRUE)
merged_wasserkocher <- cbind(wasserkocher_df, new_var_wasserkocher)
write.csv(merged_wasserkocher, paste(wasserkocher_df$prod_type[1] ,"_cleaned_merged_", nrow(merged_wasserkocher), ".csv", sep = ""), row.names = FALSE)

# get links for staubsauger
staubsauger_links <- as.data.frame(combined_data %>% filter(prod_type == "Staubsauger") %>% select(link))
staubsauger_df <- combined_data %>% filter(prod_type == "Staubsauger")

# scrape the variables for the staubsauger and merge it with the already cleaned dataframe and save it in a csv file
new_var_staubsauger <- variables_no_label(staubsauger_links, print_output = TRUE)
merged_staubsauger <- cbind(staubsauger_df, new_var_staubsauger)
write.csv(merged_staubsauger, paste(staubsauger_df$prod_type[1] ,"_cleaned_merged_", nrow(merged_staubsauger), ".csv", sep = ""), row.names = FALSE)
tail(merged_staubsauger)

# Don't forget to close the Selenium Server when done
driver[["server"]]$stop()

#---------------------------------------------
# data cleaning, merging and manipulation of non energy label data
# load toaster data and clean it
toaster_df <- read.csv("data no label/toaster_cleaned_merged_68.csv")

# predifine variables for later selection
new_name <- c("sum_comments", "power_KW", "price",  "selling_year",
 "cubic_cm", "Anzahl.Toastscheiben", "Toastertyp")

# transform three variables into numeric
for (i in c("Länge", "Breite", "Höhe")) {
  toaster_df <- toaster_df %>%mutate(!!i := as.numeric(gsub("[^0-9.]", "", !!sym(i))))
}
# filter select and manipulate the dataframe in order to proceed with the analysis
toaster_df <- toaster_df %>% filter(!is.na(Anzahl.Toastscheiben)) %>%
 filter(!is.na(Toastertyp) & Höhe < 80) %>% # delete NA
  mutate(Toastertyp = ifelse(Toastertyp == "Langschlitztoaster", "Klassischer Toaster", Toastertyp)) %>% 
   mutate(cubic_cm = Länge * Breite * Höhe) %>%  # get only one variable that represents the dimension of product
    mutate(Anzahl.Toastscheiben = as.numeric(gsub("[^0-9.]", "", Anzahl.Toastscheiben))) %>% # transfrom to numeric
      select(all_of(new_name)) %>% filter(if_any(everything(), ~ !is.na(.))) # select predifined variables

# load wasserkocher data and clean it
wasserkocher_df <- read.csv("data no label/wasserkocher_cleaned_merged_81.csv")

# predifine variables for later selection
new_name <- c("sum_comments", "power_KW", "price",  "selling_year",
 "quadratic_cm", "Gewicht", "Volumen",  "Wasserkochertyp")

# transform three variables into numeric
for (i in c("Länge", "Breite", "Höhe", "Volumen", "Gewicht")) {
  wasserkocher_df <- wasserkocher_df %>%mutate(!!i := as.numeric(gsub("[^0-9.]", "", !!sym(i))))
}
# filter select and manipulate the dataframe in order to proceed with the analysis
wasserkocher_df <- wasserkocher_df %>%
  filter(!is.na(power_KW) & Gewicht < 100) %>% # delete NA in energy usage variable
      mutate(quadratic_cm = Breite * Höhe) %>%  # get only one variable that represents the dimension of product
      # did not use Länge because there are some observations which include the cable as length
        select(all_of(new_name)) %>% filter(if_any(everything(), ~ !is.na(.))) # select predifined variables

# load Staubsauger data and clean it
staubsauger_df <- read.csv("data no label/Staubsauger_cleaned_merged_250.csv")

# predifine variables for later selection
new_name <- c("sum_comments", "power_KW", "price",  "selling_year",
 "quadrat_cm", "Gewicht",  "Staubsaugertyp", "Energieversorgung")

# transform three variables into numeric
for (i in c("Breite", "Höhe", "Gewicht")) {
  staubsauger_df <- staubsauger_df %>%mutate(!!i := as.numeric(gsub("[^0-9.]", "", !!sym(i))))
}
# filter select and manipulate the dataframe in order to proceed with the analysis
staubsauger_df <- staubsauger_df %>%
  filter(!is.na(power_KW) & !is.na(Energieversorgung) & Breite < 120 & Höhe < 150) %>% # delete NA in energy usage variable
  filter(Kategorie == "StaubsaugeriMehr Informationen zu: Staubsauger") %>% # delete vaccum roboter
    mutate(Staubsaugertyp = ifelse(grepl("Handstaubsauger", Staubsaugertyp), "Handstaubsauger", "norm_Staubsauger")) %>%
    mutate(Energieversorgung = ifelse(grepl("Netzbetrieb", Energieversorgung), "Netzbetrieb", "Akku_Batterie")) %>%
    mutate(quadrat_cm = Breite * Höhe) %>%  # get only one variable that represents the dimension of product
    mutate(selling_year = as.factor(selling_year)) %>% # change selling year to factor
      select(all_of(new_name)) %>% filter(if_any(everything(), ~ !is.na(.))) # select predifined variables


#---------------------------------------------
# descriptive statistics for energy labeled data
label_df <- combined_data %>% filter(!is.na(energy_label))

# define variables to select
var_name <- c("sum_comments", "energy_label", "prod_type", "price", "selling_year")

# clean and transform the energy label variable
label_df <- label_df %>% select(all_of(var_name)) %>% filter(prod_type != "toaster") 

# define dataframe for new energy labeled data 
new_label <- label_df %>% select(all_of(var_name)) %>% filter(prod_type != "toaster") %>% 
  filter(prod_type %in% c("Backofen", "kaffeevollautomat", "siebtraegermaschine", "waeschetrockner")) %>%
  # transfrom ordinal level ot energy label to cardinal level
  mutate(label_num = ifelse(energy_label == "A+++", 7, ifelse(energy_label == "A++", 6,
    ifelse(energy_label == "A+", 5, ifelse(energy_label == "A", 4,
      ifelse(energy_label == "B", 3, ifelse(energy_label == "C", 2,  1))))))) %>%
       select(-energy_label) # energy label in this form not used for regression

# define dataframe for old energy labeled data 
old_label <- label_df %>% select(all_of(var_name)) %>% filter(prod_type != "toaster") %>% 
  filter(prod_type %in% c("geschirrspueler", "Kühlschrank", "tv", "waschmaschine")) %>%
  # transfrom ordinal level ot energy label to cardinal level
  mutate(label_num = ifelse(energy_label == "A", 7, ifelse(energy_label == "B", 6,
    ifelse(energy_label == "C", 5, ifelse(energy_label == "D", 4,
      ifelse(energy_label == "E", 3, ifelse(energy_label == "F", 2, 1))))))) %>%
       select(-energy_label) # energy label in this form not used for regression

# creat data frame that shows count of types in the new_label data
sum_new_label <- new_label %>% mutate(prod_type = ifelse(prod_type == "Backofen", "Oven", 
  ifelse(prod_type == "kaffeevollautomat", "Coffee machine aut.",
   ifelse(prod_type == "siebtraegermaschine", "Coffee machine man.", "Tumbler")))) %>%
    group_by(prod_type) %>% summarise(n_type = n())

# create dataframe with general descriptive statistics
# such as mean, min, max, sd etc. for new_label
desc_new_label <- describe(new_label)[c(2,3,4,8,9)]


# creat data frame that shows count of types in the old_label data
sum_old_label <- old_label %>% mutate(prod_type = ifelse(prod_type == "geschirrspueler", "Dishwasher", 
  ifelse(prod_type == "Kühlschrank", "Fridge",
   ifelse(prod_type == "tv", "TV", "Washing Machine")))) %>%
    group_by(prod_type) %>% summarise(n_type = n())

# create dataframe with general descriptive statistics
# such as mean, min, max, sd etc. for old_label
desc_old_label <- describe(old_label)[c(2,3,4,8,9)]

#---------------------------------------------
# descriptive statistics for non energy labeled data i.e. toaster, vaccum cleaner and waterboiler

# create dataframe with general descriptive statistics for toaster
desc_toast <- round(describe(toaster_df)[c(2,3,4,8,9)], 2)
# create dataframe with general descriptive statistics for wasserkocher
desc_wasserkocher <- round(describe(wasserkocher_df)[c(2,3,4,8,9)], 2)
# create dataframe with general descriptive statistics for staubsauger
desc_staubsauger <- round(describe(staubsauger_df)[c(2,3,4,8,9)], 2) 

#---------------------------------------------
# start regression analysis

# energy labeled data
label_model1 <- lm(sum_comments ~  label_num + prod_type + price + as.factor(selling_year) , data = new_label)
summary(label_model1)

label_model2 <- lm(sum_comments ~  label_num + prod_type + price + as.factor(selling_year) , data = old_label)
summary(label_model2)

# create a table for the rmarkdown file
stargazer(label_model1, label_model2, type = "text", float = FALSE,
          dep.var.caption = "dependent variable: number of comments",
          dep.var.labels = "",
          column.labels = c("A+++ to D", "A to G"),
          covariate.labels=c("cardinal label", "price", "selling year 1", "selling year 2", "selling year 3", "selling year 4", "coffee machine aut", "coffee machine man", "tumbler", "fridge","TV", "washing machine"),
          omit.stat=c("rsq", "adj.rsq"),
          order = c(1,8:12),
          omit = c("Constant"),
          df = FALSE)

# non energy labeled data
model1 <- lm(sum_comments ~  power_KW + price + as.factor(selling_year) + cubic_cm + Anzahl.Toastscheiben + Toastertyp, data = toaster_df)
omit_mod1 <-names(model1$coefficients)[c(-2,-3)]

model2 <- lm(sum_comments ~  power_KW + price + as.factor(selling_year) + quadratic_cm + Gewicht + Wasserkochertyp, data = wasserkocher_df)
omit_mod2 <- names(model2$coefficients)[c(-2,-3,-9)]

model3 <- lm(sum_comments ~  power_KW + price + as.factor(selling_year) + quadrat_cm + Gewicht + Staubsaugertyp + Energieversorgung, data = staubsauger_df)

# create a table for the rmarkdown file with the models in non energy labeled data
stargazer(model1, model2, model3, header = FALSE,  type = "text", float = FALSE,
          dep.var.caption = "dependent variable: number of comments",
          dep.var.labels = "",
          column.labels = c("Waterboiler", "Toaster", "Vacuum cleaner"),
          omit.stat = c("rsq", "adj.rsq"),
          covariate.labels=c("power input KW", "price", "selling year 1", "selling year 2", "selling year 3", "selling year 4", "quadrat cm", "weight kg", "vacuum type", "no batterie"),
          df = FALSE,
          omit = c(omit_mod1, omit_mod2, "Constant"), 
          out = NULL)
