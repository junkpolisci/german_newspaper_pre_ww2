library(rvest)
library(tidyverse)

#testing the URLs visually for a pattern to identifytheir id and the date pattern 
# ###volks zeitung - ID is 24344771 -- follows SNP
# http://content.staatsbibliothek-berlin.de/zefys/SNP24344771-18570101-0-0-0-0.pdf
# http://content.staatsbibliothek-berlin.de/zefys/SNP24344771-18570103-0-0-0-0.pdf
# http://content.staatsbibliothek-berlin.de/zefys/SNP24344771-18570104-0-0-0-0.pdf
# http://zefys.staatsbibliothek-berlin.de/index.php?id=title&no_cache=1&tx_zefyskalender_pi4%5Bzdb%5D=24344771&tx_zefyskalender_pi4%5Byear%5D=%2A
# 
# ###Neue Preuﬂische Zeitung - ID is 24350382 -- follows SNP
# http://content.staatsbibliothek-berlin.de/zefys/SNP24350382-18570101-0-0-0-0.pdf
# http://content.staatsbibliothek-berlin.de/zefys/SNP24350382-18570103-0-0-0-0.pdf
# http://content.staatsbibliothek-berlin.de/zefys/SNP24350382-18570104-0-0-0-0.pdf

#generate all possible dates. This is a lazy way to loop, but will certainly get all possible dates.
#this also removes non-numeric objects to paste into the URL later. This is testing to see if it works. 
date <- stringr::str_replace_all(seq.Date(as.Date("1857-01-01"), as.Date("1957-12-31"), by = "day"),
                         pattern = "\\-" , 
                         "") 


#I automate below to extract the names and IDs of all newspapers from their newspaper list 
urls_papers_html <- read_html("http://zefys.staatsbibliothek-berlin.de/index.php?id=list") #list of newspapers
urls_papers <- html_nodes(urls_papers_html, "#c140 a") %>%
               html_attr('href') #extracts the second half of the link to each newspaper's summary page

urls_name_id <- paste0("http://zefys.staatsbibliothek-berlin.de/", urls_papers) #creates the URLs for each newspaper

name <- c() #empty name list to fill with the loop below 
id <- c() #empty id list to fill with the loop below 

for(i in 1:length(urls_name_id)){

html_name_id <- read_html(urls_name_id[i]) #read the HTML for the newspaper summary page

#Extract the name from the node
name_node <- html_node(html_name_id, ".row:nth-child(2) .td-val") %>% 
    html_text() %>% 
    stringr::str_replace_all("\\\n\\s+",
                             "")
#Extract the ID from the node 
id_node <- html_node(html_name_id, ".row:nth-child(1) .td-val") %>% 
    html_text() %>% 
    stringr::str_replace(pattern = "\\-", 
                         replacement = "") %>% 
    stringr::str_extract(pattern = "\\d+") %>% 
    unlist()

#create the lists of the name and the node 
name <- c(name, name_node)
id <- c(id, id_node)
}

#make a tibble of the name id so they match up. 
name_tibble <- as_tibble(name) %>%
        select(name = value)
id_tibble <- as_tibble(id) %>% 
             select(id = value)

name_id <- cbind(name_tibble, id_tibble)
name_id <- name_id %>% 
           mutate(name_clean = stringr::str_replace_all(tolower(name),
                                                      "\\s",
                                                      "\\_"),
                  uncount = 36889) %>% #I want one observation for each newspaper for each possible date 
           arrange(id) %>% 
           distinct() #noticed that there were some duplicates that this deletes 

#Expand the name_id tibble so it is id-date unit of analysis 
name_id_uncount <- name_id %>% 
                   group_by(id) %>% 
                   uncount(round(uncount, 0)) %>% 
                   as_tibble()

#add the date column 
name_id_uncount <- name_id_uncount %>% 
                   group_by(id) %>% 
                   mutate(date = stringr::str_replace_all(seq.Date(as.Date("1857-01-01"), as.Date("1957-12-31"), by = "day"),
                                                          pattern = "\\-" , 
                                                          ""))
#This tibble has the url, id, filename, and the newspaper name
url_id_date <- name_id_uncount %>% 
               mutate(base_url_start = "http://content.staatsbibliothek-berlin.de/zefys/SNP",
                      base_url_end = "-0-0-0-0.pdf",
                      url = paste0(base_url_start, id, "-", date, base_url_end),
                      filename = paste0(id, "_", date)
                      ) %>% 
               select(url,
                      id,
                      filename, 
                      name)

#create a unique folder for each newspaper 
for(i in 1:length(name_id$id)){
dir.create(paste0(getwd(), "/test_papers/", name_id$id[i]))
}

tictoc::tic()
for(i in 19710:20440){
download.file(url_id_date$url[i], #download the pdf from this url 
              paste0("./test_papers/", #file path - part 1
                     url_id_date$id[i], #file path - part 2
                     "/", #file path - part 3
                     url_id_date$filename[i], #file name 
                     ".pdf"), #file type 
              mode = "wb") 
              Sys.sleep(30)
}
tictoc::toc()


