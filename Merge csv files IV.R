###MERGE CSV FILES


x <- c("dplyr","readr","readxl","stringr","data.table","rvest","rjson")
lapply(x, require, character.only = TRUE)

#CREATING SEMIFINAL

setwd("C:/Users/alfre/Downloads")

#select year and quarter
year = 2021
quarter = "1Q"

#pattern to identify new files uploaded
str_cutting_point = "-current"

#function to cut each name from a list
name_cut <- function(x) {
    (str_replace_all(str_sub((x),1,end = unlist(gregexpr(
        str_cutting_point,(x)))-1),"_"," "))
}

#function to process files
function_definitive_workbook <- function(x){
    
    #open new files for reading
    final_read_workbooks <- lapply(x, read.csv)
        
    #bind all csv files in setwd Directory
    final_workbook <- as_tibble(rbindlist(final_read_workbooks, 
                                                  use.names = F))
    
    #setting column names as per whalewisdom.com
    new_names <- c("Stock", "Symbol", "Type", "Shares_Held", "Market_Value",
                   "Portfolio_Percent", "Previous_Percent_of_Portfolio", 
                   "Ranking", "Change_in_shares", "Percent_Change", 
                   "Change_Type", "Percent_Ownership", "Qtr_first_owned",
                   "sector", "source_type", "source_date", "Avg_Price",	
                   "Recent_Price","Filer_Name")
    
    setnames(final_workbook, old = c(1:19), new = new_names)
    
    #create new columns
    definitive_workbook <- final_workbook %>%
        mutate(
            Change_Type_New = ifelse (Change_Type == "new", 1, 0),
            Change_Type_Soldall = ifelse (Change_Type == "soldall", 1, 0),
            Original_Shares = case_when (
                Change_Type == "new" ~ 0,
                Change_Type == "" | Change_Type == "NA" ~ Shares_Held,
                Change_Type == "addition" ~ Shares_Held - Change_in_shares,
                Change_Type == "reduction" ~ Shares_Held - Change_in_shares,
                Change_Type == "soldall" ~ as.double(abs(Change_in_shares))
            ),
            CALL = ifelse (Type == "CALL", Shares_Held, 0),
            PUT = ifelse (Type == "PUT", Shares_Held, 0)
        )
    
    definitive_workbook <- definitive_workbook %>% 
        filter(
            !grepl("*share*|*trust*|*etf*|*note*", Stock))
    
    return(definitive_workbook)
}

#list of new files
new_workbooks_temp <- list.files(pattern = "-current", full.names= F)

#save the new files cutting the names down at "-current" pattern position
new_workbooks <- lapply(new_workbooks_temp, function(x){
    paste0(name_cut(x),".csv")
    })

#remove files less than 230 bytes (just the bytes of the header, meaning the
#filer reported 0 stocks)
lapply(new_workbooks, function(x){
  if (file.size(x) < 230) file.remove(x)
})
    
if (length(new_workbooks) != 0) {
  
  #list all files in Directory in 'setwd'
  total_workbooks_temp <- list.files(pattern = "*.csv", full.names= F)
  
  #find and removed duplicate files
  duplicate_files <- intersect(total_workbooks_temp, new_workbooks)
  duplicate_files_sans_ext <- lapply(duplicate_files,
                                     tools::file_path_sans_ext)
  lapply(duplicate_files, file.remove)
  
  
  #save new files with the same name without '-current...' and
  #add a column named as the file name.
  
  lapply(new_workbooks_temp, function(x) {
    
    write.csv(
      transform(
        read.csv(x, check.names = F),
        Filer_Name = name_cut(x) #add column with the same name of filer
      ),
      file = paste0(name_cut(x),".csv"),
      row.names = F)
  })
  
  #remove files with the '-current' pattern
  lapply(new_workbooks_temp, file.remove)
  
  #check if object Semifinal exists in the Global Environment
  if (exists("Semifinal") == T) {
    
    #remove from 'Semifinal' the rows with the names 
    #of AMENDED files, since they were replaced with new ones.
    remove_duplicates <- paste0("\\b", c(duplicate_files_sans_ext),
                                "\\b", collapse = '|')
    Semifinal <- Semifinal %>% filter(!grepl(remove_duplicates, Filer_Name))
    
    #calling function 'function_definitive_workbook'
    Semifinal_now <- function_definitive_workbook(new_workbooks)
    
    Semifinal <- rbind(as.data.frame(Semifinal, stringsAsFactors=F),
                       as.data.frame(Semifinal_now, stringsAsFactors=F))
    
  } else {
    
    #list all files in 'setwd' Dir. (newly created 'workbooks' included)
    total_workbooks <- list.files(pattern="*.csv", full.names= F)
    
    #calling function 'function_definitive_workbook'
    Semifinal <- function_definitive_workbook(total_workbooks)
  }
  
} else {
  
  if (exists("Semifinal") == T) {
    
    Semifinal <- Semifinal
    
  } else {
    
    #list all files in 'setwd' Dir.
    total_workbooks <- list.files(pattern="*.csv", full.names= F)
    
    #calling function 'function_definitive_workbook'
    Semifinal <- function_definitive_workbook(total_workbooks)
  }
}

#grouping into the Final file
Final_function <- function (x,y) {
  if (missing(y)) {
    x %>%
      group_by(Symbol) %>%
      summarise(
        Filer_Name_Count = length(unique(Filer_Name)),
        Shares_Held = sum(Shares_Held),
        Original_Shares = sum(Original_Shares),
        Change_in_shares_percent = as.numeric(gsub("NA","0",
                                                   sprintf("%.0f",(Shares_Held-Original_Shares)/
                                                             (Original_Shares)*100))),
        Avg_Price = round(mean(Avg_Price, na.rm = T),2),
        Original_Filers = Filer_Name_Count - length(unique(
          Filer_Name[Change_Type_New == "1"])),
        Present_Filers = Filer_Name_Count - length(unique(
          Filer_Name[Change_Type_Soldall == "1"])),
        Change_in_Filers_percent = as.numeric(gsub("NA","0",
                                                   sprintf("%.0f", (Present_Filers-Original_Filers)/
                                                             (Original_Filers)*100))),
        Portfolio_Mean = mean(Portfolio_Percent[
          Change_Type_Soldall != "1"], na.rm = T),
        CALL = sum(CALL, na.rm = T),
        PUT = sum(PUT, na.rm = T),
        PUT_CALL_ratio = round(PUT/CALL, digits = 2)
      )
  } else {
    x %>%
      group_by(Symbol) %>%
      filter(Filer_Name %in% y) %>% 
      summarise(
        Filer_Name_Count = length(unique(Filer_Name)),
        Shares_Held = sum(Shares_Held),
        Original_Shares = sum(Original_Shares),
        Change_in_shares_percent = as.numeric(gsub("NA","0",
                                                   sprintf("%.0f", (Shares_Held-Original_Shares)/
                                                             (Original_Shares)*100))),
        Avg_Price = round(mean(Avg_Price, na.rm = T),2),
        Original_Filers = Filer_Name_Count - length(unique(
          Filer_Name[Change_Type_New == "1"])),
        Present_Filers = Filer_Name_Count - length(unique(
          Filer_Name[Change_Type_Soldall == "1"])),
        Change_in_Filers_percent = as.numeric(gsub("NA","0",
                                                   sprintf("%.0f", (Present_Filers-Original_Filers)/
                                                             (Original_Filers)*100))),
        Portfolio_Mean = mean(Portfolio_Percent[
          Change_Type_Soldall != "1"], na.rm = T),
        CALL = sum(CALL, na.rm = T),
        PUT = sum(PUT, na.rm = T),
        PUT_CALL_ratio = round(PUT/CALL, digits = 2)
      )}
}
Final <- Final_function(Semifinal)

#finding the filers with the least number of other filers investing in their
#stocks, meaning they could be the leaders not the followers.
Merge_Final_Semi <- merge(Semifinal, Final, by = "Symbol", all = T)
Leaders_Merge <- Merge_Final_Semi %>% group_by(Filer_Name) %>% 
    summarize(
        n_stocks = length(unique(Symbol)),
        mean_filers = sum(Present_Filers)/n_stocks
    ) %>% 
    filter(
        n_stocks >= 5
    ) %>% 
    slice_min(mean_filers,n = 1000)

#Selecting only the top, bottom and leader filers by market capitalization
Filters_Top_Bottom <- function(x,n,funct) {
    x %>%
    group_by(Filer_Name) %>%
    summarise(
        Market_Value = sum(Market_Value)
    ) %>% 
    funct(Market_Value, n = n)
}
Filters_by_Top_Filers <- Filters_Top_Bottom(Semifinal,1000,slice_max)
Filters_by_Bottom_Filers <- Filters_Top_Bottom(Semifinal,1000,slice_min)
Final_Top <- Final_function(Semifinal, Filters_by_Top_Filers[[1]])
Final_Bottom <- Final_function(Semifinal, Filters_by_Bottom_Filers[[1]])
Final_Leaders <- Final_function(Semifinal, Leaders_Merge[[1]])

#function to filter object Final
Filters <- function(x,m,n,o,p,q,r) {
    x %>% 
        filter(
            Change_in_shares_percent >= m,
            Present_Filers >= n,
            Original_Filers > o,
            Change_in_Filers_percent >= p,
            Portfolio_Mean >= q,
            PUT_CALL_ratio <= r
        ) %>%
        arrange(desc(Change_in_shares_percent))
}
Final_filters <- Filters(Final,200,30,1,50,0.05,0.7)
Final_Top_filters <- Filters(Final_Top,50,10,1,50,0.05,0.7)
Final_Bottom_filters <- Filters(Final_Bottom,50,10,1,50,0.05,0.7)
Final_Leaders_filters <- Filters(Final_Leaders,100,10,1,50,0.05,0.7)

#search by symbol
Final_by_Symbol <- Semifinal %>% filter(Symbol == "ARGX")

Final_by_Symbol_2 <- Final %>% filter(
    # Symbol %in% c("ARGX")
    # Symbol %in% c("SNDL","NNDM","SOLO")
    # Symbol %in% c("FUV","AYRO","DAN","SOLO","FSR","F","GM","GTEC",
    #               "GP","HYLN","KNDI","LI","RIDE","MGA","NKLA","NIO","NIU",
    #               "OSK","TTM","TSLA","VLDR","WKHS","XL","XPEV","OUST","SPI")
    # Symbol %in% c("AMD","ADI","AVGO","INTC","NVDA","QCOM","TSM")
    # Symbol %in% c("BEEM","FSLR","MAXN","SOL","SPI","NOVA","SPWR","SUNW","NEE")
    # Symbol %in% c("PLTR","ABST","TWLO","TCEHY","DOMO","IBM","NUAN","SAIL","CRM",
    #               "SUMO","VERI")
    # Symbol %in% c("DDD","XONE","NNDM","SSYS")
    # Symbol %in% c("TLMD","AMWL","TDOC","HIMS","LFMD","OSCR")
    # Symbol %in% c("FLUX","QS","CBAT","PLUG","BLDP")
    # Symbol %in% c("VFF","TLRY","SLDN","APHA","HEXO","CRON","CGC","ACB","KERN")
) %>% 
    arrange(desc(Change_in_shares_percent))

#search by filer
Final_by_FilerName <- Semifinal %>% filter(grepl("\\bcapital markets\\b",
                                                 Filer_Name))

#setwd where the files will be saved as per year and quarter
setwd(paste0("~/ALFRE/MADE BY ALFREDO NASIFF FORS/INVESTMENT/13F/",year,"/",
            quarter))

#SAVE ALL RELEVANT FILES

#Semifinal
n <- nrow(Semifinal)
#number of times the file needs to be split
n_rep <- n%/%1048576 + 1 #'1048576': ms excel maximum number of rows
chunk_size <- ceiling(n/n_rep)
Semi_factor  <- rep(1:ceiling(n/chunk_size),each=chunk_size)[1:n]
Split_Semifinal <- split(Semifinal,Semi_factor)
lapply(1:n_rep,function(x){
  write.csv(Split_Semifinal[[x]], file = paste0("Split_Semifinal_",year,"_",
                                                quarter,"_",x,".csv"),
            row.names = F)
})
#all other files
print_files <- function(x){
  write.csv(get(x), file = paste0(x,"_",year,"_",quarter,".csv"),row.names = F)
}
lapply(c("Final","Final_filters","Final_Top_filters","Final_Bottom_filters",
         "Final_Leaders_filters"), print_files)

#STATS (OPTIONAL)
#how many filers
length(unique(Semifinal$Filer_Name))

#how many tickers
length(unique(Semifinal$Symbol))

#how much market value
sprintf("%.f",(sum(Semifinal$Market_Value, na.rm = T)))
