

#this line lists all the csv files from "Data"
csv_files <- list.files(path = "Data", pattern = "\\.csv$", full.names = TRUE)
     

    
length(csv_files)

#print("This is the length of the *csv files in Data/
#      LENGTH:", length(csv_files))

#this grabs the data from the Data/wingspan vs mass csv file, and stores it in df
df <- read.csv("Data/wingspan_vs_mass.csv")

head(df) #this head() from what I can tell shows the first 6 rows of the data frame
str(df) # this str() shows the structure of the data frame


#inspect the first 5 lines 
head(df, 5)

head(df, 20) #inspect first 20 lines

#this is hint for 9. on Assngm 2 
#for i in *df
#for> do head -1 $i
#for> done

#use the list.files thingy that finds and lists all the names of relative files
i_files <- list.files(path = "Data", pattern = "^i", recursive = TRUE, full.names = TRUE)

#now run it (this finds all the files considering lowercase i)
i_files

#now run it for lowercase b 
b_files <- list.files(path = "Data", pattern = "^b", recursive = TRUE, full.names = TRUE)

#run it now
b_files

#now refer to classroom hints on how to achieve. 
for (file in b_files){
  first_b <- readLines(file, n = 1)
  print (first_b)
}

#easy copy paste from our for for lowercase b files
for (file in csv_files){
  first_csv_line <- readLines(file, n = 1)
  print(first_csv_line)
}

#these loops are efficient because they only count up to the amount
# of files from the variable, starting at n = 1. 

#now we know how to look at the first lines using readLines, as well as
# transferring variables into for loops for different fuctions. 
