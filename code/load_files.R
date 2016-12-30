################################################################################ 
# Clear R environment
rm(list=ls())

# Initilise master dataframe
# Master dataframe header
master <- data.frame(Date_And_Time = "", 
                     Time_stamp = "",	
                     Barometric_pressure = "",
                     Phase = "",
                     Temp = "",
                     Air_saturation = "",
                     Population = "",
                     Reading = "",
                     Vial_name = "", 
                     File_name = "")
master <- master[-1,]

# Create a matrix of Populations
Population_list <- list.files("input/")

# Set Population
for (count_p in 1:length(Population_list)) {
  
  # Set Population name
  Population <- Population_list[count_p]
  
  # Set input directory where the Population files are stored
  directory <- paste("input/", Population, sep = "")
  
  # Create list of the files for selected Population
  file_list <- list.files(directory)
  
  # Intitialise Reading
  Reading <- 1
  
  # Loop through files in Population folder
  for (count_f in 1:length(file_list)) {
    
    # Initialise the temporary data frame for the file
    # Current file header
    header_current_file  <- c("Date_And_Time", "Time_stamp",	"Barometric_pressure", 
                              "Phase", "Temp", "Air_saturation")

    data_current_file <- read.table(paste(directory, "/", file_list[count_f], sep = ""),
                                    header = FALSE,
                                    stringsAsFactors=FALSE,
                                    sep= "\t",
                                    skip = 10)
    
    if(ncol(data_current_file) > 6) {
      data_current_file <- data_current_file[,1:6]
    }
    
    names(data_current_file) <- header_current_file

    # Append the Population column
    data_current_file <- cbind(data_current_file, Population)

    # Append the Reading ID
    data_current_file <- cbind(data_current_file, Reading)

    # Append the file name
    File_name = file_list[count_f]
    data_current_file <- cbind(data_current_file, File_name)

    # Determine the vial name
    Vial_name <- vector(length = nrow(data_current_file))
    check <- FALSE
    vial_count <- 1
    for (count_v in 1:nrow(data_current_file)) {
      if (data_current_file$Phase[count_v] != "NaN") {
        if (vial_count < 10) {
          Vial_name[count_v] <- paste("Vial_0", vial_count, sep = "")
        }
        else { # NEED TO ADD ERROR IF GREATER THAN 10
          Vial_name[count_v] <- paste("Vial_", vial_count, sep = "")
        }
        check <- TRUE
      }
      else if (data_current_file$Phase[count_v] == "NaN" & check == FALSE) {
        Vial_name[count_v] <- NaN
      }
      else if (data_current_file$Phase[count_v] == "NaN" & data_current_file$Phase[count_v-1] == "NaN") {
        Vial_name[count_v] <- NaN
      }
      else {
        Vial_name[count_v] <- NaN
        vial_count <- vial_count + 1
      }
      rm(count_v)
    }

    # Append the vial name
    data_current_file <- cbind(data_current_file, Vial_name)
    
    # Remove rows with NaN
    data_current_file <- na.omit(data_current_file)

    # Append temp data frame to master
    master <- rbind(master, data_current_file)

    # Close the loop neatly
    Reading <- Reading + 1
    rm(count_f)
  }
  
  rm(count_p)
}

# Create sequence number


# Save file to csv
write.csv(master, file = "output/master.csv", row.names = FALSE)



