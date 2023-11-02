library(xml2)

folders <- list.files(path = "/Users/apple/UH-CPL/ACEs/data_scholars/fundings_data_NSF/", full.names = T)

result <- data.frame(FY = integer(), # folder
                     ORG_NAME = character(), # 
                     ORG_STATE = character(), # <StateCode>
                     PI_FNAME = character(), #
                     PI_LNAME = character(), #
                     TOTAL_COST = integer()) # 

for (folder in folders) {
    files <- list.files(path = folder, full.names = T)
    print(paste("Working folder: ", folder))
    for (file in files) {
        err <- FALSE
        tryCatch({x <- read_xml(file)}, error = function(err) {err <- TRUE})
        if (!err) {
            temp <- data.frame(
                
                # Note the difference between .// and //
                # //  finds anywhere in the document (ignoring the current node)
                # .// finds anywhere beneath the current node
                
                tryCatch({as.integer(substr(folder, nchar(folder)-3, nchar(folder)))}, error = function(err) {return(NA)}),
                
                #tryCatch({xml_text(xml_child(xml_child(xml_child(x, 1), 15), 1))}, error = function(err) {return(NA)}),            
                tryCatch({xml_text(xml_find_first(xml_find_first(x, ".//Institution"), ".//Name"))}, error = function(err) {return(NA)}),
                
                #tryCatch({xml_text(xml_child(xml_child(xml_child(x, 1), 15), 8))}, error = function(err) {return(NA)}),
                tryCatch({xml_text(xml_find_first(xml_find_first(x, ".//Institution"), ".//StateCode"))}, error = function(err) {return(NA)}),
                
                #tryCatch({xml_text(xml_child(xml_child(xml_child(x, 1), 14), 1))}, error = function(err) {return(NA)}),
                tryCatch({xml_text(xml_find_first(xml_find_first(x, ".//Investigator"), ".//FirstName"))}, error = function(err) {return(NA)}),
                
                #tryCatch({xml_text(xml_child(xml_child(xml_child(x, 1), 14), 2))}, error = function(err) {return(NA)}),
                tryCatch({xml_text(xml_find_first(xml_find_first(x, ".//Investigator"), ".//LastName"))}, error = function(err) {return(NA)}),
                
                #tryCatch({as.integer(xml_text(xml_child(xml_child(x, 1),4)))}, error = function(err) {return(NA)})
                tryCatch({as.integer(xml_text(xml_find_first(x, ".//AwardTotalIntnAmount")))}, error = function(err) {return(NA)})
            )
            colnames(temp) <- c("FY", "ORG_NAME", "ORG_STATE", "PI_FNAME", "PI_LNAME", "TOTAL_COST")
            result <<- rbind(result, temp)
        }
    }
}

summary(result$TOTAL_COST)
