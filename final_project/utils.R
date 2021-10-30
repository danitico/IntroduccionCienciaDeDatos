# CODE from package RKEEL

read.keel <- function(file){
    
    text <- readLines(file)
    
    i <- 1
    while(!grepl("@attribute", tolower(text[i]))){
        i <- i+1
    }
    #now text is first @attribute
    attributeNames <- c()
    attributeTypes <- c()
    
    while(grepl("@attribute", tolower(text[i]))){
        #Obtain attribute i name
        attributeNames <- c(attributeNames, gsub("\'", "", strsplit(text[i], "[ {]")[[1]][2]))
        
        if(grepl("\\{", text[i])){
            #If line contains "{", attribute is categorical
            attributeTypes <- c(attributeTypes, "categorical")
        }
        else{
            #real or integer attribute
            attributeTypes <- c(attributeTypes, strsplit(text[i], " ")[[1]][3])
        }
        
        i <- i+1
    }
    
    
    
    outputs <- -1
    
    while(!grepl("@data", tolower(text[i]))){
        if(grepl("@outputs", tolower(text[i]))){
            outputAttribute <- gdata::trim(strsplit(text[i], " ")[[1]][2])
            
            for(j in 1:length(attributeNames)){
                if(grepl(outputAttribute, attributeNames[j])){
                    outputs <- j
                }
            }
            
            if(outputs == -1){
                stop("Output attribute don't found")
            }
            
        }
        i <- i+1
    }
    i <- i+1
    #now text is first data line
    
    data <- c()
    
    #num of rows
    row <- 0
    
    while(!is.na(text[i])){
        #Split data by commas
        dataWords <- strsplit(text[i], ",")[[1]]
        
        #If words are <= 0, there are no more data
        if(length(dataWords) > 0){
            dataLine <- c()
            
            #Create data line depending on attribute type
            for(j in 1:length(dataWords)){
                if(dataWords[j] == '?'){
                    #stop("NA")
                    dataLine <- c(dataLine, "NA")
                }
                else if(grepl("integer", attributeTypes[j])){
                    dataLine <- c(dataLine, strtoi(gdata::trim(dataWords[j])))
                }
                else if(grepl("real", attributeTypes[j])){
                    dataLine <- c(dataLine, as.double(gdata::trim(dataWords[j])))
                }
                else if(grepl("categorical", attributeTypes[j])){
                    dataLine <- c(dataLine, gdata::trim(dataWords[j]))
                }
                else{
                    stop("Type not found")
                }
            }
            
            #Add data line to full data
            data <- c(data, dataLine)
            row <- row+1
        }
        
        i <- i+1
        
    }
    
    #Create data matrix
    m <- matrix(data, nrow = row, ncol=length(attributeNames), byrow = TRUE)
    #Set column names
    colnames(m) <- attributeNames
    
    #If output is not last attribute, change columns (in matrix and attribute names and types)
    if((outputs != -1) && (outputs < length(attributeNames))){
        m2 <- m
        attributeTypes2 <- attributeTypes
        attributeNames2 <- attributeNames
        
        j <- 1
        for(i in 1:length(attributeNames)){
            if(i != outputs){
                m[,j] <- m2[,i]
                attributeTypes[j] <- attributeTypes2[i]
                attributeNames[j] <- attributeNames2[i]
                j <- j+1
            }
        }
        m[,length(attributeNames)] <- m2[,outputs]
        attributeTypes[length(attributeNames)] <- attributeTypes2[outputs]
        attributeNames[length(attributeNames)] <- attributeNames2[outputs]
    }
    
    #Generate data.frame
    df <- data.frame(m)
    
    #Convert categorical data to character
    for(i in 1:length(attributeTypes)){
        if(attributeTypes[i] == "categorical"){
            df[,i] <- as.character(df[,i])
        }
    }
    
    #Class column as factor
    df[ncol(df)] <- as.factor(df[ncol(df)][[1]])
    
    return(df)
}

getMode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}
