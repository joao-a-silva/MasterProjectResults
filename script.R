getMac <- function(dir){
    fileMac <- gsub(" ", "", paste(dir, "/tables_MAC"))
  #  print(fileMac)
    mac <- read.csv(fileMac, header = FALSE)
}

getMic <- function(dir){
    fileMic <- gsub(" ", "", paste(dir, "/tables_MIC"))
   # print(fileMic)
    mic <- read.csv(fileMic, header = FALSE)
}

getTime <- function(dir){
    fileTime <- gsub(" ", "", paste(dir, "/tables_Time"))
    #print(fileTime)
    time <- read.csv(fileTime,  sep=";", header = FALSE)
}

getNC <- function(dir){
    fileNC <- gsub(" ", "", paste(dir, "/tables_NewClass"))
   # print(fileNC)
    #print("*************************************")
    nc <- read.csv(fileNC, header = FALSE)
    
}

getDatFrame <- function(dir){
  #  print(dir)
    dataFrame <- data.frame(x1 = character(), x2 = numeric(), x3 = numeric(), x4 =  numeric(),x5 = numeric(), x6 = numeric(), x7 = numeric())
    
    listDir <- list.dirs(dir)
    #print(listDir)
    algs <- read.csv("algorithms", header = FALSE )

    j <- 1
    for (dir1 in listDir[c(2,4,6)]) {
        
        #print(dir1)
        dir1 <- gsub(" ", "", paste(dir1, "/tablesMetricsTime"))
       # print(dir1)
        mac <- getMac(dir1)
        mic <- getMic(dir1)
        time <- getTime(dir1)
        nc <- getNC(dir1)
        
        i = 1
        
        while(i < length(mac$V1)+1){
            dataFrame <- rbind(dataFrame, data.frame(algs$V1[i], mac$V1[i], mic$V1[i], time$V1[i], time$V2[i], nc$V1[i], j))
            i <- i + 1
            
        }
        j <- j+4
    } 
    
    names <- c("Algorithm", "MacroF1", "MicroF1", "TrainTime", "TestTime", "NumNewClasses", "Perc")  
    colnames(dataFrame) <- names
    dataFrame
}

plotMacMic <- function(data, base, perc){
    algs2 <- read.csv("algorithms2", header = FALSE )

    mat <- data.matrix(data[,2:3])
    rownames(mat) <- data$Algorithm
    mat <- t(mat)
    title <- paste("Macro e Micro F1: Base de dados ", base, "com ", perc*10 ,"% de treino.") 
    layout(rbind(1,2), heights=c(12,1)) 
    par(oma=c(0,0,1,0)) 
    x <- barplot(mat, beside=TRUE, col=c("steelblue4","tomato"), lwd =2, ylab = " % ",
                 las=2, yaxp=c(0,100,10), cex.names = 0.8,  xaxt = "n")
    
    title(title, cex.main=0.9, outer = TRUE)
    labs <- paste(algs2$V1, " ")
  
    text(cex=0.7, x=x+1.5, labels = labs, xpd=TRUE, srt=20, pos=2, y= -2.0, )
    abline(h=max(data[,3]), col= "red", lty=2)
    abline(h=max(data[,2]), col= "blue", lty=2)
    par(mar=c(0, 0, 1, 0), xpd=TRUE)
    plot.new()
    legend('bottom', rownames(mat), fill =c("steelblue4","tomato"), horiz = TRUE, cex=0.8 )
    
}

plotTime <- function(data, base, perc){
    algs2 <- read.csv("algorithms2", header = FALSE )
    
    mat <- data.matrix(data[,4:5])
    rownames(mat) <- data$Algorithm
    mat <- t(mat)
    title <- paste("Tempo: Base de dados ", base, "com ", perc*10 ,"% de treino.") 
    layout(rbind(1,2), heights=c(12,1)) 

    par(oma=c(0,0,1,0)) 
    h <- max(mat[1,]) + max(mat[2,])
    print(h)
    x <- barplot(mat,  col=c("steelblue4","tomato"), lwd =2, ylab = " Tempo (seg) ",
                 las=2,  cex.names = 0.8,  xaxt = "n", yaxp=c(0, trunc(h)+1, 1) )
    
    
    title(title, cex.main=0.9, outer = TRUE)
    labs <- paste(colnames(mat), " ")
    text(cex=0.7, x=x+.5, labels = labs, xpd=TRUE, srt=20, pos=2, y= 0 )
    par(mar=c(0, 0, 1, 0), xpd=TRUE)
    plot.new()
    legend('bottom', rownames(mat), fill =c("steelblue4","tomato"), horiz = TRUE, cex=0.8 )
    
}

# plotTime <- function(data, base, perc){
#     mat <- data.matrix(data[,4:5])
#     rownames(mat) <- data$Algorithm
#     mat <- t(mat)
#     title <- paste("Tempo: Base de dados ", base, "com ", perc*10 ,"% de treino.") 
#     barplot(mat, col=c("steelblue4","tomato"), ylab = "Tempo (s)", las=2)
#     title(title, line = 3)
#     legend('top', rownames(mat), fill =c("steelblue4","tomato") )
# }