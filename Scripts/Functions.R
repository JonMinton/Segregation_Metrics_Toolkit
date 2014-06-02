
# bespoke function for calculating boundary edge length of each polygon

CalcEdgeLengths <- function(In){
    
    N <- dim(In)[1]
    out <- vector("numeric", N)
    
    for (i in 1:N){
        tmp <- In[i,]@polygons[[1]]@Polygons[[1]]@coords
        tmp2 <- tmp[-1,] - tmp[-dim(tmp)[1],]
        tmp3 <- (tmp2[,2]^2 + tmp2[,1]^2)^(1/2)
        out[i] <- sum(tmp3)
    }
    
    return(out)  
}





# find ratio of edge length to polygon area (Length: Area Ratio LAR)

CalcLar <- function(In){
    edges <- CalcEdgeLengths(In)
    N <- dim(In)[1]
    out <- vector("numeric", N)
    
    for (i in 1:N){
        out[i] <- In[i,]@polygons[[1]]@Polygons[[1]]@area
    }
    
    out <- edges/out
    return(out)
}


# Calculate mean distance from each vertex to centroid

CalcUnevenness <- function(In){
    require(rgeos)
    centroids <- gCentroid(In, byid=T)
    
    N <- dim(In)[1]
    
    out.means <- vector("numeric", N)
    out.vars <- vector("numeric", N)
    
    for (i in 1:N){
        this.coords <- In[i,]@polygons[[1]]@Polygons[[1]]@coords
        this.coords[,1] <- this.coords[,1] - centroids$x[i]
        this.coords[,2] <- this.coords[,2] - centroids$y[i]
        this.dists <- (this.coords[,1]^2 + this.coords[,2]^2)^(1/2)
        
        out.means[i] <- mean(this.dists)
        out.vars[i] <- var(this.dists)
    }
    
    out.ratio <- out.vars / out.means
    
    return(list(
        ratio=out.ratio,
        mean=out.means,
        var=out.vars)
    )
}


ExtractMetadata <- function(directory.location, verbose=F){
    x <- dir(directory.location)
    # 1) Find those ending with ".csv"
    N <- length(x)
    
    Files_DF <- data.frame(filename=x, group=NA, numcode=NA, subgroup=NA, year=NA, cpart=NA, rpart=NA, qtr=NA, arealunit=NA, valid=F)
    
    Files_DF$valid[grep(".csv$", x)] <- T
    
    for (i in 1:N){
        if (Files_DF$valid[i]){
            tmp <- strsplit(x[i], "\\.")[[1]][1] # only keep the bit before .csv
            tmp <- strsplit(tmp, "_")[[1]]
            
            # count number of splits
            
            if (length(tmp)==8){
                # pattern 1 found?
                if(verbose) cat("Pattern 1 found in item ", i, "\n")
                
                grp <- tmp[1]
                if (verbose) cat("group:\t", grp, "\n")
                Files_DF$group[i] <- grp
                
                tmp2 <- tmp[2] 
                if (nchar(tmp2)==4){
                    tmp2 <- as.numeric(tmp2)
                    if(!is.na(tmp2)){ # will throw up a NA if can't be converted, i.e. not all digits
                        if (verbose) cat("numcode:\t", tmp2, "\n")
                        Files_DF$numcode[i] <- tmp2
                    }
                }
                
                # next part should be subgroup
                Files_DF$subgroup[i] <- tmp[3]
                if (verbose) cat("subgroup:\t", tmp[3], "\n")
                
                # next part should be 2 digit code
                
                tmp2 <- tmp[4]
                if (nchar(tmp2)==2){
                    Files_DF$arealunit[i] <- tmp2
                    if (verbose) cat("arealunit:\t", tmp2,"\n")
                }
                
                # next part should be something along the lines of CXRX
                
                tmp2 <- tmp[5]
                
                tmp3 <- as.numeric(substr(tmp2, 2,2))
                tmp4 <- as.numeric(substr(tmp2, 4,nchar(tmp2)))
                
                if (!is.na(tmp3) & !is.na(tmp4)){
                    Files_DF$cpart[i] <- tmp3
                    Files_DF$rpart[i] <- tmp4
                }  
                
                
            }
            
            if (length(tmp)==9){
                # pattern 2 found?
                if (verbose) cat("Pattern 2 found in item ", i, "\n")
                
                grp <- tmp[1]
                if (verbose) cat("group:\t", grp, "\n")
                Files_DF$group[i] <- grp
                
                tmp2 <- tmp[2] 
                if (nchar(tmp2)==4){
                    tmp2 <- as.numeric(tmp2)
                    if(!is.na(tmp2)){ # will throw up a NA if can't be converted, i.e. not all digits
                        if (verbose) cat("numcode:\t", tmp2, "\n")
                        Files_DF$numcode[i] <- tmp2
                    }
                }
                
                # next part should be subgroup
                Files_DF$subgroup[i] <- tmp[3]
                if (verbose) cat("subgroup:\t", tmp[3], "\n")
                
                
                # Next part should be year 
                # (oR year + quarter)
                
                tmp2 <- tmp[4]
                if (nchar(tmp2)==4){
                    tmp3 <- as.numeric(tmp2)
                    if (!is.na(tmp3)){
                        Files_DF$year[i] <- tmp3
                    }
                } else if (nchar(tmp2)==6){
                    tmp3 <- strsplit(tmp2, "Q")[[1]]
                    tmp4 <- tmp3[1]
                    tmp5 <- tmp3[2]
                    
                    if (nchar(tmp4)==4){
                        tmp4 <- as.numeric(tmp4)
                        if (!is.na(tmp4)){
                            Files_DF$year[i] <- tmp4
                        }
                    }
                    if (nchar(tmp5)==1){
                        tmp5 <- as.numeric(tmp5)
                        if (!is.na(tmp5)){
                            Files_DF$qtr[i] <- tmp5
                        }
                    }
                    
                }
                # next part should be 2 digit code
                
                tmp2 <- tmp[5]
                if (nchar(tmp2)==2){
                    Files_DF$arealunit[i] <- tmp2
                    if (verbose) cat("arealunit:\t", tmp2,"\n")
                }
                
                # next part should be something along the lines of CXRX
                
                tmp2 <- tmp[6]
                
                
                tmp3 <- as.numeric(substr(tmp2, 2,2))
                tmp4 <- as.numeric(substr(tmp2, 4,nchar(tmp2)))
                
                if (!is.na(tmp3) & !is.na(tmp4)){
                    Files_DF$cpart[i] <- tmp3
                    Files_DF$rpart[i] <- tmp4
                }  
            }
            
        }
    }
    
    return(Files_DF)    
}


ExtractSpatialData <- function(
    Files_DF=Files_DF,
    arealunits.ofinterest="ZN",
    years.ofinterest=c("2001", "2003"),
    variables.ofinterest=NULL, 
    verbose=F
)
{
    N.files <- dim(Files_DF)[1]
    
    DF.output <- data.frame(
        arealunit.type=c(),
        arealunit.code=c(),
        variable.year=c(),
        variable.name=c(),
        variable.value=c()
    )
    
    for (f in 1:N.files){
        if (verbose) cat("row:\t", f, "\n")
        arealunit.this <- Files_DF[f,"arealunit"]
        
        if (arealunit.this %in% arealunits.ofinterest){
            filename.this <- Files_DF[f,"filename"]
            
            Data.this <- read.csv(
                paste(
                    spatial_data_dir, 
                    filename.this, 
                    sep=""
                ),
                header=T
            )
            
            tmp     <-  names(Data.this)
            vars    <-  tmp[-1] # first name is X as the contents from the csv file for this cell is empty
            if (verbose) cat("There are ", length(vars), " in file ", f, "\n")
            years   <-  Data.this[1,-1] # First column in row is 'GeographyCode', so not relevant
            
            
            if (Files_DF[f,"arealunit"]=="ZN"){
                if (verbose) cat("Row ", f, " is ZN\n")
                arealunit.type      <- "ZN"
                arealunit.code      <- substr(Data.this[-1,1], 6,9)  
            }
            
            if (Files_DF[f,"arealunit"]=="SC"){
                if (verbose) cat("Row ", f, " is SC\n")
                arealunit.type      <- "SC"
                arealunit.code      <- Data.this[-1,1] # should just be 420
            }
            
            
            
            for (i in vars){
                year.this <- Data.this[1,i]
                values.this <- Data.this[-1,i]
                
                if (!is.null(variables.ofinterest)){
                    if (i %in% variables.ofinterest){
                        if (year.this %in% years.ofinterest){
                            df.var.this <- data.frame(
                                arealunit.type      =       arealunit.type, 
                                arealunit.code      =       arealunit.code,
                                variable.year       =       year.this,
                                variable.names      =       i,
                                variable.value      =       values.this
                            )
                            DF.output <- rbind(
                                DF.output, 
                                df.var.this
                            )   
                        }
                    } 
                } else {
                    if (year.this %in% years.ofinterest){
                        df.var.this <- data.frame(
                            arealunit.type      =       arealunit.type, 
                            arealunit.code      =       arealunit.code,
                            variable.year       =       year.this,
                            variable.names      =       i,
                            variable.value      =       values.this
                        )
                        
                        DF.output <- rbind(
                            DF.output, 
                            df.var.this
                        ) 
                    }
                }
                
            }      
        }
    }
    return(DF.output)
}

UserSelector <- function(){
    require(tcltk)
    
    variable.groups <- sort(unique(Files_DF$group))
    
    selection.group <- tk_select.list(
        choices=variable.groups, 
        multiple=T, 
        title="Pick variable groups"
    )
    
    # Now find subgroups without group
    
    variable.subgroups <- sort(unique(Files_DF$subgroup[Files_DF$group %in% selection.group]))
    
    selection.subgroup <- tk_select.list(
        choices=variable.subgroups,
        multiple=T,
        title="Pick variable subgroups"
    )
    
    variable.arealunits <- sort(unique(Files_DF$arealunit[Files_DF$subgroup %in% selection.subgroup]))
    
    selection.arealunit <- tk_select.list(
        choices=variable.arealunits,
        multiple=T,
        title="Pick variable subgroups"
    )
    
    
    selection.files <- as.character(
        Files_DF$filename[
            Files_DF$group %in% selection.group & 
                Files_DF$subgroup %in% selection.subgroup & 
                Files_DF$arealunit %in% selection.arealunit
            ]
    )
    
    
    
    return(list=c(
        group=selection.group, 
        subgroup=selection.subgroup,
        arealunit=selection.arealunit,
        files=selection.files
    )
    )
}


Dissimilarity.compute <- function(minority, total, n.boot=1000)
{
    #### Compute the dissimilarity index
    proportion <- minority / total
    proportion.overall <- sum(minority) / sum(total)
    Total <- sum(total)
    D <- sum(total * abs(proportion - proportion.overall)) / (2 * Total * proportion.overall * (1-proportion.overall))    
    
    #### Compute bootstrapped estimates of uncertainty
    D.boot <- rep(NA, n.boot)
    n <- length(total)
    for(j in 1:n.boot)
    {
        ## Generate a bootstrap sample
        samp <- sample(x=1:n, size=n, replace=TRUE)
        minority.boot <- minority[samp]
        total.boot <- total[samp]
        
        ## Compute D based on this sample
        proportion.boot <- minority.boot / total.boot
        proportion.overall.boot <- sum(minority.boot) / sum(total.boot)
        Total.boot <- sum(total.boot)
        D.boot[j] <- sum(total.boot * abs(proportion.boot - proportion.overall.boot)) / (2 * Total.boot * proportion.overall.boot * (1-proportion.overall.boot))    
    }
    D.estimate <- c(D, quantile(D.boot, c(0.025, 0.975)))
    results <- list(D.estimate=D.estimate, D.boot=D.boot)
    return(results)
}

