rm(list=ls())
setwd("X:/PROJECTS/SEGREGATION METRICS TOOLKIT/")
source("Segregation Metrics.R")


# Get shapefiles

require(maptools)
require(ggplot2)


# Using question on religion from 2011 census

# KS209SC2b

# Minority if Roman Catholic

dta <- read.csv("E:/Census 2011/2ablk_2astd/2ablk/SNS Data Zone/KS209SCb.csv", header=T)

names(dta)[1] <- "areal.unit"
dta.nation <- dta[1,]
dta.dz <- dta[-1,]

dta.dz[,-1] <- apply(dta.dz[,-1], 2, as.numeric)

for (i in 2:dim(dta.dz)[2]){
  x <- dta.dz[,i]
  x[is.na(x)] <- 0
  x <- x+ 0.5
  dta.dz[,i] <- x
}

n <- dta.dz[,"Roman.Catholic"]
N <- dta.dz[,"All.people"]

minority.proportion <- n/N


# Shapefiles downloaded from http://saspac.org/data/2011-census/scotland-2011/

# Lowest level: output areas (42 604 areas)
#OA_shp <- readShapePoly("E:/SNS_Data/All-Scottish-Census-boundaries(shp)/OA_2011_EoR_Scotland.shp")

# Datazones : 6505 areas 
#DZ_shp <- readShapePoly("E:/SNS_Data/All-Scottish-Census-boundaries(shp)/DZ_2011_EoR_Scotland.shp")
DZ_shp <- readShapePoly("E:/SNS/ShapeFiles/SNS_Geography_14_3_2013/DataZone_2001_bdry.shp")

# Shape file location 
shape_file_dir <- "E:/SNS_Data/All-Scottish-Census-boundaries(shp)/"

# Calculating centroids of each polygons
require(rgeos)
centroids <- gCentroid(DZ_shp, byid=T)

# calculating area of each polygon
areas <- gArea(DZ_shp, byid=T)

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

dz.unevenness <- CalcUnevenness(DZ_shp)

# 
# Spatial Data location 
spatial_data_dir <- "E:/SNS/Data/SNS_FullData_CSV_14_3_2013/"


# Splits: 
# 1) Find those ending with ".csv"
# 2) Find first "_" : symbols to the left are category
# 3) 4 digit code to the right of first "-" is numeric code
# 4) Code from first to second "_" is specific name
# 5) 4 digit code from 2nd  to 3rd "_" is year
# 6) 2 digit code after "_" after year is areal unit type

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
    
Files_DF <- ExtractMetadata(spatial_data_dir)




# want something of the following format:

#areal unit type 
#areal unit value
# year
#variable 
# value

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


Data_ZN_2011 <- ExtractSpatialData(
    Files_DF=Files_DF, 
    years.ofinterest="2010",
    arealunits.ofinterest="ZN",
    verbose=F
)


DZ_shp <- readShapePoly(
    paste(
        shape_file_dir,
        "DZ_2011_EoR_Scotland.shp",
        sep=""
    )
)


DZ_map <- fortify(DZ_shp)

# Save things at this point

save(DZ_map, DZ_shp, Data_ZN_2011, Files_DF, file="Objects.RData")



ggplot(
    DZ_map, 
    aes(
        x=long, 
        y=lat, 
        group=group
    )
) + geom_path() + theme_minimal()

ggplot(
  DZ_map, 
  aes(
    x=long, 
    y=lat, 
    group=group  )
) + geom_polygon( alpha=0.5) + theme_minimal() + geom_path()


# Choropleth using a specific variable...
# merge using arealunit.code on Data_ZN_2011 and 
# id on DZ_map

# for now just pick one variable to merge

variable.names <- unique(Data_ZN_2011$variable.names)

Data_short <- subset(
    Data_ZN_2011, 
    subset=Data_ZN_2011$variable.names==variable.names[1], 
    select=c("arealunit.code", "variable.value")
    )

Data_Merged_2011 <- merge(
    DZ_map, 
    Data_short, 
    by.x="id", 
    by.y="arealunit.code"
)

Data_Merged_2011 <-subset(Data_Merged_2011, drop="group")

names(Data_Merged_2011)[1] <- "group"

# put in order
require(plyr)
Data_Merged_2011 <- arrange(Data_Merged_2011, group, order)



ggplot(Data_Merged_2011, 
       aes(x=long, y=lat, group=group, fill=variable.value)) + 
#    coord_map("polyconic") + 
    geom_polygon(colour=NA) + scale_fill_gradient(low="black", high="red")




ggplot(DataSS, 
       aes(x=long, y=lat, group=group, fill=variable.value)) + 
    #    coord_map("polyconic") + 
    geom_polygon(colour="black",alpha=0.5) + scale_fill_gradient(low="black", high="red")



#CA_shp <- readShapePoly("E:/SNS_Data/All-Scottish-Census-boundaries(shp)/CA_2011_EoR_Scotland.shp")
#DC_shp <- readShapePoly("E:/SNS_Data/All-Scottish-Census-boundaries(shp)/DC_2011_EoR_Scotland.shp")
#IZ_shp <- readShapePoly("E:/SNS_Data/All-Scottish-Census-boundaries(shp)/IZ_2011_EoR_Scotland.shp")
#LC_shp <- readShapePoly("E:/SNS_Data/All-Scottish-Census-boundaries(shp)/LC_2011_EoR_Scotland.shp")
#WD_shp <- readShapePoly("E:/SNS_Data/All-Scottish-Census-boundaries(shp)/WD_2011_EoR_Scotland.shp")


#CA_map <- fortify(CA_shp)
#DC_map <- fortify(DC_shp)
#IZ_map <- fortify(IZ_shp)
#LC_map <- fortify(LC_shp)
#OA_map <- fortify(OA_shp)
#WD_map <- fortify(WD_shp)





# ggplot(CA_map, aes(x=long, y=lat, group=group)) + geom_path()
# ggplot(DC_map, aes(x=long, y=lat, group=group)) + geom_path()
# ggplot(IZ_map, aes(x=long, y=lat, group=group)) + geom_path()
# ggplot(LC_map, aes(x=long, y=lat, group=group)) + geom_path()
# ggplot(OA_map, aes(x=long, y=lat, group=group)) + geom_path()
# ggplot(WD_map, aes(x=long, y=lat, group=group)) + geom_path()

## Not run:
## extents and legends
##################################################
hdf <- get_map()

ggmap(hdf, extent = "normal")
ggmap(hdf) # extent = 'panel', note qmap defaults to extent = 'device'
ggmap(hdf, extent = "device")
require(MASS)
mu <- c(-95.3632715, 29.7632836); nDataSets <- sample(4:10,1)
chkpts <- NULL
for(k in 1:nDataSets){
    a <- rnorm(2); b <- rnorm(2); si <- 1/3000 * (outer(a,a) + outer(b,b))
    chkpts <- rbind(chkpts, cbind(mvrnorm(rpois(1,50), jitter(mu, .01), si), k))
}
chkpts <- data.frame(chkpts)
names(chkpts) <- c("lon", "lat","class")
chkpts$class <- factor(chkpts$class)
qplot(lon, lat, data = chkpts, colour = class)
ggmap(hdf, extent = "normal") +
    geom_point(aes(x = lon, y = lat, colour = class), data = chkpts, alpha = .5)
ggmap(hdf) +
    geom_point(aes(x = lon, y = lat, colour = class), data = chkpts, alpha = .5)
ggmap(hdf, extent = "device") +
    geom_point(aes(x = lon, y = lat, colour = class), data = chkpts, alpha = .5)
theme_set(theme_bw())
ggmap(hdf, extent = "device") +
    geom_point(aes(x = lon, y = lat, colour = class), data = chkpts, alpha = .5)
ggmap(hdf, extent = "device", legend = "topleft") +
    geom_point(aes(x = lon, y = lat, colour = class), data = chkpts, alpha = .5)



tt <- tktoplevel()
OK.but <- tkbutton(tt, text="OK", command=function() tkdestroy(tt))

tkgrid(OK.but)

tkfocus(tt)



require(tcktk)
tt <- tktoplevel()
topMenu <- tkmenu(tt)
tkconfigure(tt, menu=topMenu)
fileMenu <- tkmenu(topMenu, tearoff=F)
tkadd(fileMenu, "command", label="Quit", command=function() tkdestroy(tt))
tkadd(topMenu, "cascade", label="File", menu=fileMenu)
tkfocus(tt)


tt <- tktoplevel()
topMenu <- tkmenu(tt)
tkconfigure(tt, menu=topMenu)
fileMenu <- tkmenu(topMenu, tearoff=F)
openRecentMenu <- tkmenu(topMenu, tearoff=F)
tkadd(
    openRecentMenu, 
    "command", 
    label= "Recent File 1", 
    command = function() tkmessageBox(message="I don't know how to open recent file 1", icon="error")
    )
tkadd(
    openRecentMenu,
    "command", 
    label="Recent File 2",
    command=function() tkmessageBox(message="I don't know how to open recent file 2", icon="error")
    )
tkadd(fileMenu, "cascade", label="open recent file", menu=openRecentMenu)
tkadd(fileMenu, "command", label="Quit", command=function() tkdestroy(tt))
tkadd(topMenu, "cascade", label="File", menu=fileMenu)
tkfocus(tt)


tt <- tktoplevel()
txt <- tktext(tt)
tkgrid(txt)

copyText <- function() .Tcl(paste("event", "generate", .Tcl.args(.Tk.ID(txt), "<<Copy>>")))
editPopupMenu <- tkmenu(txt, tearoff=F)
tkadd(editPopupMenu, "command", label="Copy <Ctrl-C>", command=copyText)

RightClick <- function(x, y){
    rootx <- as.integer(tkwinfo("rootx", txt))
    rooty <- as.integer(tkwinfo("rooty", txt))
    
    xTxt <- as.integer(x) + rootx
    yTxt <- as.integer(y) + rooty
    
    .Tcl(paste("tk_popup", .Tcl.args(editPopupMenu, xTxt, yTxt)))
}

tkbind(txt, "<Button-3>", RightClick)
tkfocus(tt)

require(tcltk)

fileName <- tclvalue(tkgetOpenFile())

if (!nchar(fileName)){
    tkmessageBox(message="No file was selected!")
} else {
    tkmessageBox(message=paste("The file selected was", fileName))
}



modalDialog <- function(
    title,
    question,
    entryInit,
    entryWidth          = 20,
    returnValOnCancel   = "ID_CANCEL"
){
    dlg <- tktoplevel()
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    tkwm.title(dlg, title)
    textEntryVarTcl <- tclVar(paste(entryInit))
    textEntryWidget <- tkentry(dlg, width = paste(entryWidth), textvariable=textEntryVarTcl)
    tkgrid(tklabel(dlg, text = "      "))
    tkgrid(tklabel(dlg, text=question), textEntryWidget)
    tkgrid(tklabel(dlg, text= "       "))
    returnVal = returnValOnCancel
    
    onOK <- function(){
        ReturnVal <<- tclvalue(textEntryVarTcl)
        tkgrab.release(dlg)
        tkdestroy(dlg)
        tkfocus(ttMain)
    }
    
    onCancel <- function(){
        ReturnVal <<- returnValOnCancel
        tkgrab.release(dlg)
        tkdestroy(dlg)
        tkfocus(ttMain)
    }
    
    OK.but <- tkbutton(dlg, text= "    OK    ", command=onOK)
    
    Cancel.but <- tkbutton(dlg, text= " Cancel ", command=onCancel)
    tkgrid(OK.but, Cancel.but)
    tkgrid(tklabel(dlg, text = "     "))
    
    tkfocus(dlg)
    tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg); tkfocus(ttMain)})
    tkbind(textEntryWidget, "<Return>", onOK)
    tkwait.window(dlg)
    
    return(ReturnVal)
}

require(tcltk)

ttMain <- tktoplevel()

tktitle(ttMain) <- "ttMain"

launchDialog <- function() {
    ReturnVal <- modalDialog("First Name Entry", "Enter your First Name", "")
    if (ReturnVal=="ID_CANCEL") return()
    tkmessageBox(title = "greeting", 
                 message = paste("Hello, ", ReturnVal, ".", sep=""))
    
}

launchDlg.button <- tkbutton(ttMain, text="Launch Dialog", command= launchDialog)
tkpack(launchDlg.button)
tkdestroy(ttMain)



require(tcltk)
tt <- tktoplevel()

tktitle(tt) <- "Simple Dialogue"

done <- tclVar(0)


OK.but <- tkbutton(tt, text= "   OK    ",
                   command = function() tclvalue(done) <- 1)
Cancel.but <- tkbutton(tt, text = "Cancel",
                       command = function() tclvalue(done) <- 2)

tkgrid(OK.but, Cancel.but)

tkbind(tt, "<Destroy>", function() tclvalue(done) <- 2)

tkfocus(tt)

tkwait.variable(done)

require(tcltk)

tt <- tktoplevel()
cb <- tkcheckbutton(tt)
cbValue <- tclVar("0")
tkconfigure(cb, variable=cbValue)
tkgrid(tklabel(tt, text= "I like R tclTK "), cb)

OnOK <- function()
{
    cbVal <- as.character(tclvalue(cbValue))
    tkdestroy(tt)
    if(cbVal=="1") tkmessageBox(message="So do I!")
    if(cbVal=="0") tkmessageBox(message="You forgot to check the box", icon="warning")
    
}

OK.but <- tkbutton(tt, text="OK", command = OnOK)

tkgrid(OK.but)
tkfocus(tt)


require(tcltk)

tt <- tktoplevel()
rb1 <- tkradiobutton(tt)
rb2 <- tkradiobutton(tt)
rb3 <- tkradiobutton(tt)

rbValue <- tclVar("oranges")

tkconfigure(rb1, variable=rbValue, value="apples")
tkconfigure(rb2, variable=rbValue, value="oranges")
tkconfigure(rb3, variable=rbValue, value="nada")

tkgrid(tklabel(tt, text="Which do you prefer?"))
tkgrid(tklabel(tt, text="Apples "), rb1)
tkgrid(tklabel(tt, text="Oranges "), rb2)
tkgrid(tklabel(tt, text="Nada "), rb3)

OnOK <- function()
{
    rbVal <- as.character(tclvalue(rbValue))
    tkdestroy(tt)
    
    if (rbVal=="apples") tkmessageBox(message="Good choice")
    if (rbVal=="oranges") tkmessageBox(message="Another good choice")
    if (rbVal=="nada") tkmessageBox(message="You'll be hungry", icon="warning")
}

OK.but <- tkbutton(tt, text="OK", command=OnOK)
tkgrid(OK.but)
tkfocus(tt)


tt <- tktoplevel()
Name <- tclVar("Anonymous")
entry.Name <- tkentry(tt, width="20", textvariable=Name)
tkgrid(tklabel(tt, text="Please enter your first name."))
tkgrid(entry.Name)

OnOK <- function()
{
    NameVal <- tclvalue(Name)
    tkdestroy(tt)
    msg <- paste("You have a nice name,", NameVal)
    tkmessageBox(message=msg)
}

OK.but <- tkbutton(tt, text="     OK     ", command=OnOK)
tkbind(entry.Name, "<Return>", OnOK)
tkgrid(OK.but)
tkfocus(tt)

require(tcltk)
tt <- tktoplevel()
tl <- tklistbox(tt, height=4, selectmode="single", background="white")
tkgrid(tklabel(tt, text="What's your favourite fruit?"))
tkgrid(tl)
fruits <- c("Apple", "Orange", "Banana", "Pear")

for (i in 1:4){
    tkinsert(tl, "end", fruits[i])
}

tkselection.set(tl, 2)

OnOK <- function()
{
    fruitChoice <- fruits[as.numeric(tkcurselection(tl))+1]
    tkdestroy(tt)
    msg <- paste("Good choice! ", fruitChoice, "s are delicious!", sep="")
    
    tkmessageBox(message=msg)
}

OK.but <- tkbutton(tt, text= "    OK    ", command=OnOK)
tkgrid(OK.but)
tkfocus(tt)



require(tcltk)
tt <- tktoplevel()
scr <- tkscrollbar(tt, repeatinterval=5, command = function() tkyview(t1, ...))
tl <- tklistbox(tt, height=4, selectmode="single", yscrollcommand=function(...) tkset(scr, ...) , background="white")

tkgrid(tklabel(tt, text="What's your favourite fruit?"))
tkgrid(tl, scr)
tkgrid.configure(scr, rowspan=4, sticky="nsw")
fruits <- c("Apply", "Orange", "Banana", "Pear", "Cherry", "Apricot", "Peach")

for (i in 1:7)
{
    tkinsert(tl, "end", fruits[i])
}

tkselection.set(tl,2)

OnOK <- function()
{
    fruitChoice <- fruits[as.numeric(tkcurselection(tl))+1]
    tkdestroy(tt)
    msg <- paste("Good choice! ", fruitChoice, "s are delicious!", sep="")
    tkmessageBox(message=msg)
}


OK.but <- tkbutton(tt, text="    OK     ", command=OnOK)
tkgrid(OK.but)
tkfocus(tt)




# text windows in R tcltk

tt <- tktoplevel()
txt <- tktext(tt)
tkgrid(txt)
tkmark.set(txt, "insert", "0.0")
tkfocus(txt)


require(tcltk)
tt <- tktoplevel()
txt <- tktext(tt, bg="white", font="courier")
tkgrid(txt)
tkconfigure(txt, state="disabled")
tkinsert(txt, "end", "Hello World")
tkfocus(txt)

require(tcltk)

# Frames
require(tcltk)
tt <- tktoplevel()
frameOverall <- tkframe(tt)
frameUpper <- tkframe(frameOverall, relief="groove", borderwidth=2)
tkgrid(tklabel(frameUpper, text="Text in upper frame"))

frameLower <- tkframe(frameOverall, relief="groove", borderwidth=2)
tkgrid(tklabel(frameLower, text="Text in the lower frame"))

tkgrid(frameUpper)
tkgrid(tklabel(frameOverall, text="Text between the upper and lower frames"))
tkgrid(frameLower)
tkgrid(frameOverall)


# sliders 
require(tcltk)
tt <- tktoplevel()
SliderValue <- tclVar("50")
SliderValueLabel <- tklabel(tt, text=as.character(tclvalue(SliderValue)))
tkgrid(tklabel(tt, text="Slider Value: "), SliderValueLabel, tklabel(tt, text="%"))
tkconfigure(SliderValueLabel, textvariable=SliderValue)
slider <- tkscale(tt, from=100, to=0, showvalue=F, variable=SliderValue, resolution=1, orient="vertical")
tkgrid(slider)
tkfocus(tt)




require(tcltk)

tt <- tktoplevel()
tkwm.title(tt, "Colour selection")
colour <- "blue"

canvas <- tkcanvas(tt, width="80", height="25", bg=colour)
ChangeColour <- function()
{
    colour <- tclvalue(.Tcl(paste("tk_chooseColor", .Tcl.args(initialcolor=colour, title="Choose a colour"))))
    if (nchar(colour)> 0) tkconfigure(canvas, bg=colour)
}
ChangeColour.button <- tkbutton(tt, text="Change colour", command=ChangeColour)
tkgrid(canvas, ChangeColour.button)


# Tree widget
#http://www.sciviews.org/_rgui/tcltk/
#http://www.sciviews.org/_rgui/tcltk/TreeWidget.html
require(tcltk)
tclRequire("BWidget")

tt <- tktoplevel()
tkwm.title(tt, "Tree (Drill-Down) Widget")

xScr <- tkscrollbar(tt, command=function(...) tkxview(treeWidget, ...), orient="horizontal")
yScr <- tkscrollbar(tt, command=function(...) tkyview(treeWidget, ...))
treeWidget <- tkwidget(tt, "Tree", xscrollcommand=function(...) tkset(xScr, ...),
                       yscrollcommand=function(...) tkset(yScr, ...), width=30, height=15)

tkgrid(treeWidget, yScr)
tkgrid.configure(yScr, stick="nsw")
tkgrid(xScr)

tkinsert(treeWidget,"end","root","Record1Node",text="Record 1")
tkinsert(treeWidget,"end","root","Record2Node",text="Record 2")
tkinsert(treeWidget,"end","root","Record3Node",text="Record 3")
tkinsert(treeWidget,"end","root","Record4Node",text="Record 4")

tkinsert(treeWidget,"end","Record1Node","Name1Node",text="Name")
tkinsert(treeWidget,"end","Record2Node","Name2Node",text="Name")
tkinsert(treeWidget,"end","Record3Node","Name3Node",text="Name")
tkinsert(treeWidget,"end","Record4Node","Name4Node",text="Name")

tkinsert(treeWidget,"end","Record1Node","Age1Node",text="Age")
tkinsert(treeWidget,"end","Record2Node","Age2Node",text="Age")
tkinsert(treeWidget,"end","Record3Node","Age3Node",text="Age")
tkinsert(treeWidget,"end","Record4Node","Age4Node",text="Age")

tkinsert(treeWidget,"end","Name1Node","Name1Val",text="Fred")
tkinsert(treeWidget,"end","Name2Node","Name2Val",text="Jane")
tkinsert(treeWidget,"end","Name3Node","Name3Val",text="Tim")
tkinsert(treeWidget,"end","Name4Node","Name4Val",text="Alex")

tkinsert(treeWidget,"end","Age1Node","Age1Val",text="14")
tkinsert(treeWidget,"end","Age2Node","Age2Val",text="35")
tkinsert(treeWidget,"end","Age3Node","Age3Val",text="63")
tkinsert(treeWidget,"end","Age4Node","Age4Val",text="52")

##################### Extracting centroid info from polygons in SP format



DZ_shp[1,]@polygons -> x
x[[1]] -> x2
x2
x2@Polygons -> x3
x3[[1]] -> x4
x4@coords


polygon(Z[,1], Z[,2], type="l")
Z <- as.data.frame(Z)
names(Z) <- c("x", "y")
p <- ggplot(Z, aex(x=x, y=y)) + geom_polygon(aes(fill="grey"))
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(fill="grey"))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(fill=grey))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(fill='grey'))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(fill='grey30'))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(color='grey30'))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(color='grey'))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(color='grey', linetype=NULL))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(color='grey', line=NULL))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(color='grey', linetype=0))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon(aes(color='grey'))
p
qplot(x, y, data=Z)
qplot(x, y, data=Z) + geom_polygon()
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon()
p
?ComputePolyCentroids
?calcCentroid
require(maptools)
?calcCentroid
require(PBSmapping)
install.packages(PBSmapping)
install.packages("PBSmapping")
require("PBSmapping")
?calcCentroid
Z2 <- as.PolySet(Z)
require(rgeos)
install.packages("rgeos")
require(rgeos)
gArea(x)
gArea(x2)
gArea(x3)
gArea(x4)
gArea(Z)
class(x)
class(DZ_shp)
gArea(DZ_shp)
gArea(DZ_shp, byid=T)
plot(density(gArea(DZ_shp, byid=T)))
plot(density(log(gArea(DZ_shp, byid=T))))
plot(density(log(gArea(DZ_shp[1,], byid=T))))
gArea(DZ_shp[1,], byid=T)
gCentroid(DZ_shp[1,])
gCentroid(DZ_shp[1,]) -> centroid
p
p + geom_point(aes(x=x, y=y, data=centroid))
centroid
centroid$x
as.data.frame(centroid)
p + geom_point(aes(x=x, y=y, data=as.data.frame(centroid)))
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon()
p <- p + geom_point(aes(x=centroid$x, y=centroid$y))
p
p <- p + geom_point(aes(x=centroid$x, y=centroid$y, col="red"))
p
p <- p + geom_point(aes(x=centroid$x, y=centroid$y, fill="red"))
p
p <- p + geom_point(aes(x=centroid$x, y=centroid$y, col=red))
p
p <- p + geom_point(aes(x=centroid$x, y=centroid$y, col='red'))
p
p <- ggplot(Z, aes(x=x, y=y)) + geom_polygon()
p
p + layer(data=centroid, geom="point", geom_params=list(color="red"))
p + layer(data=as.data.frame(centroid), geom="point", geom_params=list(color="red"))
