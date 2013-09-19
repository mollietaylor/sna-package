library(plyr)

if(!exists("sampledata")) {
  system.time(sampledata <- read.csv("sampledata.csv",
    header = TRUE,
    colClasses = c("character", "character", "character", "factor", "factor", 
      "character", "integer", "integer", "character", "character", "character",
      "factor", "integer", "integer")))
  sampledata$Location <- NULL
  sampledata <- rename(sampledata,
    c(csacode = "Location"))
}



firstAppYear <- 2010
lastAppYear <- 2010
categories <- c("All",
  "Chemical", 
  "Computers & Communications",
  "Drugs & Medical", 
  "Electrical & Electronic", 
  "Mechanical", 
  "Others")
# categories <- "All"
# codes <- unique(sampledata$Location[which(is.na(sampledata$Location) == FALSE)]) # for ALL codes
# codes <- unique(sampledata$Location[which(is.na(sampledata$Location) == FALSE & sampledata$Location < 1000)]) # for ALL 3-digit codes
codes <- c(408,348,176,548,148,488,206,428,288,122,220,500,378,
  216,184,476,422,472,430,172,178,312,294,198,332,126,376,450,
  482,400,268,350,278,416,266,273,160,406,142,104,33100,38060,
  41740,45300,38900,41700,47260,27260,32820,40060)

# codes <- 332

for (category in categories) {

  system.time(for(locale in codes) {


    ######## NarrowData:
      # limit large file to locale, years, and category:
      localdata <- sampledata[which(sampledata$Location == locale),]
      if(category != "All") {
        localdata <- localdata[which(localdata$Category == category),]
      }
      localdata <- localdata[which(localdata$AppYearStr >= firstAppYear & localdata$AppYearStr <= lastAppYear),]
    ########

    numslocaldata <- data.frame(Patent = unique(localdata$Patent))



    # # create a variable with name of file:
    # categ <- sub(" .*", "", category)
    # outfileinvs <- paste("collabs by csa/", locale, "-", categ, "-", firstAppYear, "thru", lastAppYear, ".csv",
    #   sep = "")

    # merge
    invs <- merge(sampledata,
      numslocaldata,
      by.x = "Patent")
    
    # # create file with all inventors (local & nonlocal) who worked on local patents:
    # write.table(invs, #  (change write location)
    #   file = outfileinvs,
    #   quote = FALSE,
    #   sep = ",",
    #   row.names = FALSE,
    #   col.names = TRUE)

    # find CSA locations of collaborations:
    library(plyr)
    ### FOR BLOG: plyr
    collabLocs <- ddply(invs, 
      .(Location), 
      nrow)
    names(collabLocs)[2] <- "Collabs"
    local <- invs[which(invs$Location == locale),]
    # correct locale's value for collabLocs:
    collabLocs$Collabs[collabLocs$Location == locale] <- nrow(local[(duplicated(local$Patent)),])
    # sort:
    if(length(collabLocs$Location) != 0) {
      collabLocs <- collabLocs[order(-collabLocs$Collabs),]
    }
    collabLocs$Location[is.na(collabLocs$Location)] <- 0
    if(length(collabLocs$Location) == 0) {
      locMaxCollabLocs <- NA
      numMaxCollabLocs <- 0
    } else if(collabLocs$Location[collabLocs$Collabs == max(collabLocs$Collabs)] == locale) {
      locMaxCollabLocs <- collabLocs[2,1]
      numMaxCollabLocs <- collabLocs[2,2]
    } else{
      locMaxCollabLocs <- collabLocs[1,1]
      numMaxCollabLocs <- collabLocs[1,2]
    }

    # create file with only local inventors who worked on local patents:
    outfilelocal <- paste("local collabs by csa/", locale, "-", categ, "-", firstAppYear, "thru", lastAppYear, ".csv",
      sep = "")
    
    write.table(local, # (change write location)
      file = outfilelocal,
      quote = FALSE,
      sep = ",",
      row.names = FALSE,
      col.names = TRUE)

    # find lonewolves:
    if(length(invs$Patent) != 0) {
      numInvsEachPatent <- as.data.frame(table(invs$Patent))
      lonewolves <- nrow(numInvsEachPatent[numInvsEachPatent$Freq == 1,])
    } else {
      lonewolves <- 0
    }

    # find frequency of location of collaborations:
    localCollabs <- ifelse(length(collabLocs$Location) != 0, 
      collabLocs$Collabs[collabLocs$Location == locale],
      0)
    totalCollabs <- ifelse(length(collabLocs$Location) != 0, 
      sum(collabLocs$Collabs), 
      0)
### subtract NACollabs:
    nonlocalCollabs <- totalCollabs - 
      localCollabs -
      ifelse(length(collabLocs$Collabs[collabLocs$Location == 0]) != 0,
          collabLocs$Collabs[collabLocs$Location == 0],
          0)

    # invLocs: number of inventors in each location
    invLocs <- ddply(invs[!duplicated(invs$lower),],
      .(Location),
      nrow)
    names(invLocs)[2] <- "numInvs"
    # sort:
    if(length(invLocs$Location) != 0) {
      invLocs <- invLocs[order(-invLocs$numInvs),]
    }
    invLocs$Location[is.na(invLocs$Location)] <- 0
    invLocs$Location <- as.character(invLocs$Location)
    if (length(invLocs$Location) == 0) {
      locMaxInvLocs <- NA
      numMaxInvLocs <- 0
    } else if (invLocs$Location[invLocs$numInvs == max(invLocs$numInvs)] == locale) {
      locMaxInvLocs <- invLocs[2,1]
      numMaxInvLocs <- invLocs[2,2]
    } else {
      locMaxInvLocs <- invLocs[1,1]
      numMaxInvLocs <- invLocs[1,2]
    }


    ##########################################
    ## SNA graph and data, all collabs (local & nonlocal):
    # colorLocal <- "#1B9E77"
    # colorNonlocal <- "#D95F02"

    # adapted from create-patent-matrix.R:

    library(Matrix) # different than matrix
    A <- spMatrix(nrow=length(unique(invs$lower)), # change these variable names as needed
            ncol=length(unique(invs$Patent)), 
            i = as.numeric(factor(invs$lower)),
            j = as.numeric(factor(invs$Patent)),
            x = rep(1, length(as.numeric(invs$lower))) )
    row.names(A) <- levels(factor(invs$lower))
    colnames(A) <- levels(factor(invs$Patent))

    # two-mode to one-mode (by entity):
    Arow <- A %*% t(A)

    library(igraph)
    irow <- graph.adjacency(Arow,
      mode = "undirected",
      weighted = TRUE)
    irow.edges <- as.data.frame(get.edgelist(irow))
    V(irow)
    E(irow)$weight
    edges <- get.edgelist(irow, names = TRUE)

    # removes self-loops:
    net <- simplify(irow,
      remove.loops = TRUE, # removes self-loops
      remove.multiple = TRUE) # not sure if we want to do this

    # to analyze data:
    numComponents <- no.clusters(net)
    components <- clusters(net)
    # table(components$csize) # sizes of all components
    n <- length(components$csize)

    # # graph:
    # add attributes:
    V(net)$Location <- as.character(invs$Location[match(V(net)$name,
      invs$lower)])
    table(V(net)$Country)
    V(net)$Country <- as.character(invs$Country[match(V(net)$name,
      invs$lower)])
    if (length(V(net)$Country != 0)) {
      countryCount <- as.data.frame(table(V(net)$Country))
      names(countryCount) <- c("Country", "nInventors")
      totalNodes <- sum(countryCount$nInventors) # number of inventors, local & nonlocal
      USInventors <- countryCount$nInventors[countryCount$Country == "US"]
    } else {
      countryCount <- data.frame(
        Country = NA,
        nInventors = 0)
      totalNodes <- 0
      USInventors <- 0
    }


    # # define colors for attributes:
    # V(net)$color <- V(net)$Location
    # V(net)$color <- gsub(locale,
    #   colorLocal,
    #   V(net)$color) 
    # V(net)$color <- ifelse(V(net)$color == colorLocal,
    #   colorLocal,
    #   colorNonlocal)
    # V(net)$color <- ifelse(is.na(V(net)$color) == TRUE,
    #   colorNonlocal,
    #   V(net)$color)

    # cent <- data.frame(bet = betweenness(net),
    #   eig = evcent(net)$vector)
    # categGraph <- sub(" .*", "", category)
    # outGraph <- paste("network graphs/", locale, "-", categGraph, "-", 
    #   firstAppYear, "thru", lastAppYear, ".pdf",
    #     sep = "")
    # pdf(outGraph)
    # plot(net,
    #   layout = layout.fruchterman.reingold, # optional. if no layout, it tries to choose best one
    #   edge.width = E(net)$weight,
    #   vertex.label = NA,
    #   # vertex.label = V(net)$name,
    #   # vertex.label.cex = 0.5,
    #   vertex.size = 5 * (cent$eig + 1))
    #   # layout.spring
    #   # layout.lgl (for large graphs)
    # dev.off()

    descstats <- data.frame(
      locale = locale,
      firstAppYear = firstAppYear,
      lastAppYear = lastAppYear,
      category = category,
      nInventors = nrow(local[!duplicated(local$lower),]),
      nPatents = nrow(local[!duplicated(local$Patent),]),
      lonewolves = lonewolves,
      localCollabs = localCollabs,
      nonlocalCollabs = nonlocalCollabs, # does not include collabs with location NA
      totalCollabs = totalCollabs,
      locMaxInvLocs = locMaxInvLocs,
      numMaxInvLocs = numMaxInvLocs,
      locMaxCollabLocs = locMaxCollabLocs,
      numMaxCollabLocs = numMaxCollabLocs,
      totalNodes = totalNodes, # number of inventors, local & nonlocal
      intlInventors = totalNodes - 
        USInventors - 
        ifelse(length(countryCount$nInventors[countryCount$Country == ""]) != 0,
          countryCount$nInventors[countryCount$Country == ""],
          0),
      locMaxIntl = ifelse(length(countryCount$Country) > 1, 
        countryCount$Country[countryCount$nInventors == max(countryCount$nInventors[countryCount$Country != "US"])],
        NA), # needs to be changed for use with non-US cities
      numMaxIntl = ifelse(length(countryCount$Country) > 1,
        countryCount$nInventors[countryCount$nInventors == max(countryCount$nInventors[countryCount$Country != "US"])],
        NA),
      lgcomp = ifelse(length(components$csize) !=0, 
        max(components$csize),
        0),
      lgcomp2 = ifelse(length(components$csize) > 1,
        sort(components$csize, partial = n - 1)[n - 1],
        0),
      numcomp = numComponents)

    write.table(descstats, "descstats-singleyear.csv", # (change write location)
      append = TRUE, 
      quote = FALSE,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE)


  }
  )

}

# play alert sound:
system("rhythmbox KDE-Sys-App-Positive.ogg")



