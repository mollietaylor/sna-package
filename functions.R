NarrowData <- function(data, limit.by, limit.to) {
  # Gets [local] patents. Also allows you to specify any other criteria to narrow down the dataset. 
  #
  # Args:
  #   data: (localdata)
  #   limit.by: a vector (the variable the data will be limited by)
  #   limit.to: (the value the data will be limited to) (or range)
  #
  # Returns:
  #   "localdata" data

  # code goes here
  # sanity-check:
  if ( length(limit.to) < 1 ) stop("limit.to has length < 1")
  if ( (is.vector(limit.by) | is.factor(limit.by)) != TRUE ) stop("limit.by is not a vector or factor")

  # if statement for whether value is value or range
  if ( length(limit.to) == 1 ) {
      narrowed <- data[which(limit.by == limit.to),]
  } else
      narrowed <- data[which(limit.by >= min(limit.to) & limit.by <= max(limit.to)),]

  return(narrowed)
}


FunName <- function(full.data, local.data, group) {
  # What function does.
  #
  # Args:
  #   full.data: [sampledata]
  #   local.data: [data for just the local inventors. sometimes will have come from NarrowData]
  #   group: the variable name of the vector where each individual's group is listed
  #
  # Returns:
  #   A data frame [with all patents inventors worked on, including data for nonlocal inventors / local & nonlocal inventor network of local inventors]

  # Sanity check:
  if ( is.data.frame(full.data) != TRUE ) stop("full.data is not a data frame")
  if ( is.data.frame(local.data) != TRUE ) stop("local.data is not a data frame")
  if ( (is.vector(group) | is.factor(group)) != TRUE ) stop("group is not a vector or factor")

  # Function:
  numslocaldata <- data.frame(group = unique(local.data$group))

  invs <- merge(full.data,
    numslocaldata, 
    by.x = group)

  return(invs)
}

FunName(sampledata, localdata, "Patent")

    # numslocaldata <- data.frame(Patent = unique(localdata$Patent))

    # # merge
    # invs <- merge(sampledata,
    #   numslocaldata,
    #   by.x = "Patent")


# should functions use variable name or data$var as args?