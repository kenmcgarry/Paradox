berksonGenerator <- function(input, n, cutoff, default, maxIt = 1000) {
  
  # Error checks: 
  if (missing(default)) {
    message("'default' is missing. Setting default = 'EBICglasso")
    default <- "EBICglasso"
  }
  
  if (is.list(input)){
    
    # Check for graph: 
    if ("weiadj" %in% names(input)){
      graph <- input$weiadj
    }
    else if ("graph" %in% names(input)){
      graph <- input$graph
    } else stop("'graph' not in input list.")
    
    # Check for thresholds
    if ("thresholds" %in% names(input)){
      intercepts <- input$thresholds
    }
    else if ("intercepts" %in% names(input)){
      intercepts <- input$intercepts
    } else stop("'intercepts' not in input list.")
    
  } else {
    stop("'input' is not a list with graph and intercepts.")
  }
  
  # Load functions: 
  generateOrdinalData <- function(sampleSize, input) {
    
    #library("bootnet")
    gen <- ggmGenerator()
    newData <- gen(sampleSize, input)
    newData <- as.data.frame(newData)
    colnames(newData) <- colnames(graph)
    allThresholds <- intercepts   
    allThresholds <- as.data.frame(allThresholds)
    
    newOrdinalData <- newData
    
    for (i in 1:ncol(newData)) {
      
      # Select current variable: 
      variable <- colnames(newData[i])
      
      # Select thresholds of that variable:
      thresholds <- allThresholds[grep(variable, rownames(allThresholds)), ]
      
      # Add infinity:
      thresholds <- c(-Inf, thresholds, Inf)
      
      # Place into the right category:
      for (j in 1:(length(thresholds) - 1)){
        newOrdinalData[newData[ ,i] > thresholds[j] & newData[ ,i] <= thresholds[j + 1], i] <- j - 1
      }
    }
    
    return(newOrdinalData)
  }
  
  # Make new dataset to be filled:
  newDataset <- matrix(NA, n, ncol(graph))
  i <- 1
  
  # Fill dataset: 
  curIt <- 0
  while(i <= n) {
    
    # Sample 1 observation 
    if (default == "Ising" || default == "IsingFit" || default == "binary" || default == "Binary" ) {
      newDatapoint <- IsingSampler::IsingSampler(1, graph = graph, thresholds = intercepts)
    } 
    if (default == "EBICglasso" || default == "GGM" || default == "ordinal" || default == "Ordinal") {
      newDatapoint <- generateOrdinalData(1, input)
      newDatapoint <- as.matrix(newDatapoint)
    }
    
    # If sum score of new observation is high enough, add to dataset:
    sumscore <- rowSums(as.data.frame(newDatapoint))
    if(sumscore >= cutoff) {
      newDataset[i,] <- newDatapoint
      i = i + 1
      curIt <- 0
    }
    
    # Error check to not get stuck in an infinite loop
    # For example when cut off > sum score of the variables 
    curIt <- curIt + 1
    if (curIt > maxIt) {
      stop("Maximum number of iterations reached")
    }
  }
  colnames(newDataset) <- colnames(graph)
  return(as.data.frame(newDataset))
}



############### Berkson ###################################################
N <- 10 # Number of nodes
Weight <- 0.5

Graph <- as.matrix(get.adjacency(watts.strogatz.game(1, N, 1, p = 0)))* Weight
Thresh <- -rowSums(Graph) / 2
input <- list(graph = Graph, intercepts = Thresh)
#plot(input)

newDatasetIsing <- berksonGenerator(input, n = 500, cutoff = 2, default = "Ising")

Sim1 <- netSimulator(input, dataGenerator = function(n, input){
  berksonGenerator(input, n, cutoff = 2, default = "IsingFit")},
  default = "IsingFit", nCores = 8, nReps = 100, nCases = c(500, 1000))

plot(Sim1)

Sim1$cutoff <- 2

Sim2 <- netSimulator(input,dataGenerator = function(n, input){
  berksonGenerator(input, n, cutoff = 4, default = "IsingFit")
}, default = "IsingFit", nCores = 8, nReps = 100, nCases = c(500, 1000))

Sim2$cutoff <- 4
totalSim <- rbind(Sim1, Sim2)
plot(totalSim, color = "cutoff")

