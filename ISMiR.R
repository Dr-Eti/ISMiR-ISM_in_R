## ISM - Interpretive Structural Modelling - algortihm from scratch
#
# 16 08 2021 (start date)
# 19 08 2021 (end date)
#
# Author: Ettore Settanni
# Contact: E.Settanni@eng.cam.ac.uk
#
# tested on example: Warfield (1976) Societal Systems p250 - 257 (up to the condensation) + 281 (skeleton)
#

# clear
rm(list = ls())
gc()

#### load packages ####
library(tictoc)                   # running time

#### 00.1 - data dummy example ####

## JORS paper example
# test_m <- matrix(c(
#   0,	0,	1,	0,	0,	0,	5,
#   0,	0,	0,	0,	2,	5,	0,
#   0,	0,	0,	0,	0,	0,	5,
#   3,	0,	0,	0,	0,	0,	0,
#   0,	0,	0,	0,	0,	0,	0,
#   0,	3,	4,	0,	0,	0,	0,
#   5,	0,	0,	0,  0,	0,	0
# ),ncol = 7, byrow = TRUE)


# Warfield Binary Matrices in System modeling (1973) p 442
# test_m <- matrix(c(
#   1, 0, 1, 0, 0, 0, 1,
#   0, 1, 0, 0, 1, 1, 0,
#   0, 0, 1, 0, 0, 0, 1,
#   1, 0, 0, 1, 0, 0, 0,
#   0, 0, 0, 0, 1, 0, 0,
#   0, 1, 1, 0, 0, 1, 0,
#   1, 0, 0, 0, 0, 0, 1
# ),ncol = 7, byrow = TRUE)

## Warfield Binary Matrices in System modeling (1973) p 445 & Warfield (1976) societal systems p277 + 255
test_m <- matrix(c(
  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0,
  1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0,
  1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0,
  1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0,
  1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0,
  1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0,
  1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0,
  0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0,
  0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1
),ncol = 15, byrow = TRUE)

## Warfield Binary Matrices in System modeling (1974) p.409  & Warfield (1976) societal systems p 250
# test_m <- matrix(c(
#   1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#   0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#   0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#   0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#   0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#   0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0,
#   0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
#   0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0,
#   0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0,
#   0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0,
#   1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0,
#   0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0,
#   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
#   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
# ),ncol = 14, byrow = TRUE)


#### 00.2 - automated node labelling ####
n_nodes <- ncol(test_m)
node_names <- formatC(0:(n_nodes-1),width =  1+round(log10(n_nodes),0), flag = "0")          # thread: https://stackoverflow.com/questions/8266915/format-number-as-fixed-width-with-leading-zeros
colnames(test_m) <- node_names
rownames(test_m) <- node_names




#### 01.1 - declare FUNCTION: transClosure                     returning transClosure_m - Warshall's algorithm ####

# It's less efficient than Depth-first search
# online resources
# Deo 1974 or
# --- p.13 http://www.personal.reading.ac.uk/~sis01xh/teaching/CS2EA16/Le8b.pdf
# --- p.3 https://www.cs.princeton.edu/courses/archive/spr03/cs226/lectures/digraph.4up.pdf

transClosure <- function(test_m){
  transClosure_m <- test_m
  n_nodes <- nrow(test_m)
  warsh_idx_inner <- expand.grid(1:n_nodes, 1:n_nodes)
  for(k in 1:n_nodes){
    temp_dummy <- apply(warsh_idx_inner, 1, function(x){
      i_1 <- as.numeric(x[1])
      j_1 <- as.numeric(x[2])
      ifelse(transClosure_m[i_1, j_1] !=0, transClosure_m[i_1, j_1], transClosure_m[i_1, k]*transClosure_m[k, j_1])
    })
    transClosure_m <- matrix(temp_dummy, ncol = n_nodes, byrow = FALSE)
  }
  return(transClosure_m)
}



#### 01.2 - declare FUNCTION: DFS_4_ISM                        returning DFS_output$SCC, DFS_output$BlockTri_m  ####

# Reproduced from a different standalone GitHub directory: https://github.com/Dr-Eti/RtarD-Tarjans_DFS_in_R.git
# This REV    02 11 2021 fixes few issues
# returns 2 outputs
# --- component
# --- block triangular permutation

DFS_4_ISM <- function(test_m){
  
  n_nodes <- ncol(test_m)
  node_names <- formatC(0:(ncol(test_m)-1),width = 2, flag = "0")          # thread: https://stackoverflow.com/questions/8266915/format-number-as-fixed-width-with-leading-zeros
  colnames(test_m) <- node_names
  rownames(test_m) <- node_names
  
  ## step 1: create list of successors for each node
  successors <- lapply(node_names, function(x){
    current_node <- x
    # search the row corresponding to the current node for successors
    all_successors_idx <- expand.grid(current_node, node_names, width = 2, flag = "0")                 # expand on current node to find successors
    all_successors_links <- apply(all_successors_idx, 1, function(y){
      test_m[y[1], y[2]]
    })
    if(length(as.character(all_successors_idx[which(all_successors_links != 0),2])) > 0){
      as.character(all_successors_idx[which(all_successors_links != 0),2])
    } else {
      NA
    }
  })
  names(successors) <- node_names 
  
  
  ## step 2: lookup table (going to use this for depth descent and backtracking)
  util_table <- lapply(1:n_nodes, function(z){
    x <- successors[z]
    a <- match(names(x), node_names)
    b <- match(unlist(x), node_names)
    temp_col <- expand.grid(a,b)
    tab_head <- c("i", 
                  "j", 
                  "level", 
                  "v_node", 
                  #"v_label", 
                  "v_number", 
                  "v_lowlink", 
                  "successors", 
                  "k_successor_idx", 
                  "w_k_node", 
                  #"w_k_label", 
                  "w_k_number", 
                  "w_k_lowlink")
    temp_mat <- matrix(NA, ncol = length(tab_head), nrow = nrow(temp_col))
    colnames(temp_mat) <- tab_head
    rownames(temp_mat) <- rep(names(x),nrow(temp_mat))
    temp_mat[,"v_node"] <- temp_col[,1]
    temp_mat[, "w_k_node"] <- temp_col[,2]
    return(temp_mat)
  })
  util_table_df <- do.call(rbind, util_table)
  
  
  ## step 3: initialise stuff
  # counters
  n <- n_nodes
  i <- 0
  c <- 0                                                                                                                      # component counter
  # initialise condition flags
  nodes_not_numbered_yet <- n
  test0 <- TRUE
  # initialise lists
  Stack_S <- list()
  Component_list <- list()
  # initialise table of node numbers
  node_numbering <- cbind(rep(NA,length(node_names)),rep(NA,length(node_names)),rep(NA,length(node_names)))
  colnames(node_numbering) <- c("node_number","node_lowlink","node_onStack")
  rownames(node_numbering) <- node_names
  
  ## Step 4: Main loop - the magic happens here (if it works)
  # Loop #0: restart at each strongly connected component
  dummy4 <- TRUE
  while(dummy4){
    level <- 1                                                                                                                # I AM NOT ENTIRELY SURE ABOUT THIS SYSTEM I CAME UP WITH. it's like a thread that pulls you back up once the depth-first search reached an end.
    j <- 0
    # pick the first node not numbered yet
    nodes_to_explore <- which(is.na(node_numbering[, "node_number"]))
    if(nodes_not_numbered_yet != 0){                                                                                          # there are nodes not numbered yet
      v <- as.numeric(nodes_to_explore[1])                                                                                    # just picking the first unnumbered node. In the first iteration this will be node 1
      v_label <- node_names[v]
    }
    
    # newly introduced
    flag_extra_round <- TRUE                                                                                                  # for use later during backtracking to add an extra round and avoid some issues with updating values at the very end
    extra_round_counter <- 0
    
    # Loop #1: explores successors "depth first", jumping between nodes
    dummy0 <- TRUE
    while(dummy0){
      i <- i + 1                                                                                                              # labels nodes as we visit them
      j <- j + 1                                                                                                              # position in stack of nodes
      # update node numbering
      Stack_S[j] <- v
      node_numbering[v,"node_number"] <- i
      node_numbering[v,"node_lowlink"] <- i
      node_numbering[v,"node_onStack"] <- 1
      # successors
      w_labels <- unlist(successors[v])
      sink_test <- which(is.na(w_labels))                                                                                     # check if successor is a sink node
      # update predecessor (unless first node)
      # if (i > 1){                                                                                                           # this old line was replaced: unless the first node is also a sink node, it may well be it is some other node's predecessor
      back_idx <- which(util_table_df[,"w_k_node"] == v)                                                                      # ... I guess we could use this information to single out sink nodes already
      if (length(back_idx) > 0){ 
        util_table_df[back_idx, "w_k_number"] <- node_numbering[v,"node_number"]
        util_table_df[back_idx, "w_k_lowlink"] <- node_numbering[v,"node_lowlink"]
      }
      if(length(sink_test) == 0){
        w <- match(w_labels, node_names)
        n_successors <- length(w)
      } else {
        w <- NA
        n_successors <- 0
      }
      # initialise a progressive counter for incident nodes reachable from the current node
      k <- 0
      
      # Loop #2: explores incident nodes "sequentially" for a given node
      dummy1 <- TRUE
      while(dummy1){
        if(length(sink_test) == 0){                                                                                           # the current node has a successor
          k <- k + 1
          w_k <- w[k]                                                                                                         # next incident node (neighbour)
          
          # update main tableau (df version)
          util_table_df_subset <- which(rownames(util_table_df) == v_label)                                                   # filter for the current node v
          if(length(util_table_df_subset) > 1){
            if(k == 1){
              util_table_df[util_table_df_subset,][k,"i"] <- i
              util_table_df[util_table_df_subset,][k,"j"] <- j
              util_table_df[util_table_df_subset,][k,"level"] <- level
              util_table_df[util_table_df_subset,][k,"v_node"] <- v
              util_table_df[util_table_df_subset,][k,"v_number"] <- node_numbering[v,"node_number"]
              util_table_df[util_table_df_subset,][k,"v_lowlink"] <- node_numbering[v,"node_lowlink"]
              util_table_df[util_table_df_subset,][k,"successors"] <- n_successors
            }
            util_table_df[util_table_df_subset,][k,"k_successor_idx"] <- k
            util_table_df[util_table_df_subset,][k,"w_k_node"] <- w_k                                                         # could be removed... 
            util_table_df[util_table_df_subset,][k,"w_k_number"] <- node_numbering[w_k,"node_number"]
            util_table_df[util_table_df_subset,][k,"w_k_lowlink"] <- node_numbering[w_k, "node_lowlink"]
          } else {
            if(k == 1){
              util_table_df[util_table_df_subset,"i"] <- i
              util_table_df[util_table_df_subset,"j"] <- j
              util_table_df[util_table_df_subset,"level"] <- level
              util_table_df[util_table_df_subset,"v_node"] <- v
              util_table_df[util_table_df_subset,"v_number"] <- node_numbering[v,"node_number"]
              util_table_df[util_table_df_subset,"v_lowlink"] <- node_numbering[v,"node_lowlink"]
              util_table_df[util_table_df_subset,"successors"] <- n_successors
            }
            util_table_df[util_table_df_subset,"k_successor_idx"] <- k
            util_table_df[util_table_df_subset,"w_k_node"] <- w_k                                                             # could be removed... 
            util_table_df[util_table_df_subset,"w_k_number"] <- node_numbering[w_k,"node_number"]
            util_table_df[util_table_df_subset,"w_k_lowlink"] <- node_numbering[w_k, "node_lowlink"]
          }
          # tests
          test0 <- is.na(node_numbering[w_k,"node_number"])                                 # we can jump onto this node (depth first)
          test1 <- node_numbering[w_k,"node_number"] < node_numbering[v,"node_number"]      # the next node has been visited already?
          test2 <- node_numbering[w_k,"node_onStack"] == 1                                  # the next node is in the stack already OR has been on the stack
        } else {                                                     # there is no successor to the current node (sink)
          # tests
          test0 <- FALSE                                             # forces to backtrack if there is no successor
          test1 <- NA
          test2 <- NA
          # update main tableau (df version)
          util_table_df_subset <- which(rownames(util_table_df) == v_label)                              # filter for the current node v
          util_table_df[util_table_df_subset,"i"] <- i
          util_table_df[util_table_df_subset,"j"] <- j
          util_table_df[util_table_df_subset,"level"] <- level
          util_table_df[util_table_df_subset,"v_node"] <- v
          util_table_df[util_table_df_subset,"v_number"] <- node_numbering[v,"node_number"]
          util_table_df[util_table_df_subset,"v_lowlink"] <- node_numbering[v,"node_lowlink"]
          util_table_df[util_table_df_subset,"successors"] <- n_successors
          util_table_df[util_table_df_subset,"k_successor_idx"] <- k
          util_table_df[util_table_df_subset,"w_k_node"] <- (-1)
          util_table_df[util_table_df_subset,"w_k_number"] <- (-1)
          util_table_df[util_table_df_subset,"w_k_lowlink"] <- (-1)
        }
        
        # THIS IS WHERE WE DECIDE WHETHER TO CONTINUE DEPTH-FIRST, LOOK INTO A NEIGHBOUR, OR BACKTRACK
        if (test0){
          # continue depth-first search
          dummy1 <- FALSE                                                                                                     # break Loop 2 (exit sequential exploration of successors for a given node)
          v <- w_k                                                                                                            # move on to successor
          v_label <- node_names[v]
          
          # avoid having more than one line with the same level
          level_max <- max(util_table_df[,"level"], na.rm = T)                    
          if (level < level_max){                                                                                             # probably we backtracked and re-winded the level counter.This should get us back where we were before the rewind 
            level <- level_max + 1
          } else {
            level <- level + 1
          }
          
        } else {
          # start "back tracking"
          # Loop #3: are there neighbors left to explore?
          dummy5 <- TRUE
          while(dummy5){
            if(k != 0 & k < n_successors) {                                                                                  # there are other adjacent nodes left to explore
              dummy3 <- FALSE                                                                                                # skip the next loop, break out and go increase k 
              dummy5 <- FALSE
            } else {
              dummy3 <- TRUE   
            }  
            # the test below is needed anyway unless we are in a sink node
            if(length(sink_test) == 0){                                                                                       # we are not at a sink node
              # if successor is a sink then don't update or they will end up in the same component
              test3 <- FALSE
              successor_successors <- which(util_table_df[,"v_node"] == util_table_df[which(rownames(util_table_df) == v_label),"w_k_node"][k])
              if(util_table_df[successor_successors,"successors"][1]>0){test3 <- TRUE}
              if (test1 & test2 & test3){                                                                                     # the current k successor has been visited PRIOR to v (has lower number), and it is on the stack already
                # update lowlink values - FIRST TYPE (going downwards)  
                node_numbering[v,"node_lowlink"] <- min(node_numbering[v,"node_lowlink"],node_numbering[w_k,"node_number"])
                util_table_df_subset <- which(rownames(util_table_df) == v_label)                                             # filter for the current node v
                if(length(util_table_df_subset) > 1){
                  util_table_df[util_table_df_subset,][1,"v_lowlink"] <- node_numbering[v,"node_lowlink"]                     # always update the first line, corresponding to v
                } else {
                  util_table_df[util_table_df_subset,"v_lowlink"] <- node_numbering[v,"node_lowlink"]
                }
                # update lowlink elsewhere
                #if (i > 1){
                other_idx <- which(util_table_df[,"w_k_node"] == v)
                if(length(other_idx)>0){
                  util_table_df[ other_idx, "w_k_lowlink"] <- node_numbering[v,"node_lowlink"]
                }
              }
            }
            
            # Loop #4: revisit all the neighbours already visited
            while (dummy3) {
              if(n_successors !=0 & k !=0){                                                                                   # we are not at a sink node / we have not re-visited all the neighbors
                # test if successor is a sink then don't update or they will end up in the same component
                test3 <- FALSE
                successor_successors <- which(util_table_df[,"v_node"] == util_table_df[which(rownames(util_table_df) == v_label),"w_k_node"][k])
                if(util_table_df[successor_successors,"successors"][1]>0){test3 <- TRUE}
                if (test2 & test3){                                                                                           # the next node is already in the stack AND IT IS NOT A SINK
                  # update lowlink values - SECOND TYPE (ascending)                                                           # the next node is already in the stack
                  node_numbering[v,"node_lowlink"] <- min(node_numbering[v,"node_lowlink"],node_numbering[w_k,"node_lowlink"])
                  util_table_df_subset <- which(rownames(util_table_df) == v_label)                                           # filter for the current node v
                  if(length(util_table_df_subset) > 1){
                    util_table_df[util_table_df_subset,][1,"v_lowlink"] <- node_numbering[v,"node_lowlink"]                   # always update the first line, corresponding to v
                  } else {
                    util_table_df[util_table_df_subset,"v_lowlink"] <- node_numbering[v,"node_lowlink"]
                  }
                  # update lowlink elsewhere
                  #if (i > 1){
                  other_idx <- which(util_table_df[,"w_k_node"] == v)
                  if(length(other_idx)>0){
                    util_table_df[ other_idx, "w_k_lowlink"] <- node_numbering[v,"node_lowlink"]
                  }
                }
                k <- k - 1                                                                                                    # go back to the previous neighbour already visted
                if (k !=0){
                  w_k <- w[k] 
                  #re-do the test
                  test0 <- is.na(node_numbering[w_k,"node_number"])                                                           # we can jump onto this node (depth first)
                  test1 <- node_numbering[w_k,"node_number"] < node_numbering[v,"node_number"]                                # the next node has been visited already?
                  test2 <- node_numbering[w_k,"node_onStack"] == 1                                                            # the next node is in the stack already
                } else {
                  test2 <- FALSE
                  w_k <- (-1)
                }
              } else {                                                                                                        # either we are at a sink node or we have re-visited all the neighbors
                dummy3 <- FALSE                                                                                               # break loop 4 after moving one level up
                level <- level - 1                                                                                            # go one back to the previous node in the depth-first sequence
                
                # The below wraps the second ("upwards") lowlink update into an iteration - without this, a few lowlink updates may go unnoticed
                if (level >= 0){
                  
                  # The below checks that, when you've reached level 0, all nodes' lowlink values matches the lowest  lowlink of their successors.
                  # if that's not the case it may be that we need more rounds of update
                  if (level ==0 & flag_extra_round){
                    extra_round_counter <- extra_round_counter + 1
                    
                    # the below checks that we DON'T HAVE NODES WITH THE LATEST SUCCESSOR'S LOWLINK VALUE BEING lower THAN THE NODE'S LOWLINK
                    # if a node's LOWLINK is larger than the lowest among its successors' LOWLINK the we need to keep updating
                    update_needed <- lapply(Stack_S, function(x){
                      v_lowlink <- node_numbering[x,"node_lowlink"]
                      v_neigb_lowest_lowlink <- min(util_table_df[which(util_table_df[,"v_node"] == x) , "w_k_lowlink"])
                      if(v_neigb_lowest_lowlink > 0){                                                       # sink nodes by convention get "-1" in this field which might cause looping forever
                        if(v_lowlink != v_neigb_lowest_lowlink){
                          1 
                        } else {0}
                      } else {0}
                    })
                    updates_test <- sum(unlist(update_needed))
                    if(updates_test > 0 & extra_round_counter < 4){
                      level <- max(util_table_df[which(!is.na(util_table_df[,"level"])),"level"])           # start all over again, hoping we get all the lowlinks updated
                    } else {
                      flag_extra_round <- FALSE                                                             # break out of this
                    }
                  }
                  
                  # GET BACK WHERE YOU LEFT THINGS AT THE PREVIOUS LEVEL
                  # (the following fixes the case in which there is no successor going up one level)
                  test_any_successor <- length(which(util_table_df[,"level"] == level & util_table_df[, "successors"] !=0))     # test if at this level there may be no successor
                  while(test_any_successor == 0 & level > 1){
                    level <- level - 1                                                                                          # if there is no successor, keep going backwards
                    test_any_successor <- length(which(util_table_df[,"level"] == level & util_table_df[, "successors"] !=0))   
                  }
                  if (test_any_successor != 0 & level > 0){
                    back_idx <- which(util_table_df[,"level"] == level & util_table_df[, "successors"] !=0)                   # there may be more than one node with the same level if E.G. ONE NEIGHBOUR IS A SINK but the other isn't. We ignore the sink
                    v <- util_table_df[back_idx, "v_node"]                                                  
                    v_label <-  node_names[v]
                    n_successors <- util_table_df[back_idx, "successors"]
                    k_back_idx_a <- as.numeric(which(util_table_df[, "v_node"] == v))
                    util_table_df_subset <- util_table_df[k_back_idx_a,]
                    if(length(k_back_idx_a) > 1){
                      k_back_idx_b <- max(which(!is.na(util_table_df_subset[, "k_successor_idx"])))                           # retrieve last successor index explored for the current node
                      k <- util_table_df_subset[k_back_idx_b , "k_successor_idx"]
                      w_k <- util_table_df_subset[k_back_idx_b , "w_k_node"]
                    } else {
                      k <- as.numeric(util_table_df_subset["k_successor_idx"])
                      w_k <- as.numeric(util_table_df_subset["w_k_node"])
                    }
                    w <- as.numeric(util_table_df[k_back_idx_a, "w_k_node"])
                    w_labels <- node_names[w]
                    sink_test <- which(is.na(w_labels))                                                                       # check if the successor is a sink node
                    # re-do test
                    test0 <- is.na(node_numbering[w_k,"node_number"])                                                         # we can jump onto this node (depth first)
                    test1 <- node_numbering[w_k,"node_number"] < node_numbering[v,"node_number"]                              # the next node has been visited already?
                    test2 <- node_numbering[w_k,"node_onStack"] == 1                                                          # the next node is in the stack already
                  }
                }
              }
            } # end of Loop 4 (dummy 3)
            if(level == 0) {
              dummy5 <- FALSE                                                                                                 # break loop 3
              dummy1 <- FALSE                                                                                                 # break loop 2 (ends up in the same place as if there were no successors)
            }
          }                                                                                                                   # end of Loop 3 (dummy 5)
          #  end of "back tracking" sub
        } 
      } # end of Loop 2 (dummy1)
      
      # THIS IS WHERE WE POP THE STACK AND POPULATE THE COMPONENT
      # update count of nodes not numbered yet
      nodes_not_numbered_yet <- length(which(is.na(node_numbering[,"node_number"])))
      nodes_visited_so_far <- as.numeric(which(!is.na(node_numbering[,"node_number"]) & node_numbering[,"node_onStack"] == 1))
      if(!test0){
        Stack_S_BACKUP <- Stack_S
        Component_list_BACKUP <- Component_list
        # Start of "Pop nodes" sub, but only if we're done with depth-first search
        for(r in nodes_visited_so_far){
          x <- as.numeric(node_numbering[r,])
          if (x[1] == x[2]){                                                                                                  # the node is a root node
            c <- c + 1                                                                                                        # component number
            # put nodes in component
            temp_component_list <- lapply(1:length(Stack_S), function(s){
              y <- Stack_S[[s]]
              pop_test1 <- node_numbering[y, "node_number"] >= x[1]
              pop_test2 <- node_numbering[y, "node_lowlink"] == x[2]
              if(pop_test1 & pop_test2){
                y
              }
            })
            to_delete <- unlist(temp_component_list)
            Component_list[[c]] <- to_delete
            # pop elements out of stack
            #idx_delete <- which(Stack_S == to_delete)
            temp_idx_delete <- rep(seq_along(Stack_S), sapply(Stack_S, length))                                               # thread: https://stackoverflow.com/questions/11002391/fast-way-of-getting-index-of-match-in-list
            idx_delete <- temp_idx_delete[match(to_delete, unlist(Stack_S))]            
            Stack_S[idx_delete] <- NULL
            # update table
            node_numbering[to_delete,"node_onStack"] <- (-1)                                                                  # mark elements that have been on stack, but no longer are
          }
        }
        # need to re-label the LEVELS used so that they do not interefere with the next component
        util_table_df_BACKUP <- util_table_df
        idx_change_level <- which(!is.na(util_table_df[,"level"]) & util_table_df[,"level"] > 0)
        util_table_df[idx_change_level,"level"] <- (-1)*util_table_df[idx_change_level,"level"]
        # restart for next component or terminate
        if(nodes_not_numbered_yet != 0){
          dummy0 <- FALSE                                                                                                     # break Loop 1 (depths first exploration of successors, jumping between nodes) and move up one level
        }  else {
          dummy4 <- FALSE                                                                                                     # break Loop 5: YOU'RE DONE
          dummy0 <- FALSE                                                                                                     # break Loop 1
        }
      } ## end of "Pop nodes" sub
    } # end of Loop 1 (dummy0)
  } # end of Loop 0 (dummy4)
  
  
  ## Step 5: Block triangular form
  # Plain english description from: Hume D., Litsey J., and Plemmons (1981) p 272 - see this google book https://books.google.co.uk/books?id=pEMsAQAAIAAJ&pg=PA272&lpg=PA272&dq=duff+and+reid+block+triangularization&source=bl&ots=zlTT95Usx-&sig=ACfU3U3GkBU0av0mI459KObNAqyrc-1sTw&hl=en&sa=X&ved=2ahUKEwiWkKnBlKbyAhUSesAKHXmZArcQ6AF6BAgREAM#v=onepage&q=duff%20and%20reid%20block%20triangularization&f=false
  # - all vertices within a strongly connected component are numbered sequentially
  # - if there exists and edge from a vertex in component i to a vertex in component j, then all vertices in component i are labelled before all those in component j
  n_components <- length(Component_list)
  x_components_idx <- expand.grid(1:n_components, 1:n_components)       # equivalent to nested for
  x_components_idx <- x_components_idx[, ncol(x_components_idx):1]      # flip
  x_comp_link <- apply(x_components_idx, 1, function(x){
    if(x[1] != x[2]){
      comp_nodes_a <- Component_list[[as.numeric(x[1])]]
      comp_nodes_b <- Component_list[[as.numeric(x[2])]]
      sum(test_m[comp_nodes_a, comp_nodes_b])
    } else {
      NA
    }
  })
  preced_comp <- x_components_idx[which(x_comp_link > 0),]
  colnames(preced_comp) <- c("precedent", "consequent")
  ord_comp <- c(1:c)                                                   # arrange components number in an array
  ord_comp_UPDATE <- ord_comp
  swap_track <- list()
  for(i in 1:nrow(preced_comp)){
    if(i ==1){
      dummy_ord <- ord_comp
    } else {
      dummy_ord <- swap_track[[i - 1]]
    }
    a <- as.numeric(preced_comp[i,1])
    b <- as.numeric(preced_comp[i,2])
    a_pos <- as.numeric(match(a, dummy_ord))
    b_pos <- as.numeric(match(b, dummy_ord))
    if (a_pos > b_pos){                                                # a should precede b but a is currently placed after b
      dummy_ord[b_pos] <- a
      dummy_ord[a_pos] <- b
    }
    swap_track[[i]] <- dummy_ord
    ord_comp_UPDATE <- dummy_ord
  }
  # obtain permutation
  permuted_lines <- unlist(Component_list[ord_comp_UPDATE])                    # re-order components in the list
  permuted_m <- test_m[permuted_lines, permuted_lines]
  
  ## Step 7: Output list
  list(SCC = Component_list,
       BlockTri_m = permuted_m 
  )
}


#### 01.3 - declare FUNCTION: m_pow                            returning Powered_Mat and Binary - Matrix powers ####
# ref: matrx.power function in package matrix.calc - thread: https://rdrr.io/cran/matrixcalc/src/R/matrix.power.R
m_pow <- function(A, p){
  # few checks
  n_nodes <- nrow(A)
  if (!all.equal(nrow(A),ncol(A)))    stop( "not a square matrix" )
  if (!is.numeric(A)) stop( "not a numeric matrix" )
  if ((p != trunc(p)) | (p < (-1))) stop( "issue with exponent" )
  # special cases
  if (p == 0) return(diag( 1, n_nodes))
  if (p == 1) {
    x.p_Bool <- A
    x.p_Bool[which(x.p_Bool > 0)] <- 1
    out2 <- list(Powered_Mat = A, Binary = x.p_Bool)
    return(out2)
  }
  if (p == (-1)) return(solve(A))
  if (p >= 2){
    # iteration
    p_star <- NA
    p_star_flag <- NA
    x.p <- A
    x.p_previous <- A
    for(i in 2:p) {
      x.p <- x.p %*% A
      x.p_Bool <- x.p
      x.p_Bool_count <- x.p
      x.p_Bool[which(x.p > 0)] <- 1
      check <- (x.p_Bool - x.p_previous)                                   # for test only
      if(isTRUE(all.equal(x.p_Bool, x.p_previous)) & (is.na(p_star_flag))){
        p_star <- i
        p_star_flag <- 1
      }
      x.p_previous <- x.p_Bool
      
      ## display for testing only
      #i
      #x.p_Bool
      #check
    }
    out <- list(Powered_Mat = x.p, Binary = x.p_Bool, Binary_count = x.p_Bool_count, p_star = p_star)
    return(out)
  }
}



#### 01.5 - declare FUNCTION: Sets to intersect                returning succendent_set/antecededents_set ####
# part of this code (successors) is embedded in DFS but it's helpful to have it as a separate function too
succedent_set <- function(node_names, reach_m){
  M <- reach_m
  successors <- lapply(node_names, function(x){
    current_node <- x
    # search the row corresponding to the current node for successors
    all_successors_idx <- expand.grid(current_node, node_names)                 # expand on current node to find successors
    all_successors_links <- apply(all_successors_idx, 1, function(y){
      M[y[1], y[2]]
    })
    if(length(as.character(all_successors_idx[which(all_successors_links != 0),2])) > 0){
      as.character(all_successors_idx[which(all_successors_links != 0),2])
    } else {
      NA
    }
  })
  names(successors) <- node_names 
  return(successors)
}
antecedent_set <-  function(node_names, reach_m){
  M <- t(reach_m)                                                                                       # by transposing the matrix we are now looking column-wise
  predecessors <- lapply(node_names, function(x){
    current_node <- x
    # search the column corresponding to the current node for antecedents
    all_antecedents_idx <- expand.grid(current_node, node_names)                                        # expand on current node to find predecessors
    all_antecedents_links <- apply(all_antecedents_idx, 1, function(y){
      M[y[1], y[2]]                                                                             
    })
    if(length(as.character(all_antecedents_idx[which(all_antecedents_links != 0),2])) > 0){
      as.character(all_antecedents_idx[which(all_antecedents_links != 0),2])
    } else {
      NA
    }
  })
  names(predecessors) <- node_names 
  return(predecessors)
}

#### 01.6 - declare FUNCTION: block_diag_EXPAND (block diagonal expansion) ####

block_diag_EXPAND <- function(m, n_levels = n_levels){
  lapply(1:(n_levels), function(y){  
    expansion_term <- y
    expansion_term_MATRIX <- matrix(0L, nrow = nrow(m), ncol = ncol(m))      # prepare an empty matrix to fill with elements along the specific block diagonal that corresponds to this iteration
    rownames(expansion_term_MATRIX) <- rownames(m)
    colnames(expansion_term_MATRIX) <- colnames(m)
    n_levels_next <- n_levels - expansion_term + 1                          # to avoid out of bound
    for(i in 1:n_levels_next){
      x <- relabel_levels[[i]]
      idx_COL <- match(x, comp_names_CONDENSATION)
      if(expansion_term == 1){                                              # this is the main diagonal
        idx_ROW <- idx_COL
      } else {
        x2 <- relabel_levels[[i + expansion_term - 1]]                      # displace the rows to pick to one or more levels below (it's a lower block diagonal)
        idx_ROW <-  match(x2, comp_names_CONDENSATION)
      }
      expansion_term_MATRIX[idx_ROW, idx_COL] <- m[idx_ROW, idx_COL]                     # copy from condensation to the expansion term, block by block as these are picked along the block diagonal
    }
    expansion_term_MATRIX
  })
}

#### 02.1 - ISM - step 1: Reachability matrix                  returning reach_m ####

# Transform to binary - if it isn't already
if(max(test_m) > 1) {
  G <- test_m
  G[which(test_m >= 1)] <- 1
} else {
  G <- test_m
}
if(sum(diag(G)) < n_nodes){                    # does the diagonal of the adjacency matrix have ones on it, already?
  I <- diag(nrow(test_m))
  I_G <- I + G
  I_G[which(I_G >= 1)] <- 1  
} else {
  I_G <- G
}

# method 1: transitive closure, Warshall
tic("running Warshall")
reach_m_1 <-  transClosure(I_G)
rownames(reach_m_1) <- node_names
colnames(reach_m_1) <- node_names
toc()

# method 3: Powers
tic("running Powers")
pow_out <- m_pow(I_G, (n_nodes - 1))
reach_m_3 <- as.matrix(pow_out$Binary)
names(reach_m_2) <- NULL
reach_converg <- pow_out$p_star
toc()

# compare
if(isTRUE(all.equal(reach_m_1, reach_m_3))){
  reach_m <- reach_m_1
} else {
  stop("different reachability matrices- plese check")
}
dontcompare <- FALSE
if(isTRUE(all.equal(reach_m_1, test_m))){
  message("the input matrix was already a reachability matrix")                          # ...as in most papers from Warfield...
  dontcompare <- TRUE
} 


# method 3b: Boolean Powers with %&%  method
# tic("running Powers - Boolean method matrix multiplication")
# pow_out_BOOL <- mpow_BOO(I_G, (n_nodes - 1))
# toc()
#### 02.2 - ISM - step 2: Find Levels                          returning level_ism ####
# Follow Warfield societal systems (1976) p. 276ff - which is same as Warfield 1973 binary matrices in system modeling
# also Warfield (1976 p. 251ff)

level_ism <- list()
flag_level <- TRUE
max_iter <- 1000                          # just for safety
iter <- 0
node_names_dummy <- node_names
set_S <- succedent_set(node_names_dummy, reach_m)
set_A <- antecedent_set(node_names_dummy, reach_m)
while(flag_level & (iter < max_iter)){
  inters_set <- lapply(node_names_dummy, function(i){
    common_elements <- intersect(set_S[[i]], set_A[[i]])
    test_inters <- isTRUE(all.equal(common_elements, set_S[[i]], check.attributes = FALSE))                                   # is the intersection set equal to the succedent set? (Warfield 1976, p251 and 276)
    if (test_inters){
      common_elements
    }
  })
  iter <- iter + 1
  level_nodes <- unique(unlist(inters_set))
  level_ism[[iter]] <- level_nodes
  # update antecedent set
  new_set_A <- lapply(node_names_dummy, function(x){
    temp_set_A <- set_A[[x]]
    matched_nodes <- match(level_nodes,temp_set_A)
    remove_idx <- matched_nodes[which(!is.na(matched_nodes))]
    if(length(remove_idx) != 0){
      if(length(temp_set_A[-remove_idx]) != 0){
        temp_set_A <- temp_set_A[-remove_idx]
      } else {
        NA
      }
    } else {
      temp_set_A
    }
  })
  names(new_set_A) <- node_names_dummy
  new_set_A[sapply(new_set_A, is.null)] <- NULL 
  set_A <- new_set_A 
  # update succedent set
  new_set_S <- lapply(node_names_dummy, function(x){
    temp_set_S <- set_S[[x]]
    matched_nodes <- match(level_nodes,temp_set_S)
    remove_idx <- matched_nodes[which(!is.na(matched_nodes))]
    if(length(remove_idx) != 0){
      if(length(temp_set_S[-remove_idx]) != 0){
        temp_set_S <- temp_set_S[-remove_idx]
      } else {
        NA
      }
    } else {
      temp_set_S
    }
  })
  names(new_set_S) <- node_names_dummy
  new_set_S[sapply(new_set_S, is.null)] <- NULL           # thread: https://stackoverflow.com/questions/33004238/r-removing-null-elements-from-a-list
  set_S <- new_set_S
  # remove reference to the nodes included in this level
  node_names_dummy <- node_names_dummy[-match(level_nodes, node_names_dummy)]
  # check if we've eliminated all nodes
  if(length(node_names_dummy) == 0){
    flag_level <- FALSE
  }
}
n_levels <- length(level_ism)                            # number of levels identified


#### 02.3 - ISM - step 3: Block-triangular permuation          returning blocktri_reach_m   ####
## This function is built-into DFS but I will use it here
# ref: Warfiled (1976 page 252) or equivalently (1974) p.409
# Warfield does not specify a criteria for computers - but I think it works like for components. hence, as before
# - all vertices within a strongly connected component are numbered sequentially
# - if there exists and edge from a vertex in component i to a vertex in component j, then all vertices in component i are labelled before all those in component j
x_levels_idx <- expand.grid(1:n_levels, 1:n_levels)                   # equivalent to nested for
x_levels_idx <- x_levels_idx[, ncol(x_levels_idx):1]      # flip
x_levels_link <- apply(x_levels_idx, 1, function(x){
  if(x[1] != x[2]){
    levels_nodes_a <- level_ism[[as.numeric(x[1])]]
    levels_nodes_b <- level_ism[[as.numeric(x[2])]]
    sum(reach_m[levels_nodes_a, levels_nodes_b])
  } else {
    NA
  }
})
preced_levels <- x_levels_idx[which(x_levels_link > 0),]
colnames(preced_levels) <- c("precedent", "consequent")
ord_levels <- c(1:n_levels)                                                   # arrange levels number in an array
ord_levels_UPDATE <- ord_levels
swap_track <- list()
for(i in 1:nrow(preced_levels)){
  if(i ==1){
    dummy_ord <- ord_levels
  } else {
    dummy_ord <- swap_track[[i - 1]]
  }
  a <- as.numeric(preced_levels[i,1])
  b <- as.numeric(preced_levels[i,2])
  a_pos <- as.numeric(match(a, dummy_ord))
  b_pos <- as.numeric(match(b, dummy_ord))
  if (a_pos > b_pos){                                                # a should precede b but a is currently placed after b
    dummy_ord[b_pos] <- a
    dummy_ord[a_pos] <- b
  }
  swap_track[[i]] <- dummy_ord
  ord_levels_UPDATE <- dummy_ord
}
# obtain permutation
permuted_lines <- unlist(level_ism[ord_levels_UPDATE])                    # re-order levels in the list
blocktri_reach_m_UPPER <- reach_m[permuted_lines, permuted_lines]         # upper block-triangular
blocktri_reach_m_LOWER <- blocktri_reach_m_UPPER[n_nodes:1,n_nodes:1]     # lower block-triangular







#### 02.4 - ISM - step 4: Condensation matrix                  returning condensation_m ####
# Warfield introduces the concept of condensation Matrix (1976: p.256 and 1974:408)
# --- check 1973 paper Figure 8; 1976 book apge 254, 255 & p.281
# my understanding is that Warfield's levels contain one or more strongly connected components = cycles
# this is good news because when you do DFS each component gives you what Warfield calls a cycle - and its transitive closure is filled with ones
# just one CAVEAT: remembe that Warfield doesn't give the adjacency matrix so I cannot really demonstarte with his numerical examples..
#
# Conclusion in plain English: 
# --- execute DFS and find the components - but beware when using Warfield's numerical examples, he gives you the reachability matrix directly
# --- merge all nodes within a component in a single node

DFS_output <- DFS_4_ISM(test_m)

## sub-step 1: map Components onto levels to find cycles       returning relabel_levels
# to take advantage of this, now I want to extend from nodes to components the membership to a given level
# retrieve output from Tarjan's Depth-first function
components_list <- DFS_output$SCC
n_components <- length(components_list)-1
component_names <- paste0("COMP_", formatC(0:n_components, width = 1+round(log10(n_components),0), flag = "0"), sep = "", collapse = NULL)         # thread: https://stackoverflow.com/questions/8266915/format-number-as-fixed-width-with-leading-zeros
names(components_list) <- component_names
# table of components and nodes in a component. the component names are REPEATED if they contain more than one node
comp_table <- unlist(components_list, use.names = FALSE)
setNames(comp_table, rep(names(components_list), lengths(components_list)))                           # KEEP THE list labels without indentation - thread https://stackoverflow.com/questions/35163648/r-unlist-changes-names
comp_table_names <- rep(names(components_list), lengths(components_list))
# replace the individual node names in the level with the name of the component they belong to. Unless 1 node = 1 component
relabel_levels <- lapply(1:n_levels, function(i){                            
  nodes_to_find <- level_ism[[i]]
  nodes_to_find_id <- match(nodes_to_find, node_names)
  correspond_comp <- match(nodes_to_find_id, comp_table)                         
  correspond_comp_unique <- unique(comp_table_names[correspond_comp])
  correspond_comp_unique
})                                                         
## sub-step 2: COLLAPSE rows/columns that correspond to nodes that are in the same component (cycle)
# find rows/cols in the block triang reachab matrix that belong to the same strongly connected component (they form a cycle by definition and need to be collapsed into 1 row/col)
condensation_m <- blocktri_reach_m_LOWER                                                           # pick the reachab matrix in after the block trianguloar permutation
temp_condensation_names <- rownames(condensation_m)                              
for (x in component_names ){
  remove_from_m_idx <- comp_table[which(comp_table_names == x)]                                    # node_id to remove from 
  if(length(remove_from_m_idx) > 1){
    remove_from_m_label <- node_names[remove_from_m_idx ]
    delete_ths_rowCol_m <- match(remove_from_m_label, temp_condensation_names)                     # find current position of component's elements in the permuted (block triang) reachab matrix
    delete_ths_rowCol_m <- delete_ths_rowCol_m[-1]                                                 # leave just the row/col corresponding to the first node (or any node - because it's a reachability natrix) in the component 
    condensation_m  <- condensation_m[-delete_ths_rowCol_m, ]
    condensation_m <- condensation_m[, -delete_ths_rowCol_m]
    #update the name of vectors
    temp_condensation_names <- rownames(condensation_m)
  }
}


#### 02.5 - ISM - step 5: skeleton matrix                      returning skeleton_m ####
# This is the actual BRAIN of ISM ... which has been lost: Warfield develops an algorithm to revert back the condensed reachability matrix into and adjacency matrix
# --- (see Fig 2 in the paper or Fig 9.1 in the book)
# Few problems with it - the notation in the 1976 book p 258 and 1974 paper p.411 DIFFER slightly when it comes to the block diagonal expansion of the Error matrix
# this leads to an assignment in the loop when the next matrix B is derived from one term of the error matrix block diag expansion (see Fig 2 in the paper or Fig 9.1 in the book)
# ... I just can't wrap my head around it

## Sub-step 1: block diagonal expansion of the condensation matrix - there are "levels - 1" of them
node_names_CONDENSATION <- rownames(condensation_m)                                                    # update the nodes list, ordered as in the condensation matrix (block triangualr)
comp_names_CONDENSATION <- comp_table_names[match(node_names_CONDENSATION, node_names[comp_table])]
## from scratch version
# expand_condensation_m <- lapply(1:(n_levels), function(y){  
#   expansion_term <- y
#   expansion_term_MATRIX <- matrix(0L, nrow = nrow(condensation_m), ncol = ncol(condensation_m))      # prepare an empty matrix to fill with elements along the specific block diagonal that corresponds to this iteration
#   rownames(expansion_term_MATRIX) <- rownames(condensation_m)
#   colnames(expansion_term_MATRIX) <- colnames(condensation_m)
#   n_levels_next <- n_levels - expansion_term + 1                          # to avoid out of bound
#   for(i in 1:n_levels_next){
#     x <- relabel_levels[[i]]
#     idx_COL <- match(x, comp_names_CONDENSATION)
#     if(expansion_term == 1){                                              # this is the main diagonal
#       idx_ROW <- idx_COL
#     } else {
#       x2 <- relabel_levels[[i + expansion_term - 1]]                      # displace the rows to pick to one or more levels below (it's a lower block diagonal)
#       idx_ROW <-  match(x2, comp_names_CONDENSATION)
#     }
#     expansion_term_MATRIX[idx_ROW, idx_COL] <- condensation_m[idx_ROW, idx_COL]                     # copy from condensation to the expansion term, block by block as these are picked along the block diagonal
#   }
#   expansion_term_MATRIX
# })

## function call version - we will need it again later
expand_condensation_m <- block_diag_EXPAND(condensation_m, n_levels)


## Sub-step 2: find Skeleton matrix
if(n_levels >= 2){                                                                       # don't really know what to do if this doesn't happen - Warfield doesn't say
  # this part is fixed so we need at least 3 leveles
  B <- list()
  B[[1]] <- expand_condensation_m[[1]]
  B[[2]] <- expand_condensation_m[[2]]
  i_B <- 3
  
  skeleton_STOP <- FALSE
  iter <- 1
  while (!skeleton_STOP & iter < (n_levels)){
    n_in_B <-  length(B)
    beta_m <- matrix(0L, nrow = nrow(condensation_m), ncol = ncol(condensation_m))      # prepare an empty matrix
    rownames(beta_m) <- rownames(condensation_m)
    colnames(beta_m) <- colnames(condensation_m)
    for(j in 1:n_in_B){
      beta_m <- beta_m + B[[j]]
    }
    beta_m_p <- m_pow(beta_m, (n_levels-1))                                             # use my own function
    errow <- condensation_m - beta_m_p$Binary                                           # approximation error: how well is the condensation matrix approximated by powering beta?
    if(sum(errow) > 0){
      # expand error
      E <- block_diag_EXPAND(errow, n_levels)
      B[[i_B]] <- E[[i_B]]
      iter <- iter + 1
      i_B <- i_B + 1
    } else {
      skeleton_STOP <- TRUE
      skeleton_m <- beta_m - diag(nrow(beta_m))
    }
    if(iter == n_levels & !skeleton_STOP){warning("no skeleton matrix found")}
  }
}





