
# EU H2020 PASSION
# Planning tool
# Jose Alberto Hernandez
# May 2021



# Inputs:
# Network topology and traffic (nodesLabeling and crossmatrix)
# Passion OSNR characterisation for lightpaths
# Passion cost values 

# Output:
# Lightpaths, both primary and secondary, and their allocation in the fibre/wavelengths (First-Fit)
# Node dimensioning (number of ROADM degrees and Passion S-BVTs)
# Cost per node and total


# Required libraries

library(igraph)

setwd("D:/github_projects/PASSION_jose")
#setwd("~/Google Drive/Research/Proyectos/PASSION_jose")
#setwd("~/github_projects")

rm(list=ls())

options(warn=-1)



# Auxiliar functions:

# Graph preparation 
get_gPass <- function(nodes.df, connectivity.mat, distCoeff = 1) {
  
  gPass = graph_from_adjacency_matrix(connectivity.mat, mode = c("undirected"), 
                                      weighted = TRUE, diag = TRUE, add.colnames = NULL, add.rownames = NA)
  V(gPass)$name = paste(nodes.df$Nodes,nodes.df$Types,sep="_")
  V(gPass)$type = as.character(nodes.df$Types)
  E(gPass)$name = paste(get.edgelist(gPass)[,1],get.edgelist(gPass)[,2],sep="--")
  
  # Removing HL5s
  gPass = delete_vertices(gPass, V(gPass)[which(V(gPass)$type=="HL5")])
  
  return(gPass)
}



# Main code



alpha = 1

# Loading topology
print("Loading Topology and OSNR configuration")

# Choose nodesLabeling_Germany.csv, nodesLabeling_Tokyo.csv, nodesLabeling_Milano.csv, nodesLabeling_Mexico_short.csv, 
nodes.df = read.csv(file="nodesLabeling_Tokyo.csv", sep=";", header=F); 
colnames(nodes.df) = c("Nodes","Types","Traffic")
nodes.df$Types = as.character(nodes.df$Types)

# Choose crossMatrix_Germany.csv, crossMatrix_Tokyo.csv, crossMatrix_Milano.csv, crossMatrix_Mexico_short.csv
connectivity.mat = alpha*as.matrix(read.csv(file="crossMatrix_Tokyo.csv", sep = ";",header=F))


nodes.df[which(nodes.df$Types=="HL5"),"Types"] = "HL5"
nodes.df[which(nodes.df$Types=="HL4"),"Types"] = "HL4"
nodes.df[which(nodes.df$Types=="HL3"),"Types"] = "HL3"
nodes.df[which(nodes.df$Types=="HL2"),"Types"] = "HL12"
nodes.df$Types = factor(nodes.df$Types)
rownames(nodes.df)=paste(nodes.df$Nodes,nodes.df$Types,sep="_"); 

colnames(connectivity.mat)=paste(nodes.df$Nodes,nodes.df$Types,sep="_"); 
rownames(connectivity.mat)=paste(nodes.df$Nodes,nodes.df$Types,sep="_"); 

# OSNR values
osnr_25G.mat = read.csv("osnr_25_oh_fec.csv",header=TRUE,sep=";")
osnr_40G.mat = read.csv("osnr_40_oh_fec.csv",header=TRUE,sep=";")
osnr_50G.mat = read.csv("osnr_50_oh_fec.csv",header=TRUE,sep=";")



# Load graph
gPass = get_gPass(nodes.df,connectivity.mat)

N_HL12s = length(which(V(gPass)$type=="HL12"))
N_HL3s = length(which(V(gPass)$type=="HL3"))
N_HL4s = length(which(V(gPass)$type=="HL4"))

if ("Traffic" %in% colnames(nodes.df)) {
  demand_matrix = nodes.df$Traffic  
  Traff = mean(nodes.df$Traffic)
} else {
  Traff = 600; # 600G per HL4 toward HL12
  demand_matrix = rnorm(N_HL4s,mean=Traff,sd=0.2*Traff)
}


FFallocation = as.data.frame(matrix(NA,
                                    nrow=length(E(gPass)),
                                    ncol=40*ceiling(ceiling(0.6*Traff/50*length(V(gPass)))/40)))

Sallocation = FFallocation; Dallocation = FFallocation


nlambdas_HL12s = 0*(1:length(V(gPass)[which(V(gPass)$type=="HL12")]))
nlambdas_HL3s = 0*(1:length(V(gPass)[which(V(gPass)$type=="HL3")]))
nlambdas_HL4s = 0*(1:length(V(gPass)[which(V(gPass)$type=="HL4")]))
speed_HL3s = 0*(1:length(V(gPass)[which(V(gPass)$type=="HL3")]))
speed_HL4s = 0*(1:length(V(gPass)[which(V(gPass)$type=="HL4")]))


Results.df = data.frame(matrix(c(1:18),nrow=1,ncol=18), stringsAsFactors = FALSE)

colnames(Results.df) = c("Source","Destination","prim_sec",
              "distance_KM","distance_hops",
              "N_HL5s","N_HL4s","N_HL3s","N_HL12s",
              "OSNR_e2e","OSNR_req50G","OSNR_req40G","OSNR_req25G",
              "Can_50G","Can_40G","Can_25G",
              "FullPath","LinksDistance")



E(gPass)$traff = 0; 
ll_traff = E(gPass)$traff
ll_links = E(gPass)$name


print("Finding lightpaths for HL4 nodes")
n_exec = 0;
for (HL4index in (1:N_HL4s)) { #N_HL4s)) {
  n_exec = n_exec + 1;
  
  gPass = get_gPass(nodes.df,connectivity.mat)
  HL12s = V(gPass)[which(V(gPass)$type=="HL12")]
  HL3s = V(gPass)[which(V(gPass)$type=="HL3")]
  HL4s = V(gPass)[which(V(gPass)$type=="HL4")]
  HL5s = V(gPass)[which(V(gPass)$type=="HL5")]
  
  E(gPass)$traff = ll_traff
  
  Source_node = V(gPass)[which(V(gPass)$type=="HL4")][HL4index]
  Source = V(gPass)[which(V(gPass)$type=="HL4")][HL4index]$name
  
  aa_minhops = get.shortest.paths(gPass, 
                                  from = V(gPass)[which(V(gPass)$name==Source)], #from = V(gPass)[which(V(gPass)$type=="HL4")][HL4index], 
                                  to = V(gPass)[which(V(gPass)$type=="HL12")], 
                                  output = 'epath',
                                  weights = NA)
  
  aa_minKm = get.shortest.paths(gPass, 
                                from = V(gPass)[which(V(gPass)$name==Source)], #from = V(gPass)[which(V(gPass)$type=="HL4")][HL4index], 
                                to = V(gPass)[which(V(gPass)$type=="HL12")], 
                                output = 'epath')
  
  aux_HL12_winner = unlist(lapply(aa_minhops$epath,length))*1e5
  
  for (ii in c(1:length(aux_HL12_winner))){
    aux_HL12_winner[ii] = aux_HL12_winner[ii] + sum(aa_minKm$epath[[ii]]$weight) 
  }
  
  HL12_winner_prim = which(aux_HL12_winner==min(aux_HL12_winner))
  
  
  # Primary path
  
  HL12_winner_prim = order(unlist(lapply(aa_minhops$epath,length)),decreasing = F)[1]

  Destination = HL12s[HL12_winner_prim]$name
  Destination_node = HL12s[HL12_winner_prim]
    
  aa_primary_e = get.shortest.paths(gPass, 
                                    from = Source_node, 
                                    to = Destination_node, 
                                    output = 'epath',
                                    weights = NA)
  
  aa_primary_v = get.shortest.paths(gPass, 
                                    from = Source_node, 
                                    to = Destination_node, 
                                    output = 'vpath',
                                    weights = NA)
  
  E(gPass)$traff = ll_traff
  E(gPass)[which(E(gPass) %in% aa_primary_e$epath[[1]])]$traff = E(gPass)[which(E(gPass) %in% aa_primary_e$epath[[1]])]$traff +1
  ll_traff = E(gPass)$traff
  if (n_exec == 1) {
    append_var = F;
  }else{
    append_var = T;
  }
  write.table( t(aa_primary_v$vpath[[1]]$name),  
               file="primary_path.csv", 
               append = append_var, 
               sep=';', 
               row.names=F, 
               col.names=F )
  
  #write.csv(aa_primary_v$vpath[[1]]$name, file = "primary_path.csv", row.names = FALSE) # guarda un archivo csv
  PrimaryPath = paste0(aa_primary_v$vpath[[1]]$name,collapse="++++")
  Node_sequence = PrimaryPath
  
  # node sequence metrics
  disthops_winner = length(aa_primary_v$vpath[[1]])-1
  HL5_hops = sum(aa_primary_v$vpath[[1]] %in% HL5s)
  HL4_hops = sum(aa_primary_v$vpath[[1]] %in% HL4s)
  HL3_hops = sum(aa_primary_v$vpath[[1]] %in% HL3s)
  HL12_hops = sum(aa_primary_v$vpath[[1]] %in% HL12s)
  
  # 1 amplifier per link
  dist_links = aa_primary_e$epath[[1]]$weight
  distKm_winner = sum(aa_primary_e$epath[[1]]$weight)
  Link_sequence = paste0(dist_links,collapse = " ++++ ")  # in km
  osnr_e2e = -10*log10(sum(10^(-0.1*(58-6-0.25*dist_links)))) 
  # similar to 58-0.25*sum(dist_links)-6-10*log10(length(dist_links)-1) , but the above is exact
  
  # osnr of path
  osnr_req_50G = osnr_50G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
  osnr_req_40G = osnr_40G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
  osnr_req_25G = osnr_25G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
  
  can50G = ifelse(osnr_e2e>osnr_req_50G,TRUE,FALSE)
  can40G = ifelse(osnr_e2e>osnr_req_40G,TRUE,FALSE)
  can25G = ifelse(osnr_e2e>osnr_req_25G,TRUE,FALSE)
  
  if (can50G == TRUE) {
    maxSpeed = 50
  } else {
    if (can40G == TRUE) {
      maxSpeed = 40
    } else {
      if (can25G == TRUE) {
        maxSpeed = 25
      } else {
        maxSpeed = 0 
      }
    }
  }
  nlambdas = ceiling(demand_matrix[HL4index]/maxSpeed)
  
  # we do the first fit allocation
  eindex = which(E(gPass) %in% aa_primary_e$epath[[1]])
  conditionFF = FALSE
  elambda = 0
  while (conditionFF == FALSE) {
    elambda = elambda + 1 
    if (prod(is.na(FFallocation[eindex,(elambda:(elambda+nlambdas-1))])) == 1) { # hueco libre
      FFallocation[eindex, elambda:(elambda+nlambdas-1)] = paste("lightpath",Source,Destination, sep = "++")
      Sallocation[eindex, elambda:(elambda+nlambdas-1)] = Source
      Dallocation[eindex, elambda:(elambda+nlambdas-1)] = Destination
      conditionFF = TRUE
    } else { # ocupado, sigo buscando
      conditionFF = FALSE
    }
  }
  
  speed_HL4s[HL4index] = maxSpeed
  nlambdas_HL4s[HL4index] = nlambdas_HL4s[HL4index] + nlambdas
  nlambdas_HL12s[which(HL12s$name == Destination)] = nlambdas_HL12s[which(HL12s$name == Destination)] + nlambdas
  
  datapoint = c(Source, Destination, "Primary_path",
                distKm_winner, disthops_winner,
                HL5_hops, HL4_hops, HL3_hops, HL12_hops,
                osnr_e2e, osnr_req_50G, osnr_req_40G, osnr_req_25G,
                ifelse(osnr_e2e>osnr_req_50G,TRUE,FALSE),
                ifelse(osnr_e2e>osnr_req_40G,TRUE,FALSE),
                ifelse(osnr_e2e>osnr_req_25G,TRUE,FALSE),
                Node_sequence,Link_sequence)
  Results.df = rbind(Results.df,datapoint)
  
  colnames(Results.df) = c("Source","Destination","prim_sec",
                           "distance_KM","distance_hops",
                           "N_HL5s","N_HL4s","N_HL3s","N_HL12s",
                           "OSNR_e2e","OSNR_req50G","OSNR_req40G","OSNR_req25G",
                           "Can_50G","Can_40G","Can_25G",
                           "FullPath","LinksDistance")
  
  
  
  # Plan-B secondary path
  
  all_weights_orig = E(gPass)$weight
  weights_aux = all_weights_orig
  weights_aux[(E(gPass) %in% aa_primary_e$epath[[1]])] = weights_aux[(E(gPass) %in% aa_primary_e$epath[[1]])] + 1000
  E(gPass)$weight = weights_aux
  
  aa_minKm_plan_b = get.shortest.paths(gPass, 
                                from = V(gPass)[which(V(gPass)$name==Source)], #from = V(gPass)[which(V(gPass)$type=="HL4")][HL4index], 
                                to = V(gPass)[which(V(gPass)$type=="HL12")], 
                                output = 'epath')
  
  aux_HL12_winner_sec_plan_b = unlist(lapply(aa_minKm_plan_b$epath,length))*1e5
  for (ii in c(1:length(aux_HL12_winner_sec_plan_b))){
    aux_HL12_winner_sec_plan_b[ii] = aux_HL12_winner_sec_plan_b[ii] + sum(aa_minKm_plan_b$epath[[ii]]$weight) 
  }
  
  HL12_winner_sec_planb = which(aux_HL12_winner_sec_plan_b==min(aux_HL12_winner_sec_plan_b))
  
  if (HL12_winner_prim == HL12_winner_sec_planb) {
    HL12_winner_sec_planb = order(aux_HL12_winner_sec_plan_b,decreasing = F)[2]
  }
  
  Destination = HL12s[HL12_winner_sec_planb]$name
  Destination_node = HL12s[HL12_winner_sec_planb]
  
  aa_secondary_e_planb = get.shortest.paths(gPass, 
                                            from = Source_node, 
                                            to = Destination_node, 
                                            output = 'epath')
  
  aa_secondary_v_planb = get.shortest.paths(gPass, 
                                            from = Source_node, 
                                            to = Destination_node, 
                                            output = 'vpath')
  
  write.table( t(aa_secondary_v_planb$vpath[[1]]$name),  
               file="secondary_path.csv", 
               append = append_var, 
               sep=';', 
               row.names=F, 
               col.names=F )
  
  SecondaryPath_PlanB = paste0(aa_secondary_v_planb$vpath[[1]]$name,collapse="++++")
  
  Destination = HL12s[HL12_winner_sec_planb]$name
  
  Node_sequence = SecondaryPath_PlanB
  
  # node sequence metrics
  disthops_winner = length(aa_secondary_v_planb$vpath[[1]])-1
  HL5_hops = sum(aa_secondary_v_planb$vpath[[1]] %in% HL5s)
  HL4_hops = sum(aa_secondary_v_planb$vpath[[1]] %in% HL4s)
  HL3_hops = sum(aa_secondary_v_planb$vpath[[1]] %in% HL3s)
  HL12_hops = sum(aa_secondary_v_planb$vpath[[1]] %in% HL12s)
  
  # 1 amplifier per link
  dist_links = aa_secondary_e_planb$epath[[1]]$weight
  dist_links[which(dist_links>999)] = dist_links[which(dist_links>999)] -1000
  Nshared_links = floor(sum(aa_secondary_e_planb$epath[[1]]$weight)/1000)
  Nshared_nodes = sum(as.numeric(aa_secondary_v_planb$vpath[[1]] %in% aa_primary_v$vpath[[1]]))-1
  
  distKm_winner = sum(dist_links)
  
  Link_sequence = paste0(dist_links,collapse = " ++++ ")  # in km
  osnr_e2e = -10*log10(sum(10^(-0.1*(58-6-0.25*dist_links)))) 
  
  # osnr of path
  osnr_req_50G = osnr_50G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
  osnr_req_40G = osnr_40G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
  osnr_req_25G = osnr_25G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
  
  
  # Secondary path, link and node disjoint
  
  E(gPass)$weight = all_weights_orig
  
  gPass = delete_vertices(gPass, aa_primary_v$vpath[[1]][2:length(aa_primary_v$vpath[[1]])])
  HL12s = V(gPass)[which(V(gPass)$type=="HL12")]
  HL3s = V(gPass)[which(V(gPass)$type=="HL3")]
  HL4s = V(gPass)[which(V(gPass)$type=="HL4")]
  HL5s = V(gPass)[which(V(gPass)$type=="HL5")]
  
  
  aa_secondary_e = get.shortest.paths(gPass,
                                      from = V(gPass)[which(V(gPass)$name==Source)], #from = V(gPass)[which(V(gPass)$type=="HL4")][HL4index], 
                                      to = V(gPass)[which(V(gPass)$type=="HL12")], 
                                      output = 'epath', 
                                      weights=NA)
  
  aa_secondary_minKm_e = get.shortest.paths(gPass, 
                                from = V(gPass)[which(V(gPass)$name==Source)], #from = V(gPass)[which(V(gPass)$type=="HL4")][HL4index], 
                                to = V(gPass)[which(V(gPass)$type=="HL12")], 
                                output = 'epath')
  
  aux_HL12_winner_sec = unlist(lapply(aa_secondary_minKm_e$epath,length))*1e5
  for (ii in c(1:length(aux_HL12_winner_sec))){
    aux_HL12_winner_sec[ii] = aux_HL12_winner_sec[ii] + sum(aa_secondary_minKm_e$epath[[ii]]$weight) 
  }

  HL12s_winner_sec = which(aux_HL12_winner_sec==min(aux_HL12_winner_sec))
  

  if (min(unlist(lapply(aa_secondary_e$epath,length)))==0) {
    
    # node unreachable
    
    datapoint = c(Source, Destination, paste("Secondary_path_shared_",Nshared_nodes,"nodes",Nshared_links,"links",sep=""),
                  distKm_winner, disthops_winner,
                  HL5_hops, HL4_hops, HL3_hops, HL12_hops,
                  osnr_e2e, osnr_req_50G, osnr_req_40G, osnr_req_25G,
                  ifelse(osnr_e2e>osnr_req_50G,TRUE,FALSE),
                  ifelse(osnr_e2e>osnr_req_40G,TRUE,FALSE),
                  ifelse(osnr_e2e>osnr_req_25G,TRUE,FALSE),
                  Node_sequence,Link_sequence)
    Results.df = rbind(Results.df,datapoint)
    
    
  } else {
    
    HL12s_winner_sec = order(unlist(lapply(aa_secondary_e$epath,length)),decreasing = F)[1]
    
    Destination = V(gPass)[which(V(gPass)$type=="HL12")][HL12s_winner_sec]$name
    
    aa_secondary_e = get.shortest.paths(gPass, 
                                        from = V(gPass)[which(V(gPass)$name==Source)], #from = V(gPass)[which(V(gPass)$type=="HL4")][HL4index], 
                                        to = V(gPass)[which(V(gPass)$type=="HL12")][HL12s_winner_sec], 
                                        output = 'epath',
                                        weights = NA)
    
    
    aa_secondary_v = get.shortest.paths(gPass, 
                                        from = V(gPass)[which(V(gPass)$name==Source)], #from = V(gPass)[which(V(gPass)$type=="HL4")][HL4index], 
                                        to = V(gPass)[which(V(gPass)$type=="HL12")][HL12s_winner_sec], 
                                        output = 'vpath',
                                        weights = NA)
    
    SecondaryPath = paste0(aa_secondary_v$vpath[[1]]$name,collapse="++++")
    Node_sequence = SecondaryPath
    
    # node sequence metrics
    disthops_winner = length(aa_secondary_v$vpath[[1]])-1
    HL5_hops = sum(aa_secondary_v$vpath[[1]] %in% HL5s)
    HL4_hops = sum(aa_secondary_v$vpath[[1]] %in% HL4s)
    HL3_hops = sum(aa_secondary_v$vpath[[1]] %in% HL3s)
    HL12_hops = sum(aa_secondary_v$vpath[[1]] %in% HL12s)
    
    # 1 amplifier per link
    dist_links = aa_secondary_e$epath[[1]]$weight
    distKm_winner = sum(aa_secondary_e$epath[[1]]$weight)
    Link_sequence = paste0(dist_links,collapse = " ++++ ")  # in km
    osnr_e2e = -10*log10(sum(10^(-0.1*(58-6-0.25*dist_links)))) 
    
    # osnr of path
    osnr_req_50G = osnr_50G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
    osnr_req_40G = osnr_40G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
    osnr_req_25G = osnr_25G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
    
    datapoint = c(Source, Destination, "Secondary_path_totally_disjoint",
                  distKm_winner, disthops_winner,
                  HL5_hops, HL4_hops, HL3_hops, HL12_hops,
                  osnr_e2e, osnr_req_50G, osnr_req_40G, osnr_req_25G,
                  ifelse(osnr_e2e>osnr_req_50G,TRUE,FALSE),
                  ifelse(osnr_e2e>osnr_req_40G,TRUE,FALSE),
                  ifelse(osnr_e2e>osnr_req_25G,TRUE,FALSE),
                  Node_sequence,Link_sequence)
    Results.df = rbind(Results.df,datapoint)
    
    }
  
}


print("Finding lightpaths for HL3 nodes")

for (HL3index in (1:N_HL3s)) { #N_HL4s)) {
  
  gPass = get_gPass(nodes.df,connectivity.mat)
  HL12s = V(gPass)[which(V(gPass)$type=="HL12")]
  HL3s = V(gPass)[which(V(gPass)$type=="HL3")]
  HL4s = V(gPass)[which(V(gPass)$type=="HL4")]
  HL5s = V(gPass)[which(V(gPass)$type=="HL5")]
  
  E(gPass)$traff = ll_traff
  
  Source_node = V(gPass)[which(V(gPass)$type=="HL3")][HL3index]
  Source = V(gPass)[which(V(gPass)$type=="HL3")][HL3index]$name
  
  aa_minhops = get.shortest.paths(gPass, 
                                  from = V(gPass)[which(V(gPass)$name==Source)], #from = V(gPass)[which(V(gPass)$type=="HL4")][HL4index], 
                                  to = V(gPass)[which(V(gPass)$type=="HL12")], 
                                  output = 'epath',
                                  weights = NA)
  
  aa_minKm = get.shortest.paths(gPass, 
                                from = V(gPass)[which(V(gPass)$name==Source)], #from = V(gPass)[which(V(gPass)$type=="HL4")][HL4index], 
                                to = V(gPass)[which(V(gPass)$type=="HL12")], 
                                output = 'epath')
  
  
  aux_HL12_winner = unlist(lapply(aa_minhops$epath,length))*1e5
  for (ii in c(1:length(aux_HL12_winner))){
    aux_HL12_winner[ii] = aux_HL12_winner[ii] + sum(aa_minKm$epath[[ii]]$weight) 
  }
  
  HL12_winner_prim = which(aux_HL12_winner==min(aux_HL12_winner))
  
  
  # Primary path
  
  HL12_winner_prim = order(unlist(lapply(aa_minhops$epath,length)),decreasing = F)[1]
  
  Destination = HL12s[HL12_winner_prim]$name
  Destination_node = HL12s[HL12_winner_prim]
  
  aa_primary_e = get.shortest.paths(gPass, 
                                    from = Source_node, 
                                    to = Destination_node, 
                                    output = 'epath',
                                    weights = NA)
  
  aa_primary_v = get.shortest.paths(gPass, 
                                    from = Source_node, 
                                    to = Destination_node, 
                                    output = 'vpath',
                                    weights = NA)
  
  E(gPass)$traff = ll_traff
  E(gPass)[which(E(gPass) %in% aa_primary_e$epath[[1]])]$traff = E(gPass)[which(E(gPass) %in% aa_primary_e$epath[[1]])]$traff +1
  ll_traff = E(gPass)$traff
  
  PrimaryPath = paste0(aa_primary_v$vpath[[1]]$name,collapse="++++")
  Node_sequence = PrimaryPath
  
  # node sequence metrics
  disthops_winner = length(aa_primary_v$vpath[[1]])-1
  HL5_hops = sum(aa_primary_v$vpath[[1]] %in% HL5s)
  HL4_hops = sum(aa_primary_v$vpath[[1]] %in% HL4s)
  HL3_hops = sum(aa_primary_v$vpath[[1]] %in% HL3s)
  HL12_hops = sum(aa_primary_v$vpath[[1]] %in% HL12s)
  
  # 1 amplifier per link
  dist_links = aa_primary_e$epath[[1]]$weight
  distKm_winner = sum(aa_primary_e$epath[[1]]$weight)
  Link_sequence = paste0(dist_links,collapse = " ++++ ")  # in km
  osnr_e2e = -10*log10(sum(10^(-0.1*(58-6-0.25*dist_links)))) 
  
  # osnr of path
  osnr_req_50G = osnr_50G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
  osnr_req_40G = osnr_40G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
  osnr_req_25G = osnr_25G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
  
  can50G = ifelse(osnr_e2e>osnr_req_50G,TRUE,FALSE)
  can40G = ifelse(osnr_e2e>osnr_req_40G,TRUE,FALSE)
  can25G = ifelse(osnr_e2e>osnr_req_25G,TRUE,FALSE)
  
  if (can50G == TRUE) {
    maxSpeed = 50
  } else {
    if (can40G == TRUE) {
      maxSpeed = 40
    } else {
      if (can25G == TRUE) {
        maxSpeed = 25
      } else {
        maxSpeed = 0 
      }
    }
  }
  nlambdas = ceiling(demand_matrix[HL3index]/maxSpeed)
  
  # we do the first fit allocation
  eindex = which(E(gPass) %in% aa_primary_e$epath[[1]])
  conditionFF = FALSE
  elambda = 0
  while (conditionFF == FALSE) {
    elambda = elambda + 1 
    if (prod(is.na(FFallocation[eindex,(elambda:(elambda+nlambdas-1))])) == 1) { # hueco libre
      FFallocation[eindex, elambda:(elambda+nlambdas-1)] = paste("lightpath",Source,Destination, sep = "++")
      Sallocation[eindex, elambda:(elambda+nlambdas-1)] = Source
      Dallocation[eindex, elambda:(elambda+nlambdas-1)] = Destination
      conditionFF = TRUE
    } else { # ocupado, sigo buscando
      conditionFF = FALSE
    }
  }
  
  speed_HL3s[HL3index] = maxSpeed
  nlambdas_HL3s[HL3index] = nlambdas_HL3s[HL3index] + nlambdas
  nlambdas_HL12s[which(HL12s$name == Destination)] = nlambdas_HL12s[which(HL12s$name == Destination)] + nlambdas
  
  datapoint = c(Source, Destination, "Primary_path",
                distKm_winner, disthops_winner,
                HL5_hops, HL4_hops, HL3_hops, HL12_hops,
                osnr_e2e, osnr_req_50G, osnr_req_40G, osnr_req_25G,
                ifelse(osnr_e2e>osnr_req_50G,TRUE,FALSE),
                ifelse(osnr_e2e>osnr_req_40G,TRUE,FALSE),
                ifelse(osnr_e2e>osnr_req_25G,TRUE,FALSE),
                Node_sequence,Link_sequence)
  Results.df = rbind(Results.df,datapoint)
  
  colnames(Results.df) = c("Source","Destination","prim_sec",
                           "distance_KM","distance_hops",
                           "N_HL5s","N_HL4s","N_HL3s","N_HL12s",
                           "OSNR_e2e","OSNR_req50G","OSNR_req40G","OSNR_req25G",
                           "Can_50G","Can_40G","Can_25G",
                           "FullPath","LinksDistance")
  
  
  # Plan-B secondary path
  
  all_weights_orig = E(gPass)$weight
  weights_aux = all_weights_orig
  weights_aux[(E(gPass) %in% aa_primary_e$epath[[1]])] = weights_aux[(E(gPass) %in% aa_primary_e$epath[[1]])] + 1000
  E(gPass)$weight = weights_aux
  
  aa_minKm_plan_b = get.shortest.paths(gPass, 
                                       from = V(gPass)[which(V(gPass)$name==Source)], #from = V(gPass)[which(V(gPass)$type=="HL4")][HL4index], 
                                       to = V(gPass)[which(V(gPass)$type=="HL12")], 
                                       output = 'epath')
  
  aux_HL12_winner_sec_plan_b = unlist(lapply(aa_minKm_plan_b$epath,length))*1e5
  for (ii in c(1:length(aux_HL12_winner_sec_plan_b))){
    aux_HL12_winner_sec_plan_b[ii] = aux_HL12_winner_sec_plan_b[ii] + sum(aa_minKm_plan_b$epath[[ii]]$weight) 
  }
  
  HL12_winner_sec_planb = which(aux_HL12_winner_sec_plan_b==min(aux_HL12_winner_sec_plan_b))
  
  if (HL12_winner_prim == HL12_winner_sec_planb) {
    HL12_winner_sec_planb = order(aux_HL12_winner_sec_plan_b,decreasing = F)[2]
  }
  
  Destination = HL12s[HL12_winner_sec_planb]$name
  Destination_node = HL12s[HL12_winner_sec_planb]
  
  aa_secondary_e_planb = get.shortest.paths(gPass, 
                                            from = Source_node, 
                                            to = Destination_node, 
                                            output = 'epath')
  
  aa_secondary_v_planb = get.shortest.paths(gPass, 
                                            from = Source_node, 
                                            to = Destination_node, 
                                            output = 'vpath')
  
  SecondaryPath_PlanB = paste0(aa_secondary_v_planb$vpath[[1]]$name,collapse="++++")
  
  Destination = HL12s[HL12_winner_sec_planb]$name
  
  Node_sequence = SecondaryPath_PlanB
  
  # node sequence metrics
  disthops_winner = length(aa_secondary_v_planb$vpath[[1]])-1
  HL5_hops = sum(aa_secondary_v_planb$vpath[[1]] %in% HL5s)
  HL4_hops = sum(aa_secondary_v_planb$vpath[[1]] %in% HL4s)
  HL3_hops = sum(aa_secondary_v_planb$vpath[[1]] %in% HL3s)
  HL12_hops = sum(aa_secondary_v_planb$vpath[[1]] %in% HL12s)
  
  # 1 amplifier per link
  dist_links = aa_secondary_e_planb$epath[[1]]$weight
  dist_links[which(dist_links>999)] = dist_links[which(dist_links>999)] -1000
  Nshared_links = floor(sum(aa_secondary_e_planb$epath[[1]]$weight)/1000)
  Nshared_nodes = sum(as.numeric(aa_secondary_v_planb$vpath[[1]] %in% aa_primary_v$vpath[[1]]))-1
  
  distKm_winner = sum(dist_links)
  
  Link_sequence = paste0(dist_links,collapse = " ++++ ")  # in km
  osnr_e2e = -10*log10(sum(10^(-0.1*(58-6-0.25*dist_links)))) 
  
  # osnr of path
  osnr_req_50G = osnr_50G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
  osnr_req_40G = osnr_40G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
  osnr_req_25G = osnr_25G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
  
  
  # Secondary path
  
  E(gPass)$weight = all_weights_orig
  
  gPass = delete_vertices(gPass, aa_primary_v$vpath[[1]][2:length(aa_primary_v$vpath[[1]])])
  HL12s = V(gPass)[which(V(gPass)$type=="HL12")]
  HL3s = V(gPass)[which(V(gPass)$type=="HL3")]
  HL4s = V(gPass)[which(V(gPass)$type=="HL4")]
  HL5s = V(gPass)[which(V(gPass)$type=="HL5")]
  
  
  aa_secondary_e = get.shortest.paths(gPass,
                                      from = V(gPass)[which(V(gPass)$name==Source)], #from = V(gPass)[which(V(gPass)$type=="HL4")][HL4index], 
                                      to = V(gPass)[which(V(gPass)$type=="HL12")], 
                                      output = 'epath', 
                                      weights=NA)
  
  aa_secondary_minKm_e = get.shortest.paths(gPass, 
                                            from = V(gPass)[which(V(gPass)$name==Source)], #from = V(gPass)[which(V(gPass)$type=="HL4")][HL4index], 
                                            to = V(gPass)[which(V(gPass)$type=="HL12")], 
                                            output = 'epath')
  
  aux_HL12_winner_sec = unlist(lapply(aa_secondary_minKm_e$epath,length))*1e5
  for (ii in c(1:length(aux_HL12_winner_sec))){
    aux_HL12_winner_sec[ii] = aux_HL12_winner_sec[ii] + sum(aa_secondary_minKm_e$epath[[ii]]$weight) 
  }
  
  HL12s_winner_sec = which(aux_HL12_winner_sec==min(aux_HL12_winner_sec))
  
  if (min(unlist(lapply(aa_secondary_e$epath,length)))==0) {
    
    # node unreachable
    
    datapoint = c(Source, Destination, paste("Secondary_path_shared_",Nshared_nodes,"nodes",Nshared_links,"links",sep=""),
                  distKm_winner, disthops_winner,
                  HL5_hops, HL4_hops, HL3_hops, HL12_hops,
                  osnr_e2e, osnr_req_50G, osnr_req_40G, osnr_req_25G,
                  ifelse(osnr_e2e>osnr_req_50G,TRUE,FALSE),
                  ifelse(osnr_e2e>osnr_req_40G,TRUE,FALSE),
                  ifelse(osnr_e2e>osnr_req_25G,TRUE,FALSE),
                  Node_sequence,Link_sequence)
    Results.df = rbind(Results.df,datapoint)
    
    
  } else {

    HL12s_winner_sec = order(unlist(lapply(aa_secondary_e$epath,length)),decreasing = F)[1]

    Destination = V(gPass)[which(V(gPass)$type=="HL12")][HL12s_winner_sec]$name
    
    aa_secondary_e = get.shortest.paths(gPass, 
                                        from = V(gPass)[which(V(gPass)$name==Source)], #from = V(gPass)[which(V(gPass)$type=="HL4")][HL4index], 
                                        to = V(gPass)[which(V(gPass)$type=="HL12")][HL12s_winner_sec], 
                                        output = 'epath',
                                        weights = NA)

    aa_secondary_v = get.shortest.paths(gPass, 
                                        from = V(gPass)[which(V(gPass)$name==Source)], #from = V(gPass)[which(V(gPass)$type=="HL4")][HL4index], 
                                        to = V(gPass)[which(V(gPass)$type=="HL12")][HL12s_winner_sec], 
                                        output = 'vpath',
                                        weights = NA)
    
    SecondaryPath = paste0(aa_secondary_v$vpath[[1]]$name,collapse="++++")
    Node_sequence = SecondaryPath
    
    # node sequence metrics
    disthops_winner = length(aa_secondary_v$vpath[[1]])-1
    HL5_hops = sum(aa_secondary_v$vpath[[1]] %in% HL5s)
    HL4_hops = sum(aa_secondary_v$vpath[[1]] %in% HL4s)
    HL3_hops = sum(aa_secondary_v$vpath[[1]] %in% HL3s)
    HL12_hops = sum(aa_secondary_v$vpath[[1]] %in% HL12s)
    
    # 1 amplifier per link
    dist_links = aa_secondary_e$epath[[1]]$weight
    distKm_winner = sum(aa_secondary_e$epath[[1]]$weight)
    Link_sequence = paste0(dist_links,collapse = " ++++ ")  # in km
    osnr_e2e = -10*log10(sum(10^(-0.1*(58-6-0.25*dist_links)))) 

    # osnr of path
    osnr_req_50G = osnr_50G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
    osnr_req_40G = osnr_40G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
    osnr_req_25G = osnr_25G.mat[HL4_hops+1,HL12_hops+HL3_hops+2]
    
    datapoint = c(Source, Destination, "Secondary_path_totally_disjoint",
                  distKm_winner, disthops_winner,
                  HL5_hops, HL4_hops, HL3_hops, HL12_hops,
                  osnr_e2e, osnr_req_50G, osnr_req_40G, osnr_req_25G,
                  ifelse(osnr_e2e>osnr_req_50G,TRUE,FALSE),
                  ifelse(osnr_e2e>osnr_req_40G,TRUE,FALSE),
                  ifelse(osnr_e2e>osnr_req_25G,TRUE,FALSE),
                  Node_sequence,Link_sequence)
    Results.df = rbind(Results.df,datapoint)
    
  }
  

}



dimFFalloc = ceiling(length(which(colSums(is.na(FFallocation))<dim(FFallocation)[1]))/40)*40

FFallocation_final = FFallocation[,c(1:dimFFalloc)]
Sallocation_final = Sallocation[,c(1:dimFFalloc)]
Dallocation_final = Dallocation[,c(1:dimFFalloc)]


heat_table = apply(FFallocation_final,2,is.na)
heat_table2 = matrix(as.numeric(heat_table), nrow=dim(heat_table)[1], ncol=dim(heat_table)[2], byrow=F)

heatmap(heat_table2, Colv = NA, Rowv = NA, scale="none", 
        xlab="Freq. Slots", ylab="links", main="First-Fit allocation")


colnames(Results.df) = c("Source","Destination","prim_sec",
                         "distance_KM","distance_hops",
                         "N_HL5s","N_HL4s","N_HL3s","N_HL12s",
                         "OSNR_e2e","OSNR_req50G","OSNR_req40G","OSNR_req25G",
                         "Can_50G","Can_40G","Can_25G",
                         "FullPath","LinksDistance")



print("Writing results in output files")

Results.df = Results.df[-1,]

write.csv(Results.df,file="lightpaths.csv")
write.csv(FFallocation_final,file="FFlightpaths.csv")



# Analysis of primary paths

Results.df$Source = as.factor(Results.df$Source)
Results.df$Destination = as.factor(Results.df$Destination)
Results.df$prim_sec = as.factor(Results.df$prim_sec)
Results.df$distance_KM = as.numeric(Results.df$distance_KM)
Results.df$distance_hops = as.numeric(Results.df$distance_hops)
Results.df$N_HL5s = as.numeric(Results.df$N_HL5s)
Results.df$N_HL4s = as.numeric(Results.df$N_HL4s)
Results.df$N_HL3s = as.numeric(Results.df$N_HL3s)
Results.df$N_HL12s = as.numeric(Results.df$N_HL12s)
Results.df$OSNR_e2e = as.numeric(Results.df$OSNR_e2e)
Results.df$OSNR_req50G = as.numeric(Results.df$OSNR_req50G)
Results.df$OSNR_req40G = as.numeric(Results.df$OSNR_req40G)
Results.df$OSNR_req25G = as.numeric(Results.df$OSNR_req25G)
Results.df[which(Results.df$OSNR_req50G>100),"OSNR_req50G"] = NA
Results.df[which(Results.df$OSNR_req40G>100),"OSNR_req40G"] = NA
Results.df[which(Results.df$OSNR_req25G>100),"OSNR_req25G"] = NA
Results.df$Can_50G = as.logical(Results.df$Can_50G)
Results.df$Can_40G = as.logical(Results.df$Can_40G)
Results.df$Can_25G = as.logical(Results.df$Can_25G)


Ppaths.df = Results.df[which(Results.df$prim_sec=="Primary_path"),]
SecPaths.df = Results.df[-which(Results.df$prim_sec=="Primary_path"),]



boxplot(Ppaths.df[,"distance_KM"], SecPaths.df[,"distance_KM"], 
        main = "Distance (KM)",
        at = c(1,2),
        names = c("primary","secondary"),
        las = 2,
        col = c("green","red"),
        border = "brown",
        horizontal = FALSE,
        notch = TRUE
)

boxplot(Ppaths.df[,"distance_hops"], SecPaths.df[,"distance_hops"], 
        main = "# Hops",
        at = c(1,2),
        names = c("primary","secondary"),
        las = 2,
        col = c("green","red"),
        border = "brown",
        horizontal = FALSE,
        notch = TRUE
)

boxplot(Ppaths.df[,"OSNR_e2e"], SecPaths.df[,"OSNR_e2e"], 
        main = "# End-to-End OSNR (dB)",
        at = c(1,2),
        names = c("primary","secondary"),
        las = 2,
        col = c("green","red"),
        border = "brown",
        horizontal = FALSE,
        notch = TRUE
)


Ppath_NA.df = na.omit(Ppaths.df)
SecPath_NA.df = na.omit(SecPaths.df)


# Node configuration

gPass = get_gPass(nodes.df,connectivity.mat)
HL12s = V(gPass)[which(V(gPass)$type=="HL12")]
HL3s = V(gPass)[which(V(gPass)$type=="HL3")]
HL4s = V(gPass)[which(V(gPass)$type=="HL4")]
HL5s = V(gPass)[which(V(gPass)$type=="HL5")]

Component_cost = read.csv("Passion_cost_components.csv",header=F, sep=";")

# HL4 configuration

HL4conf.df = data.frame(node_name = HL4s$name, 
                   nlambdas = nlambdas_HL4s,
                   speed = speed_HL4s, 
                   deg = degree(gPass)[which(V(gPass)$type=="HL4")], 
                   degRoadm = NA,
                   cost = rep(NA, length(HL4s)))


for (ii in HL4s) {
  aux  = which(E(gPass) %in% incident(gPass,V(gPass)[ii]))
  deg = 0
  for (jj in 1:length(aux)) {
    mm = (matrix(FFallocation[aux[jj],],nrow=(dim(FFallocation)[2]/40),ncol=40,byrow=TRUE))  
    deg = deg + ceiling(sum(abs(as.numeric(apply(mm,1,is.na))-1))/40)
  }
  HL4conf.df[V(gPass)[ii]$name,"degRoadm"] = deg
}

HL4conf.df$cost = Component_cost[which(Component_cost=="ROADM_degree"),2] * apply(HL4conf.df[,c("deg","degRoadm")],1,max) + 
  Component_cost[which(Component_cost=="SBVT"),2]/40 * HL4conf.df$nlambdas + Component_cost[which(Component_cost=="HL4_Router"),2]

# HL3 configuration

HL3conf.df = data.frame(node_name = HL3s$name, 
                        nlambdas = nlambdas_HL3s,
                        speed = speed_HL3s, 
                        deg = degree(gPass)[which(V(gPass)$type=="HL3")], 
                        degRoadm = NA,
                        cost = rep(NA, length(HL3s)))

for (ii in HL3s) {
  aux  = which(E(gPass) %in% incident(gPass,V(gPass)[ii]))
  deg = 0
  for (jj in 1:length(aux)) {
    mm = (matrix(FFallocation[aux[jj],],nrow=(dim(FFallocation)[2]/40),ncol=40,byrow=TRUE))  
    deg = deg + ceiling(sum(abs(as.numeric(apply(mm,1,is.na))-1))/40)
  }
  HL3conf.df[V(gPass)[ii]$name,"degRoadm"] = deg
}


HL3conf.df$cost = Component_cost[which(Component_cost=="ROADM_degree"),2] * apply(HL3conf.df[,c("deg","degRoadm")],1,max) + 
  Component_cost[which(Component_cost=="SBVT"),2]/40 * HL3conf.df$nlambdas + Component_cost[which(Component_cost=="HL4_Router"),2]

# HL12 configuration

HL12conf.df = data.frame(node_name = HL12s$name, 
                   nlambdas = nlambdas_HL12s,
                   deg = degree(gPass)[which(V(gPass)$type=="HL12")], 
                   degRoadm = NA,
                   cost = rep(NA,length(HL12s)))

for (ii in HL12s) {
  
  aux  = which(E(gPass) %in% incident(gPass,V(gPass)[ii]))
  deg = 0
  for (jj in 1:length(aux)) {
    mm = (matrix(FFallocation[aux[jj],],nrow=(dim(FFallocation)[2]/40),ncol=40,byrow=TRUE))  
    deg = deg + ceiling(sum(abs(as.numeric(apply(mm,1,is.na))-1))/40)
  }
  HL12conf.df[V(gPass)[ii]$name,"degRoadm"] = deg
}

# cost = roadm-degree + S-BVTs + router
HL12conf.df$cost = Component_cost[which(Component_cost=="ROADM_degree"),2] * apply(HL12conf.df[,c("deg","degRoadm")],1,max) + 
  Component_cost[which(Component_cost=="SBVT"),2]/40 * HL12conf.df$nlambdas + Component_cost[which(Component_cost=="HL12_Router"),2]


# Total cost 
TCO = sum(HL12conf.df$cost) + sum(HL3conf.df$cost) + sum(HL4conf.df$cost) 


write.csv(rbind(HL4conf.df[,colnames(HL4conf.df)[c(1:2,4:6)]],HL3conf.df[,colnames(HL3conf.df)[c(1:2,4:6)]],HL12conf.df), 
          file = "NodeDesign.csv")









