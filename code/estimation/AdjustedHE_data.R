#script to rep data files/ data file editing
library(ggplot2)
library(data.table)

#P_cov_model = manual adjusted HE (pos, neg, and zero)
#P_cov_noa_model = manual unadjusted HE (pos, neg, and zero)
#P_cov_gcta_model = GCTA unadjusted HE (only zero)

#load data
load_data <- function(model, P, cov){
  data_file <- fread(file = paste0("admix_", model, "_theta0.5_gen20_P", P, "_", cov, ".txt"), header = TRUE)
  return(data_file)
}

#pos/neg manual adjusted
P0_neg_HI<-load_data("HI","0","neg")
P0.3_neg_HI<-load_data("HI","0.3","neg")
P0.6_neg_HI<-load_data("HI","0.6","neg")
P0.9_neg_HI<-load_data("HI","0.9","neg")

P0_neg_CGF<-load_data("CGF","0","neg")
P0.3_neg_CGF<-load_data("CGF","0.3","neg")
P0.6_neg_CGF<-load_data("CGF","0.6","neg")
P0.9_neg_CGF<-load_data("CGF","0.9","neg")

P0_pos_HI<-load_data("HI","0","pos")
P0.3_pos_HI<-load_data("HI","0.3","pos")
P0.6_pos_HI<-load_data("HI","0.6","pos")
P0.9_pos_HI<-load_data("HI","0.9","pos")

P0_pos_CGF<-load_data("CGF","0","pos")
P0.3_pos_CGF<-load_data("CGF","0.3","pos")
P0.6_pos_CGF<-load_data("CGF","0.6","pos")
P0.9_pos_CGF<-load_data("CGF","0.9","pos")

#Zeros manual adjusted
P0_zero_HI<-load_data("HI","0","zero")
P0.3_zero_HI<-load_data("HI","0.3","zero")
P0.6_zero_HI<-load_data("HI","0.6","zero")
P0.9_zero_HI<-load_data("HI","0.9","zero")

P0_zero_CGF<-load_data("CGF","0","zero")
P0.3_zero_CGF<-load_data("CGF","0.3","zero")
P0.6_zero_CGF<-load_data("CGF","0.6","zero")
P0.9_zero_CGF<-load_data("CGF","0.9","zero")

#sort without rep for CI calculations
sort_data <- function(file_adj, file_noa, P, cov, model){
  file_adj[, seed := rep(1:10, each =21)]
  file_adj[, P := P]
  file_adj[, cov := cov]
  file_adj[, model := model]
  
  #for manually adjusted HE estimates
  setnames(file_adj, "G.off.coeff", "vg_HEreg_adj")
  #merge noa column to file_adj dataset
  file_adj[, vg_HEreg_noa := file_noa$G.off.coeff]
  
  file.sort <- file_adj[order(file_adj$t),]
  return(file.sort)
}

#negative unadjusted and adjusted (all data)
P0_neg_HI.sort <- sort_data(P0_neg_HI, P0_neg_noa_HI, 0, "neg", "HI")
P0.3_neg_HI.sort <- sort_data(P0.3_neg_HI, P0.3_neg_noa_HI, 0.3, "neg", "HI")
P0.6_neg_HI.sort <- sort_data(P0.6_neg_HI, P0.6_neg_noa_HI, 0.6, "neg", "HI")
P0.9_neg_HI.sort <- sort_data(P0.9_neg_HI, P0.9_neg_noa_HI, 0.9, "neg", "HI")

P0_neg_CGF.sort <- sort_data(P0_neg_CGF, P0_neg_noa_CGF, 0, "neg", "CGF")
P0.3_neg_CGF.sort <- sort_data(P0.3_neg_CGF, P0.3_neg_noa_CGF, 0.3, "neg", "CGF")
P0.6_neg_CGF.sort <- sort_data(P0.6_neg_CGF, P0.6_neg_noa_CGF, 0.6, "neg", "CGF")
P0.9_neg_CGF.sort <- sort_data(P0.9_neg_CGF, P0.9_neg_noa_CGF, 0.9, "neg", "CGF")

#positive unadjusted and adjusted (all data)
P0_pos_HI.sort <- sort_data(P0_pos_HI, P0_pos_noa_HI, 0, "pos", "HI")
P0.3_pos_HI.sort <- sort_data(P0.3_pos_HI, P0.3_pos_noa_HI, 0.3, "pos", "HI")
P0.6_pos_HI.sort <- sort_data(P0.6_pos_HI, P0.6_pos_noa_HI, 0.6, "pos", "HI")
P0.9_pos_HI.sort <- sort_data(P0.9_pos_HI, P0.9_pos_noa_HI, 0.9, "pos", "HI")

P0_pos_CGF.sort <- sort_data(P0_pos_CGF, P0_pos_noa_CGF, 0, "pos", "CGF")
P0.3_pos_CGF.sort <- sort_data(P0.3_pos_CGF, P0.3_pos_noa_CGF, 0.3, "pos", "CGF")
P0.6_pos_CGF.sort <- sort_data(P0.6_pos_CGF, P0.6_pos_noa_CGF, 0.6, "pos", "CGF")
P0.9_pos_CGF.sort <- sort_data(P0.9_pos_CGF, P0.9_pos_noa_CGF, 0.9, "pos", "CGF")

#zeros unadjusted and adjusted (all data)
P0_zero_HI.sort <- sort_data(P0_zero_HI, P0_zero_noa_HI, 0, "zero", "HI")
P0.3_zero_HI.sort <- sort_data(P0.3_zero_HI, P0.3_zero_noa_HI, 0.3, "zero", "HI")
P0.6_zero_HI.sort <- sort_data(P0.6_zero_HI, P0.6_zero_noa_HI, 0.6, "zero", "HI")
P0.9_zero_HI.sort <- sort_data(P0.9_zero_HI, P0.9_zero_noa_HI, 0.9, "zero", "HI")

P0_zero_CGF.sort <- sort_data(P0_zero_CGF, P0_zero_noa_CGF, 0, "zero", "CGF")
P0.3_zero_CGF.sort <- sort_data(P0.3_zero_CGF, P0.3_zero_noa_CGF, 0.3, "zero", "CGF")
P0.6_zero_CGF.sort <- sort_data(P0.6_zero_CGF, P0.6_zero_noa_CGF, 0.6, "zero", "CGF")
P0.9_zero_CGF.sort <- sort_data(P0.9_zero_CGF, P0.9_zero_noa_CGF, 0.9, "zero", "CGF")

#combine files
CGF_all <- rbind(P0_pos_CGF.sort, P0.3_pos_CGF.sort, 
                 P0.6_pos_CGF.sort, P0.9_pos_CGF.sort,
                 P0_neg_CGF.sort, P0.3_neg_CGF.sort, 
                 P0.6_neg_CGF.sort, P0.9_neg_CGF.sort,
                 P0_zero_CGF.sort, P0.3_zero_CGF.sort, 
                 P0.6_zero_CGF.sort, P0.9_zero_CGF.sort)

HI_all <- rbind(P0_pos_HI.sort, P0.3_pos_HI.sort, 
                P0.6_pos_HI.sort, P0.9_pos_HI.sort,
                P0_neg_HI.sort, P0.3_neg_HI.sort, 
                P0.6_neg_HI.sort, P0.9_neg_HI.sort,
                P0_zero_HI.sort, P0.3_zero_HI.sort, 
                P0.6_zero_HI.sort, P0.9_zero_HI.sort)

CGF_all <- CGF_all[order(CGF_all$t, CGF_all$P, CGF_all$cov)]
HI_all <- HI_all[order(HI_all$t, HI_all$P, HI_all$cov)]

write.table(HI_all, "admix_HI_theta0.5_gen20_all.txt", sep = "\t", quote = F, row.names = F)
write.table(CGF_all, "admix_CGF_theta0.5_gen20_all.txt", sep = "\t", quote = F, row.names = F)


#manual not adjusted (noa)
load_data_noa <- function(model, P, cov){
  data_file <- fread(file = paste0("admix_", model, "_theta0.5_gen20_P", P, "_", cov, "_noa.txt"), header = TRUE)
  return(data_file)
}

P0_zero_noa_HI <- load_data_noa("HI","0","zero")
P0.3_zero_noa_HI <- load_data_noa("HI","0.3","zero")
P0.6_zero_noa_HI <- load_data_noa("HI","0.6","zero")
P0.9_zero_noa_HI <- load_data_noa("HI","0.9","zero")

P0_zero_noa_CGF<-load_data_noa("CGF","0","zero")
P0.3_zero_noa_CGF<-load_data_noa("CGF","0.3","zero")
P0.6_zero_noa_CGF<-load_data_noa("CGF","0.6","zero")
P0.9_zero_noa_CGF<- load_data_noa("CGF","0.9","zero")

#sort and rep over 10
rep_data <- function(file, P, cov){
  file.sort <- file[order(file$t),]
  file.avg <- colSums(matrix(file.sort$G.off.coeff, nrow =10)/10)
  file.rep <- data.table(t=0:20, G.off.coeff=file.avg, P = P, cov = cov)
  return(file.rep)
}

#zeros manual not adjusted rep and sort
P0_zero_noa_HI.rep <- rep_data(P0_zero_noa_HI, 0, "zero")
P0.3_zero_noa_HI.rep <- rep_data(P0.3_zero_noa_HI, 0.3, "zero")
P0.6_zero_noa_HI.rep <- rep_data(P0.6_zero_noa_HI, 0.6, "zero")
P0.9_zero_noa_HI.rep <- rep_data(P0.9_zero_noa_HI, 0.9, "zero")

P0_zero_noa_CGF.rep <-rep_data(P0_zero_noa_CGF, 0, "zero")
P0.3_zero_noa_CGF.rep <-rep_data(P0.3_zero_noa_CGF, 0.3, "zero")
P0.6_zero_noa_CGF.rep <-rep_data(P0.6_zero_noa_CGF, 0.6, "zero")
P0.9_zero_noa_CGF.rep <- rep_data(P0.9_zero_noa_CGF, 0.9, "zero")


#pos/neg adjusted rep and sort
P0_pos_HI.rep <- rep_data(P0_pos_HI, 0, "pos")
P0.3_pos_HI.rep <- rep_data(P0.3_pos_HI, 0.3, "pos")
P0.6_pos_HI.rep <- rep_data(P0.6_pos_HI, 0.6, "pos")
P0.9_pos_HI.rep <- rep_data(P0.9_pos_HI, 0.9, "pos")

P0_pos_CGF.rep <-rep_data(P0_pos_CGF, 0, "pos")
P0.3_pos_CGF.rep <-rep_data(P0.3_pos_CGF, 0.3, "pos")
P0.6_pos_CGF.rep <-rep_data(P0.6_pos_CGF, 0.6, "pos")
P0.9_pos_CGF.rep <- rep_data(P0.9_pos_CGF, 0.9, "pos")

P0_neg_HI.rep <- rep_data(P0_neg_HI, 0, "neg")
P0.3_neg_HI.rep <- rep_data(P0.3_neg_HI, 0.3, "neg")
P0.6_neg_HI.rep <- rep_data(P0.6_neg_HI, 0.6, "neg")
P0.9_neg_HI.rep <- rep_data(P0.9_neg_HI, 0.9, "neg")

P0_neg_CGF.rep <-rep_data(P0_neg_CGF, 0, "neg")
P0.3_neg_CGF.rep <-rep_data(P0.3_neg_CGF, 0.3, "neg")
P0.6_neg_CGF.rep <-rep_data(P0.6_neg_CGF, 0.6, "neg")
P0.9_neg_CGF.rep <- rep_data(P0.9_neg_CGF, 0.9, "neg")

#Combine files for adjusted pos and neg
CGF_pos <- rbind(P0_pos_CGF.rep, P0.3_pos_CGF.rep, 
                     P0.6_pos_CGF.rep, P0.9_pos_CGF.rep)
HI_pos <- rbind(P0_pos_HI.rep, P0.3_pos_HI.rep, 
                    P0.6_pos_HI.rep, P0.9_pos_HI.rep)

CGF_neg <- rbind(P0_neg_CGF.rep, P0.3_neg_CGF.rep, 
                     P0.6_neg_CGF.rep, P0.9_neg_CGF.rep)
HI_neg <- rbind(P0_neg_HI.rep, P0.3_neg_HI.rep, 
                    P0.6_neg_HI.rep, P0.9_neg_HI.rep)

CGF_cov <- rbind(CGF_pos, CGF_neg)
HI_cov <- rbind(HI_pos, HI_neg)

CGF_cov <- CGF_cov[order(CGF_cov$t, CGF_cov$P, CGF_cov$cov)]
HI_cov <- HI_cov[order(HI_cov$t, HI_cov$P, HI_cov$cov)]

DickerHE_CGF$vg_HEreg_a<- CGF_cov$G.off.coeff
DickerHE_HI$vg_HEreg_a<- HI_cov$G.off.coeff

###################
#Adjusted zeros rep and sort
P0_zero_CGF.rep <-rep_data(P0_zero_CGF, 0, "zero")
P0.3_zero_CGF.rep <-rep_data(P0.3_zero_CGF, 0.3, "zero")
P0.6_zero_CGF.rep <-rep_data(P0.6_zero_CGF, 0.6, "zero")
P0.9_zero_CGF.rep <- rep_data(P0.9_zero_CGF, 0.9, "zero")

P0_zero_HI.rep <- rep_data(P0_zero_HI, 0, "zero")
P0.3_zero_HI.rep <- rep_data(P0.3_zero_HI, 0.3, "zero")
P0.6_zero_HI.rep <- rep_data(P0.6_zero_HI, 0.6, "zero")
P0.9_zero_HI.rep <- rep_data(P0.9_zero_HI, 0.9, "zero")


#Combine files  for zeros adjusted 
CGF_zero <- rbind(P0_zero_CGF.rep, P0.3_zero_CGF.rep, P0.6_zero_CGF.rep, P0.9_zero_CGF.rep)
HI_zero <- rbind(P0_zero_HI.rep, P0.3_zero_HI.rep, P0.6_zero_HI.rep, P0.9_zero_HI.rep)

CGF_zero <- CGF_zero[order(CGF_zero$t, CGF_zero$P)]
HI_zero <- HI_zero[order(HI_zero$t, HI_zero$P)]

HE_HI_zero$vg_zero_adj <- HI_zero$G.off.coeff
HE_CGF_zero$vg_zero_adj <- CGF_zero$G.off.coeff

#output files
# write.table(HI_zeros, "admix_HI_theta0.5_gen20_zeros.txt", sep = "\t", quote = F, row.names = F)
# write.table(CGF_zeros, "admix_CGF_theta0.5_gen20_zeros.txt", sep = "\t", quote = F, row.names = F)

#############
#gcta HEreg zeros not adjusted
load_data_gcta <- function(model, P, cov){
  data_file <- fread(file = paste0("admix_", model, "_theta0.5_gen20_P", P, "_", cov, ".HEreg.txt"), header = TRUE)
  return(data_file)
}

P0_zero_gcta_HI <- load_data_gcta("HI","0","zero")
P0.3_zero_gcta_HI <- load_data_gcta("HI","0.3","zero")
P0.6_zero_gcta_HI <- load_data_gcta("HI","0.6","zero")
P0.9_zero_gcta_HI <- load_data_gcta("HI","0.9","zero")

P0_zero_gcta_CGF<-load_data_gcta("CGF","0","zero")
P0.3_zero_gcta_CGF<-load_data_gcta("CGF","0.3","zero")
P0.6_zero_gcta_CGF<-load_data_gcta("CGF","0.6","zero")
P0.9_zero_gcta_CGF<- load_data_gcta("CGF","0.9","zero")

#rep over 10
rep_data_gcta <- function(file){
  file.avg <- colSums(matrix(file$vg_HEreg, nrow =10)/10)
  file.rep <- data.table(t=0:20, vg_HEreg=file.avg)
  return(file.rep)
}

#gcta zeros not adjusted
P0_zero_gcta_HI.rep <- rep_data_gcta(P0_zero_gcta_HI)
P0.3_zero_gcta_HI.rep <- rep_data_gcta(P0.3_zero_gcta_HI)
P0.6_zero_gcta_HI.rep <- rep_data_gcta(P0.6_zero_gcta_HI)
P0.9_zero_gcta_HI.rep <- rep_data_gcta(P0.9_zero_gcta_HI)

P0_zero_gcta_CGF.rep <-rep_data_gcta(P0_zero_gcta_CGF)
P0.3_zero_gcta_CGF.rep <-rep_data_gcta(P0.3_zero_gcta_CGF)
P0.6_zero_gcta_CGF.rep <-rep_data_gcta(P0.6_zero_gcta_CGF)
P0.9_zero_gcta_CGF.rep <- rep_data_gcta(P0.9_zero_gcta_CGF)

#save files
HI_zeros <- fread(file ="admix_HI_theta0.5_gen20_zeros.txt", header=T)
CGF_zeros <- fread(file ="admix_CGF_theta0.5_gen20_zeros.txt", header=T)

#Combine files 
CGF_zero_gcta <- rbind(P0_zero_gcta_CGF.rep, P0.3_zero_gcta_CGF.rep, 
                       P0.6_zero_gcta_CGF.rep, P0.9_zero_gcta_CGF.rep)
HI_zero_gcta <- rbind(P0_zero_gcta_HI.rep, P0.3_zero_gcta_HI.rep, 
                      P0.6_zero_gcta_HI.rep, P0.9_zero_gcta_HI.rep)

HI_zeros$vg_zeros_gcta <- HI_zero_gcta$vg_HEreg
CGF_zeros$vg_zeros_gcta <- CGF_zero_gcta$vg_HEreg

#output files
write.table(HI_zeros, "admix_HI_theta0.5_gen20_zeros.txt", sep = "\t", quote = F, row.names = F)
write.table(CGF_zeros, "admix_CGF_theta0.5_gen20_zeros.txt", sep = "\t", quote = F, row.names = F)

#############
#pos/neg cov unadjusted
P0_pos_noa_HI <- load_data_noa("HI","0","pos")
P0.3_pos_noa_HI <- load_data_noa("HI","0.3","pos")
P0.6_pos_noa_HI <- load_data_noa("HI","0.6","pos")
P0.9_pos_noa_HI <- load_data_noa("HI","0.9","pos")

P0_pos_noa_CGF<-load_data_noa("CGF","0","pos")
P0.3_pos_noa_CGF<-load_data_noa("CGF","0.3","pos")
P0.6_pos_noa_CGF<-load_data_noa("CGF","0.6","pos")
P0.9_pos_noa_CGF<- load_data_noa("CGF","0.9","pos")

P0_neg_noa_HI <- load_data_noa("HI","0","neg")
P0.3_neg_noa_HI <- load_data_noa("HI","0.3","neg")
P0.6_neg_noa_HI <- load_data_noa("HI","0.6","neg")
P0.9_neg_noa_HI <- load_data_noa("HI","0.9","neg")

P0_neg_noa_CGF<-load_data_noa("CGF","0","neg")
P0.3_neg_noa_CGF<-load_data_noa("CGF","0.3","neg")
P0.6_neg_noa_CGF<-load_data_noa("CGF","0.6","neg")
P0.9_neg_noa_CGF<- load_data_noa("CGF","0.9","neg")

#rep over 10
rep_data <- function(file, P, cov){
  file.sort <- file[order(file$t),]
  file.avg <- colSums(matrix(file.sort$G.off.coeff, nrow =10)/10)
  file.rep <- data.table(t=0:20, G.off.coeff=file.avg, P = P, cov = cov)
  return(file.rep)
}

#pos/neg not adjusted
P0_pos_noa_HI.rep <- rep_data(P0_pos_noa_HI, 0, "pos")
P0.3_pos_noa_HI.rep <- rep_data(P0.3_pos_noa_HI, 0.3, "pos")
P0.6_pos_noa_HI.rep <- rep_data(P0.6_pos_noa_HI, 0.6, "pos")
P0.9_pos_noa_HI.rep <- rep_data(P0.9_pos_noa_HI, 0.9, "pos")

P0_pos_noa_CGF.rep <-rep_data(P0_pos_noa_CGF, 0, "pos")
P0.3_pos_noa_CGF.rep <-rep_data(P0.3_pos_noa_CGF, 0.3, "pos")
P0.6_pos_noa_CGF.rep <-rep_data(P0.6_pos_noa_CGF, 0.6, "pos")
P0.9_pos_noa_CGF.rep <- rep_data(P0.9_pos_noa_CGF, 0.9, "pos")

P0_neg_noa_HI.rep <- rep_data(P0_neg_noa_HI, 0, "neg")
P0.3_neg_noa_HI.rep <- rep_data(P0.3_neg_noa_HI, 0.3, "neg")
P0.6_neg_noa_HI.rep <- rep_data(P0.6_neg_noa_HI, 0.6, "neg")
P0.9_neg_noa_HI.rep <- rep_data(P0.9_neg_noa_HI, 0.9, "neg")

P0_neg_noa_CGF.rep <-rep_data(P0_neg_noa_CGF, 0, "neg")
P0.3_neg_noa_CGF.rep <-rep_data(P0.3_neg_noa_CGF, 0.3, "neg")
P0.6_neg_noa_CGF.rep <-rep_data(P0.6_neg_noa_CGF, 0.6, "neg")
P0.9_neg_noa_CGF.rep <- rep_data(P0.9_neg_noa_CGF, 0.9, "neg")


#Combine files 
CGF_pos_noa <- rbind(P0_pos_noa_CGF.rep, P0.3_pos_noa_CGF.rep, 
                       P0.6_pos_noa_CGF.rep, P0.9_pos_noa_CGF.rep)
HI_pos_noa <- rbind(P0_pos_noa_HI.rep, P0.3_pos_noa_HI.rep, 
                      P0.6_pos_noa_HI.rep, P0.9_pos_noa_HI.rep)

CGF_neg_noa <- rbind(P0_neg_noa_CGF.rep, P0.3_neg_noa_CGF.rep, 
                     P0.6_neg_noa_CGF.rep, P0.9_neg_noa_CGF.rep)
HI_neg_noa <- rbind(P0_neg_noa_HI.rep, P0.3_neg_noa_HI.rep, 
                    P0.6_neg_noa_HI.rep, P0.9_neg_noa_HI.rep)

CGF_cov_noa <- rbind(CGF_pos_noa, CGF_neg_noa)
HI_cov_noa <- rbind(HI_pos_noa, HI_neg_noa)

CGF_cov_noa <- CGF_cov_noa[order(CGF_cov_noa$t, CGF_cov_noa$P, CGF_cov_noa$cov)]
HI_cov_noa <- HI_cov_noa[order(HI_cov_noa$t, HI_cov_noa$P, HI_cov_noa$cov)]

DickerHE_CGF$vg_HEreg_noa<- CGF_cov_noa$G.off.coeff
DickerHE_HI$vg_HEreg_noa<- HI_cov_noa$G.off.coeff

# #output files
# write.table(HI_zeros, "admix_HI_theta0.5_gen20_zeros.txt", sep = "\t", quote = F, row.names = F)
# write.table(CGF_zeros, "admix_CGF_theta0.5_gen20_zeros.txt", sep = "\t", quote = F, row.names = F)

#combining zero files
HEreg_CGF_zero_w <- fread("HEreg_CGF_zero_w.txt", header = T)
HEreg_HI_zero_w <- fread("HEreg_HI_zero_w.txt", header = T)

HEreg_CGF_zero <- fread("HEreg_CGF_zero.txt", header = T)
HEreg_HI_zero <- fread("HEreg_HI_zero.txt", header = T)

HEreg_CGF_zero_w[, vg_zero_adj := HEreg_CGF_zero$vg_zero_adj]
HEreg_HI_zero_w[, vg_zero_adj := HEreg_HI_zero$vg_zero_adj]

write.table(HEreg_HI_zero_w, "HE_HI_zero.txt", sep = "\t", quote = F, row.names = F)
write.table(HEreg_CGF_zero_w, "HE_CGF_zero.txt", sep = "\t", quote = F, row.names = F)

#-------------------------------------------------------------------------------
#view zeros 1.1-1.4 data
zero_CGF = fread("admix_CGF_theta0.5_gen20_zero_seed1-10.summary.txt", header = T)
zero_HI = fread("admix_HI_theta0.5_gen20_zero_seed1-10.summary.txt", header = T)

zero_CGF = zero_CGF[order(zero_CGF$t,zero_CGF$P),]
zero_HI = zero_HI[order(zero_HI$t,zero_HI$P),]

zero_CGF = subset(zero_CGF, select = -c(cov,model) )
zero_HI = subset(zero_HI, select = -c(cov,model) )

#rep over data
rep_data_zero <- function(file){
  file.avg <- apply(file, 2, function(file){
    colMeans(matrix(file, nrow =10))})
  return(as.data.table(file.avg))
}

zero_CGF_rep = rep_data_zero(zero_CGF)
zero_HI_rep = rep_data_zero(zero_HI)

#add back in cov
zero_CGF_rep[, cov := "zero"]
zero_HI_rep[, cov := "zero"]

#take out seed and theta
zero_CGF_rep = subset(zero_CGF_rep, select = -c(seed, theta))
zero_HI_rep = subset(zero_HI_rep, select = -c(seed, theta))

#output rep tables
write.table(zero_CGF_rep, "HEreg_CGF_zero_main.txt", sep = "\t", quote = F, row.names = F)
write.table(zero_HI_rep, "HEreg_HI_zero_main.txt", sep = "\t", quote = F, row.names = F)

#combine zero files
CGF_zero_main = fread("HEreg_CGF_zero_main.txt", header = T)
HI_zero_main = fread("HEreg_HI_zero_main.txt", header = T)

#merge main file with estimate file
CGF_zero_all = merge(HE_CGF_zero, CGF_zero_main, by= c("t","P","cov"))
HI_zero_all = merge(HE_HI_zero, HI_zero_main, by= c("t","P", "cov"))

#remove columns from greml
CGF_zero_all = subset(CGF_zero_all, select = -c(var.theta, var.prs.geno, cov.ganc.prsgeno, cor.ganc.prsgeno, 
                                                var.prs.lanc, cov.ganc.prslanc, cor.ganc.prslanc, cov.ganc.pheno, cor.ganc.pheno, 
                                                vbetween.genic, vbetween.obs, m1, prs.diff))
HI_zero_all = subset(HI_zero_all, select = -c(var.theta, var.prs.geno, cov.ganc.prsgeno, cor.ganc.prsgeno, 
                                              var.prs.lanc, cov.ganc.prslanc, cor.ganc.prslanc, cov.ganc.pheno, cor.ganc.pheno, 
                                              vbetween.genic, vbetween.obs, m1, prs.diff))

write.table(CGF_zero_all, "HE_CGF_zero_summary.txt", sep = "\t", quote = F, row.names = F)
write.table(HI_zero_all, "HE_HI_zero_summary.txt", sep = "\t", quote = F, row.names = F)

#-------------------------------------------------------------------------------
#Scaled GRMs
#load data
CGF_varX = fread("admix_CGF_theta0.5_gen20.summary_GRMvarX_cov.txt", header = T)
HI_varX = fread("admix_HI_theta0.5_gen20.summary_GRMvarX_cov.txt", header = T)

CGF_ld = fread("admix_CGF_theta0.5_gen20.summary_GRMld_cov.txt", header = T)
HI_ld = fread("admix_HI_theta0.5_gen20.summary_GRMld_cov.txt", header = T)

CGF_pheno = fread("CGF_var.pheno_cov.txt", header = T)
HI_pheno = fread("HI_var.pheno_cov.txt", header = T)

CGF_grm = merge(CGF_varX, CGF_ld, by= c("t","P","seed","cov"))
HI_grm = merge(HI_varX, HI_ld, by= c("t","P","seed","cov"))

CGF_grm.sort = CGF_grm[order(CGF_grm$t, CGF_grm$P, CGF_grm$cov),]
HI_grm.sort = HI_grm[order(HI_grm$t, HI_grm$P, HI_grm$cov),]

#sort pheno files
CGF_pheno = CGF_pheno[order(CGF_pheno$t, CGF_pheno$P, CGF_pheno$cov),]
HI_pheno = HI_pheno[order(HI_pheno$t, HI_pheno$P, HI_pheno$cov),]

#merge grm file with pheno file
CGF_grm_pheno = merge(CGF_grm.sort, CGF_pheno, by= c("t","P","seed","cov"))
HI_grm_pheno = merge(HI_grm.sort, HI_pheno, by= c("t","P","seed","cov"))

#calculate vg for gcta estimates since GCTA gives Vg/Vp
CGF_grm_pheno = as.data.table(CGF_grm_pheno)
HI_grm_pheno = as.data.table(HI_grm_pheno)

CGF_grm_pheno[, vg_GRMvarX_gcta := vg_GRMvarX*var.pheno]
CGF_grm_pheno[, vg_GRMld_gcta := vg_GRMld*var.pheno]

HI_grm_pheno[, vg_GRMvarX_gcta := vg_GRMvarX*var.pheno]
HI_grm_pheno[, vg_GRMld_gcta := vg_GRMld*var.pheno]


#output scaled grm tables
write.table(CGF_grm_pheno, "admix_CGF_theta0.5_gen20_cov_grms.txt", sep = "\t", quote = F, row.names = F)
write.table(HI_grm_pheno, "admix_HI_theta0.5_gen20_cov_grms.txt", sep = "\t", quote = F, row.names = F)

