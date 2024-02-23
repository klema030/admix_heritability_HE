# plot vg results from 3 estimators (alternative to GCTA-GREML analysis): 
# Dicker1, Dicker2 (Dicker 2014), and Haseman-Elston regression analysis 

# load summary stat, average of 10 reps 
library(data.table)

DickerHE_HI=fread("HEreg_HI.txt", header=T)
DickerHE_CGF=fread("HEreg_CGF.txt", header=T)

HE_HI_zero=fread("HE_HI_zero_summary.txt", header=T)
HE_CGF_zero=fread("HE_CGF_zero_summary.txt", header=T)

# A function factory for getting integer y-axis values.
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}


# plot vg estimate and expected 
fig4A=function(data, yobs, yexp, ylab, title, legend.position="right"){
  library(ggplot2)
  ggplot() +
    geom_line(data=data, linewidth=0.9, alpha = 0.65, #transparent this line
              aes(x=t, y = yobs,                               
                  linetype = "obs",
                  group=interaction(P, cov), 
                  color=interaction(P, cov))) +
    geom_line(data=data, linewidth=0.9, 
              aes(x=t, y = yexp, 
                  linetype = "exp",
                  group=interaction(P, cov),
                  color=interaction(P, cov))) +
    # geom_line(data=HE_CGF_zero, linewidth=0.9,
    #           aes(x=t, y=vg_zeros_noa,
    #               linetype = "obs",
    #               group=interaction(P, cov),
    #               color=interaction(P, cov)))+
    #scale_y_log10(limits=c(0.9, 2.1)) +
    scale_linetype_manual("", 
                          breaks = c("obs",   "exp"),
                          values = c("solid",  "11"),
                          labels = c("Estimated",
                                     "Expected")) +
    scale_colour_manual("", 
                        values = c('#92c5de','#4393c3','#2166ac','#053061',
                                   '#f4a582','#d6604d','#b2182b','#67001f'),
                                   # '#65ef57','#3baf30','#32712c','#193816'),
                        labels = c("P=0", "P=0.3", "P=0.6", "P=0.9",
                                   "P=0", "P=0.3", "P=0.6", "P=0.9"
                                   # "P=0", "P=0.3", "P=0.6", "P=0.9"
                        )) +
    theme_classic() +
    xlab("t") +
    ylab(ylab)  + 
    ggtitle(title) +
    theme(aspect.ratio = 1, 
          plot.title = element_text(hjust = 0.5), # to center the title
          legend.position = legend.position, 
          legend.text.align = 0, #left align legend
          text = element_text(size = 12),
          plot.margin = unit(c(0, 0, 0, 0), 'cm')
    ) + 
    guides(color = "none", # no show color legend
           linetype = guide_legend(order = 2, reverse = T)
    )
}

#color legend only
fig4color=function(data){
  library(ggplot2)
  ggplot() +
    geom_line(data=data, linewidth=0.9, alpha = 0.65, #transparent this line
              aes(x=t, y = tau2_dicker2,                               
                  linetype = "obs",
                  group=interaction(P, cov), 
                  color=interaction(P, cov))) +
    geom_line(data=data, linewidth=0.9, 
              aes(x=t, y = exp.vg, 
                  linetype = "exp",
                  group=interaction(P, cov),
                  color=interaction(P, cov))) +
    #scale_y_log10(limits=c(0.9, 2.1)) +
    scale_linetype_manual("", 
                          breaks = c("obs",   "exp"),
                          values = c("solid",  "11"),
                          labels = c("Estimated\n(standard)",
                                     "Expected")) +
    scale_colour_manual("", 
                        values = c('#92c5de','#4393c3','#2166ac','#053061',
                                   '#f4a582','#d6604d','#b2182b','#67001f'),
                                   # '#65ef57','#3baf30','#32712c','#193816'),
                        labels = c("P=0", "P=0.3", "P=0.6", "P=0.9",
                                   "P=0", "P=0.3", "P=0.6", "P=0.9"
                                   # "P=0", "P=0.3", "P=0.6", "P=0.9"
                        )) +
    theme_classic() +
    xlab("t") +
    ylab(expression(hat(V)[g]))  + 
    theme(aspect.ratio = 1, 
          legend.position = "bottom", #hide legend for HI
          text = element_text(size = 12)
    ) + 
    guides(color =  guide_legend(order = 1, nrow=2, 
                                 byrow = T, reverse = T,
                                 override.aes = list(linewidth = 2)),  # thicken the line in legend
           linetype = "none" 
    )}
HI_color=fig4color(DickerHE_HI)



# tau2 from Dicker estimator is Vg
HIa_wo_1=fig4A(data=DickerHE_HI, 
               yobs = DickerHE_HI$vg_HEreg_cov, 
               yexp = DickerHE_HI$vg_123,
               ylab = expression(hat(V)[g]),
               title="Hybrid Isolation - Dicker1", 
               legend.position = "none") 

HIa_wo_2=fig4A(data=DickerHE_HI, 
               yobs = DickerHE_HI$tau2_dicker2, 
               yexp = DickerHE_HI$VG_123,
               ylab = expression(hat(V)[g]),
               title="Hybrid Isolation - Dicker2", 
               legend.position = "none") 

# the HEreg RESULT vg_HEreg is VG/VP
# vg_HEreg_new is the result of vg_HEreg * var.pheno, ie VG
HIa_HE=fig4A(data=DickerHE_HI, 
             yobs = DickerHE_HI$vg_HEreg_noa, 
             yexp = DickerHE_HI$var.prs.geno,
             ylab = expression(hat(V)[g]),
             title="Hybrid Isolation - HE",
             legend.position = "none") 

HIa_gcta=fig4A(data=DickerHE_HI, 
                yobs = DickerHE_HI$vg_HEreg_new, 
                yexp = DickerHE_HI$var.prs.geno,
                ylab = expression(hat(V)[g]),
                title="Hybrid Isolation - HE GCTA",
               legend.position = "none") 

CGFa_gcta=fig4A(data=DickerHE_CGF, 
                yobs = DickerHE_CGF$vg_HEreg_new, 
                yexp = DickerHE_CGF$var.prs.geno,
                ylab = expression(hat(V)[g]),
                title="Continuous Gene Flow - HE GCTA") 

CGFa_HE=fig4A(data=DickerHE_CGF, 
              yobs = DickerHE_CGF$vg_HEreg_noa, 
              yexp = DickerHE_CGF$var.prs.geno,
              ylab = expression(hat(V)[g]),
              title="Continuous Gene Flow - HE") 


# plot vg estimate and expected 
fig4Azero=function(data, yobs, yexp, ylab, title, legend.position="right"){
  library(ggplot2)
  ggplot() +
    geom_line(data=data, linewidth=0.9, alpha = 0.65, #transparent this line
              aes(x=t, y = yobs,                               
                  linetype = "obs",
                  group=interaction(P, cov), 
                  color=interaction(P, cov))) +
    geom_line(data=data, linewidth=0.9, 
              aes(x=t, y = yexp, 
                  linetype = "exp",
                  group=interaction(P, cov),
                  color=interaction(P, cov))) +
    #scale_y_log10(limits=c(0.9, 2.1)) +
    scale_linetype_manual("", 
                          breaks = c("obs",   "exp"),
                          values = c("solid",  "11"),
                          labels = c("Estimated",
                                     "Expected")) +
    scale_colour_manual("", 
                        values = c('#65ef57','#3baf30','#32712c','#193816'),
                        labels = c("P=0", "P=0.3", "P=0.6", "P=0.9")) +
    theme_classic() +
    xlab("t") +
    ylab(ylab)  + 
    ggtitle(title) +
    theme(aspect.ratio = 1, 
          plot.title = element_text(hjust = 0.5), # to center the title
          legend.position = legend.position, 
          legend.text.align = 0, #left align legend
          text = element_text(size = 12),
          plot.margin = unit(c(0, 0, 0, 0), 'cm')
    ) + 
    guides(color = "none", # no show color legend
           linetype = guide_legend(order = 2, reverse = T)
    )
}

#color legend only
fig4colorzero=function(data){
  library(ggplot2)
  ggplot() +
    geom_line(data=data, linewidth=0.9, alpha = 0.65, #transparent this line
              aes(x=t, y = vg_zero_adj,                               
                  linetype = "obs",
                  group=interaction(P, cov), 
                  color=interaction(P, cov))) +
    geom_line(data=data, linewidth=0.9, 
              aes(x=t, y = vg_123, 
                  linetype = "exp",
                  group=interaction(P, cov),
                  color=interaction(P, cov))) +
    #scale_y_log10(limits=c(0.9, 2.1)) +
    scale_linetype_manual("", 
                          breaks = c("obs",   "exp"),
                          values = c("solid",  "11"),
                          labels = c("Estimated\n(standard)",
                                     "Expected")) +
    scale_colour_manual("", 
                        values = c('#65ef57','#3baf30','#32712c','#193816'),
                        labels = c("P=0", "P=0.3", "P=0.6", "P=0.9")) +
    theme_classic() +
    xlab("t") +
    ylab(expression(hat(V)[g]))  + 
    theme(aspect.ratio = 1, 
          legend.position = "bottom", #hide legend for HI
          text = element_text(size = 12)
    ) + 
    guides(color =  guide_legend(order = 1, nrow=1, 
                                 byrow = T, reverse = T,
                                 override.aes = list(linewidth = 2)),  # thicken the line in legend
           linetype = "none" 
    )}
HI_color=fig4colorzero(HE_HI_zero)

#Zeros adjusted
HI_HE_zero=fig4Azero(data=HE_HI_zero,
                 yobs = HE_HI_zero$vg_zero_adj,
                 yexp = HE_HI_zero$vg_123,
                 ylab = expression(hat(V)[g]),
                 title="Hybrid Isolation - HE Adjusted",
                 legend.position = "none")
print(HI_HE_zero)

CGF_HE_zero=fig4Azero(data=HE_CGF_zero,
                 yobs = HE_CGF_zero$vg_zero_adj,
                 yexp = HE_CGF_zero$vg_123,
                 ylab = expression(hat(V)[g]),
                 title="Continuous Gene Flow - HE Adjusted")

#zeros unadjusted
HI_HE_zero_noa=fig4Azero(data=HE_HI_zero,
                 yobs = HE_HI_zero$vg_zeros_noa,
                 yexp = HE_HI_zero$vg_123,
                 ylab = expression(hat(V)[g]),
                 title="Hybrid Isolation - HE",
                 legend.position = "none")

CGF_HE_zero_noa=fig4Azero(data=HE_CGF_zero,
                  yobs = HE_CGF_zero$vg_zeros_noa,
                  yexp = HE_CGF_zero$vg_123,
                  ylab = expression(hat(V)[g]),
                  title="Continuous Gene Flow - HE")

#zeros unadjusted from gcta
HI_HE_zero_gcta=fig4Azero(data=HE_HI_zero,
                     yobs = HE_HI_zero$vg_HEreg_new,
                     yexp = HE_HI_zero$vg_123,
                     ylab = expression(hat(V)[g]),
                     title="Hybrid Isolation - HE GCTA",
                     legend.position = "none")

CGF_HE_zero_gcta=fig4Azero(data=HE_CGF_zero,
                      yobs = HE_CGF_zero$vg_HEreg_new,
                      yexp = HE_CGF_zero$vg_123,
                      ylab = expression(hat(V)[g]),
                      title="Continuous Gene Flow - HE GCTA")

#HE cov adjusted
HI_HE_adj=fig4A(data=DickerHE_HI, 
             yobs = DickerHE_HI$vg_HEreg_a, 
             yexp = DickerHE_HI$var.prs.geno,
             ylab = expression(hat(V)[g]),
             title="Hybrid Isolation - HE adjusted",
             legend.position = "none") 

CGF_HE_adj=fig4A(data=DickerHE_CGF, 
                yobs = DickerHE_CGF$vg_HEreg_a, 
                yexp = DickerHE_CGF$var.prs.geno,
                ylab = expression(hat(V)[g]),
                title="Continous Gene Flow - HE adjusted") 

library(ggpubr)
#zeros
plt_zero=ggarrange(HI_HE_zero_noa, CGF_HE_zero_noa,
                   HI_HE_zero_gcta, CGF_HE_zero_gcta,
                   HI_HE_zero, CGF_HE_zero,
              ncol = 2, nrow = 3, 
              labels = c("A", "", "B", "", "C", ""),
              align = c("h")) %>% # to move the legend closer 
  gridExtra::grid.arrange(get_legend(HI_color), 
                          #heights = unit(c(160, 10), "mm")
                          heights = unit(c(8, 0.8), "in")
  )   
ggsave("HE_vg_adj_zero.png", plot=plt_zero,
       width = 9, height = 9, dpi = 300, units = "in", device='png')

#pos/neg
plt_cov=ggarrange(HIa_HE, CGFa_HE,
                  HIa_gcta, CGFa_gcta,
                  HI_HE_adj, CGF_HE_adj,
                  ncol = 2, nrow = 3, 
                  labels = c("A", "", "B", "", "C", ""),
                  align = c("h")) %>% # to move the legend closer 
  gridExtra::grid.arrange(get_legend(HI_color), 
                          #heights = unit(c(160, 10), "mm")
                          heights = unit(c(8, 0.8), "in")
  )   
ggsave("HE_vg_adjusted.png", plot=plt_cov,
       width = 9, height = 9, dpi = 300, units = "in", device='png')

#output files
write.table(DickerHE_HI, "HEreg_HI.txt", sep = "\t", quote = F, row.names = F)
write.table(DickerHE_CGF, "HEreg_CGF.txt", sep = "\t", quote = F, row.names = F)

write.table(HI_var_pheno, "HEreg_HI_zero_w.txt", sep = "\t", quote = F, row.names = F)
write.table(CGF_var_pheno, "HEreg_CGF_zero_w.txt", sep = "\t", quote = F, row.names = F)
