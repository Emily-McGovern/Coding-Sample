##--------------------------------------------------------------------------------------------##
## Author:   Emily McGovern
## Date:     June 2022
## Task:     Statistical programming task for recruitment process - survival analysis
## Data Source: survival package R (https://cran.r-project.org/web/packages/survival/index.html)
##--------------------------------------------------------------------------------------------##
#Data setup
require(pacman)
pacman::p_load(tidyverse,
               ggpubr,
               survival,
               survminer,
               here)

#output directory 
dir.name<-base::format(Sys.Date(), "%d-%B-%y")
out.dir<-paste0(here("out/Survival-data/"),dir.name)

#check if directory exists - if not create daily directory 
if(!base::dir.exists(out.dir)) {base::dir.create(out.dir)}

# use survival vetaran data 
data <- survival::veteran

## Dataset Details
#trt:	1=standard 2=test
#celltype:	1=squamous, 2=smallcell, 3=adeno, 4=large
#time:	survival time
#status:	censoring status
#karno:	Karnofsky performance score (100=good)
#diagtime:	months from diagnosis to randomisation
#age:	in years
#prior:	prior therapy 0=no, 10=yes

#explore data
dplyr::glimpse(data)

#data manipulation
data<-data %>% 
  dplyr::mutate(type = case_when(prior ==10 ~ "treatment-naïve",
                                 TRUE ~ "treatment-experienced"),
                month = round(time/30.417, digit=0))

#Computes an estimate of a survival curve for censored data using the Kaplan-Meier method
surv_obj <- survival::survfit(survival::Surv(month, status) ~ trt, data = data)

base::summary(surv_obj)

##plot survival curves
p<-survminer::ggsurvplot(
  fit = surv_obj,
  data = data,
  conf.int = TRUE, # plot the confidence interval of the survival probability
  risk.table = TRUE, # draw the risk table below the graph
  pval  = TRUE,
  surv.median.line = "hv", # draw the survival median line horizontally & vertically
  title = "Survial rate of treatment naïve v treatment experience ",
  xlab = "Survival time (months)", 
  ylab = "Survival probability",
  legend.title = "",
  risk.table.title ="",
  break.x.by = 4,
  break.y.by = 0.2,
  palette = c("#E7B800",
              "#2E9FDF")
)

#extract plot from ggsurvplot object
plot<-p$plot

#extract risk table from ggsurvplot object
table<-p$table

#arrange plot and risk table into one plot
final.plot<-ggpubr::ggarrange(plot, table,
                               nrow = 2,
                               heights = c(1, 0.3, 0.5), align = "v")

#save plot
ggplot2::ggsave(filename = paste0(out.dir, "/surv-plot-", dir.name, ".png"), 
                width = 40, height = 20, dpi = 320, units = "cm")


