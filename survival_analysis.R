##----------------------------------------------------------------------------------------##
##Author:   Emily McGovern
##Date:     June 2022
##Task:     Statistical programming task for recruitment process
##Data Source: survival package R (https://cran.r-project.org/web/packages/survival/index.html)
##--------------------------------------------------------------------------------------------##
#Data setup
require(pacman)
pacman::p_load(tidyverse,
               janitor,
               survival,
               lubridate,
               survminer,
               here)

data <- survival::veteran

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

data<-data %>% 
  mutate(type = case_when(prior ==10 ~ "treatment-naïve",
                          TRUE ~ "treatment-experienced"))

surv_obj <- survival::survfit(survival::Surv(time, status) ~ type, data = data)

ggsurvplot(
  fit = surv_obj,
  data = data,
  conf.int = TRUE, # plot the confidence interval of the survival probability
  risk.table = TRUE, # draw the risk table below the graph
  pval  = TRUE,
  surv.median.line = "hv",# draw the survival median line horizontally & vertically
  xlab = "Survival time (days)", 
  ylab = "Overall survival probability",
  break.x.by = 100,
  break.y.by = 0.2,
  palette = c("#E7B800",
              "#2E9FDF")
)


survdiff(Surv(time, status)~type, data=veteran)
