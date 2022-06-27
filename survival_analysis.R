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

out.dir<-format(Sys.Date(), "%d-%B-%y")

#check if directory exists - if not create daily directory 
if(!dir.exists(paste0(here("out/Survival-data/"), out.dir))) {dir.create(paste0(here("out/Survival-data/"), out.dir))}


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

data<-data %>% 
  mutate(type = case_when(prior ==10 ~ "treatment-naïve",
                          TRUE ~ "treatment-experienced"),
         month = round(time/30.417, digit=0))

surv_obj <- survival::survfit(survival::Surv(month, status) ~ trt, data = data)

summary(surv_obj)

p<-ggsurvplot(
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

plot<-p$plot

table<-p$table

final.plot<-ggarrange(plot, table,
                      nrow = 2,
                      heights = c(1, 0.3, 0.5), align = "v")


ggsave(filename = (paste0(here("out/Survival-data/"), out.dir, "/surv-plot-", out.dir, ".png")), 
       width = 40, height = 20, dpi = 320, units = "cm")


