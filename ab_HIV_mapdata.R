# Bijoy Joseph
# 2018.08.20
# Download India map data from http://gadm.org/, read in NFHS data and plot them
# Code examples taken from http://visual.yantrajaal.com/2015/05/using-r-for-maps-of-india-state.html
#+ and http://www.dpi.inpe.br/gilberto/tutorials/software/R-contrib/sp/html/spplot.html
# OUTPUTs:
#+  1) District-level map plots of data

library(sp)
library(RColorBrewer)

'%ni%' <- Negate('%in%')

## Read map of India, state-level
ind1 = readRDS("data/gadm36_IND_1_sp.rds")

# ind1$NAME_1 = as.factor(ind1$NAME_1)

## Read HIV data
paf_un = read.csv2("data/NFHS District level_HIV-PAF-statelevel.csv", stringsAsFactors=FALSE)

# Convert all data columns to numeric
paf_un[,3:ncol(paf_un)] = sapply(paf_un[,3:ncol(paf_un)], as.numeric)

## Corrections in PAF state/district names
# Replace ampersand in state names to 'and', e.g. Daman & Diu
paf_un$State[grep('&', paf_un$State)] = gsub('&', 'and', paf_un$State[grep('&', paf_un$State)])
paf_un$State[(paf_un$State=="Telanagana")] <- "Telangana"


## plot for districts in each State listed in PAF data
# Merge data with map
state_data <- merge(ind1, paf_un, by.x="NAME_1", by.y="State")

# colour palette
col = topo.colors(40)

# Three columns
png("results/HIVpct-preval-antenatal-women.png", width = 880, height = 680)
spplot(state_data, c("pctHIV.prevalence.in.antenatal.women"), main="Prevalence (%) of HIV in antenatal women", col.regions = col, par.settings=list(fontsize=list(text=14)))
dev.off()

png("results/HIV-prevalence.png", width = 880, height = 680)
spplot(state_data, c("HIV.prevalence"), main="HIV prevalence", col.regions=col, par.settings=list(fontsize=list(text=14)))
dev.off()

png("results/HIV-PAF.png", width = 880, height = 680)
spplot(state_data, c("PAF.related.to.HIV"), main="PAF for HIV", col.regions=col, par.settings=list(fontsize=list(text=14)))
dev.off()

stop("End of script")



## Scratch()

# Add custom colours
my.palette <- brewer.pal(n = 7, name = "Oranges")
png(paste0("results/",gsub(' ','',i),"-PAF-women.png"), width = 880, height = 680)
print(spplot(state_data, c("PAF.UN.women.urban.","PAF.UN.women.rural.", "PAF.UN.women.total."), key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i, " districts: PAF for women"), col.regions = my.palette, layout = c(3, 1), as.table=TRUE))
dev.off()


# find intervals using cut()
png("results/HIVpct-preval-antenatal-women.png", width = 880, height = 680)
spplot(state_data, c("pctHIV.prevalence.in.antenatal.women"), main="Prevalence (%) of HIV in antenatal women", col.regions = col, par.settings=list(fontsize=list(text=20)), colorkey = list(at = cut(as.numeric(as.character(state_data$pctHIV.prevalence.in.antenatal.women)), length(levels(state_data$pctHIV.prevalence.in.antenatal.women)), labels=FALSE)))
dev.off()

spplot(state_data, c("pctHIV.prevalence.in.antenatal.women"), main="Prevalence (%) of HIV in antenatal women", col.regions = col, par.settings=list(fontsize=list(text=20)), colorkey = list(at = quantile(sort(x1))))

# map1 <- ggplot(state_data) + geom_polygon(aes(x = long, y = lat, group = group), fill = "white", colour = "black")
# map1 + geom_point(data = state_data) + scale_colour_manual(values = rainbow(14)) + labs(x = "Longitude", y = "Latitude", title = "Map of Karnataka districts with PAF data")


## ## Data summary options, by package
# base::by(data, data$category, summary)
# 
# library(pastecs)
# stat.desc(data)
# 
# library(Hmisc)
# Hmisc::describe(data)
# 
# library(psych)
# psych::describe(data)
# psych::describeBy(data, data$type)
# 
# library(skimr)
# skim(data)
# group_by(data, category) %>% skim()
# 
# library(summarytools)
# summarytools::descr(data)
# summarytools::dfSummary(data)

