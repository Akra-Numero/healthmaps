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

## Read map of India, District-level
ind2 = readRDS("data/gadm36_IND_2_sp.rds")

## Read NFHS data, PAF for undernutrition
paf_un = read.csv2("data/NFHSundernutritionPAF.csv", stringsAsFactors=FALSE)

# Convert all data columns to numeric
paf_un[,3:ncol(paf_un)] = sapply(paf_un[,3:ncol(paf_un)], as.numeric)

## Function to check names of districts; do check after corrections below
check_districts <- function() {
  for(i in unique(paf_un$State)) {
    if(i=="Delhi") next
    state2 = (ind2[ind2$NAME_1 == i,])
    pafun_i = paf_un[(paf_un$State == i),]

#   x = setdiff(state2$NAME_2, pafun_i$District)
    x = pafun_i$District[pafun_i$District %ni% state2$NAME_2]
    if(length(x) > 0) {
      print(paste0(i, " =================== ",i))
      print(sort(x))
    }
  }
}

# Call function to check districts
check_districts()

## Corrections in PAF state/district names
# Replace ampersand in state names to 'and', e.g. Daman & Diu
paf_un$State[grep('&', paf_un$State)] = gsub('&', 'and', paf_un$State[grep('&', paf_un$State)])
paf_un$State[(paf_un$State=="Telanagana")] <- "Telangana"

# ind2$NAME_2[ind2$NAME_1 == "Gujarat"]
#GJ
paf_un$District[(paf_un$District=="Dohad")] <- "Dahod"
ind2$NAME_2[(ind2$NAME_2 == "Banas Kantha")] <- "Banaskantha"
ind2$NAME_2[(ind2$NAME_2 == "Mahesana")] <- "Mehsana"
ind2$NAME_2[(ind2$NAME_2 == "Panch Mahals")] <- "Panchmahal"
ind2$NAME_2[(ind2$NAME_2 == "Sabar Kantha")] <- "Sabarkantha"

#KA
paf_un$District[(paf_un$District=="Chikkaballapura")] <- "Chikballapura"
paf_un$District[(paf_un$District=="Dakshina kannada")] <- "Dakshina Kannada"
paf_un$District[(paf_un$District=="Chamrajanagar")] <- "Chamrajnagar"

#UP
paf_un$District[(paf_un$District=="Ambedkar")] <- "Ambedkar Nagar"
paf_un$District[(paf_un$District=="Jyotiba Phule Nagar")] <- "Amroha"
paf_un$District[(paf_un$District=="Kanshiram Nagar")] <- "Kasganj"
paf_un$District[(paf_un$District=="Bara Banki")] <- "Barabanki"
paf_un$District[(paf_un$District=="Kheri")] <- "Lakhimpur Kheri"
paf_un$District[(paf_un$District=="Mahamaya Nagar")] <- "Hathras"
paf_un$District[(paf_un$District=="Shrawasti")] <- "Shravasti"
ind2$NAME_2[(ind2$NAME_2 == "Sant Ravi Das Nagar")] <- "Sant Ravidas Nagar"
ind2$NAME_2[(ind2$NAME_2 == "Siddharth Nagar")] <- "Siddharthnagar"

#MP
paf_un$District[(paf_un$District=="Khandwa(East Nimar)")] <- "East Nimar"
paf_un$District[(paf_un$District=="Khargone(West Nimar)")] <- "West Nimar"

#BH
paf_un$District[(paf_un$District=="Buxer")] <- "Buxar"

#MH
#"Mumbai" in data is  "Mumbai City" and "Mumbai Suburban" in map
ind2$NAME_2[(ind2$NAME_2 == "Ahmadnagar")] <- "Ahmednagar"
ind2$NAME_2[(ind2$NAME_2 == "Buldana")] <- "Buldhana"
ind2$NAME_2[(ind2$NAME_2 == "Garhchiroli")] <- "Gadchiroli"

#AS
paf_un$District[(paf_un$District=="Kamrup ")] <- "Kamrup"

#CG
paf_un$District[(paf_un$District=="Dakshin Bastar Dantewada")] <- "Dantewada"
paf_un$District[(paf_un$District=="Janjgir Champa")] <- "Janjgir-Champa"
paf_un$District[(paf_un$District=="Korea")] <- "Koriya"
paf_un$District[(paf_un$District=="Utter Bastar Kanker")] <- "Kanker"
ind2$NAME_2[(ind2$NAME_2 == "Uttar Bastar Kanker")] <- "Kanker"
ind2$NAME_2[(ind2$NAME_2 == "Kabeerdham")] <- "Kabirdham"

#JH
ind2$NAME_2[(ind2$NAME_2 == "Saraikela-kharsawan")] <- "Saraikela Kharsawan"

#WB
paf_un$District[(paf_un$District=="North Twenty Four Parganas")] <- "North 24 Parganas"
paf_un$District[(paf_un$District=="Paschin Medinipur")] <- "Pashchim Medinipur"
paf_un$District[(paf_un$District=="South Twenty Four Parganas")] <- "South 24 Parganas"

#OR
ind2$NAME_2[(ind2$NAME_2 == "Bauda")] <- "Baudh"

#TG
paf_un$District[(paf_un$District=="Rangareddy")] <- "Ranga Reddy"

#HP
ind2$NAME_2[(ind2$NAME_2 == "Lahul & Spiti")] <- "Lahul and Spiti"

#TN
paf_un$District[(paf_un$District=="Coddalore")] <- "Cuddalore"
paf_un$District[(paf_un$District=="Erobe")] <- "Erode"
paf_un$District[(paf_un$District=="Ramananthapuram")] <- "Ramanathapuram"
ind2$NAME_2[(ind2$NAME_2 == "Nagappattinam")] <- "Nagapattinam"
ind2$NAME_2[(ind2$NAME_2 == "Virudunagar")] <- "Virudhunagar"

#JK
paf_un$District[(paf_un$District=="Baramula")] <- "Baramulla"
paf_un$District[(paf_un$District=="Leh(Ladakh)")] <- "Leh (Ladakh)"
paf_un$District[(paf_un$District=="Punch")] <- "Poonch"

#AP
paf_un$District[(paf_un$District=="Sri Potti Sriramulu Nellore")] <- "Nellore"

#PB
paf_un$District[(paf_un$District=="Fatehgarh")] <- "Fatehgarh Sahib"
paf_un$District[(paf_un$District=="Kopurthala")] <- "Kapurthala"
paf_un$District[(paf_un$District=="Rup Nagar")] <- "Rupnagar"
paf_un$District[(paf_un$District=="Sahid Bhagat Singh Nagar")] <- "Shahid Bhagat Singh Nagar"

#AR
paf_un$District[(paf_un$District=="Papumpare")] <- "Papum Pare"

#ML
paf_un$District[(paf_un$District=="Ribhoi")] <- "Ri Bhoi"

#MN
paf_un$District[(paf_un$District=="Toubal")] <- "Thoubal"

#MZ
ind2$NAME_2[(ind2$NAME_2 == "Lawangtlai")] <- "Lawngtlai"

#AN
paf_un$District[(paf_un$District=="North & Middle Andaman")] <- "North and Middle Andaman"
ind2$NAME_2[(ind2$NAME_2 == "Nicobar Islands")] <- "Nicobar"

#SK
paf_un$District[(paf_un$District=="East District")] <- "East Sikkim"
paf_un$District[(paf_un$District=="North District")] <- "North Sikkim"
paf_un$District[(paf_un$District=="South District")] <- "South Sikkim"
paf_un$District[(paf_un$District=="West District")] <- "West Sikkim"

# Call function to check districts again; only Mumbai remains
check_districts()

# Colour palette
col = topo.colors(40)

## plot for districts in each State listed in PAF data
for(i in sort(unique(paf_un$State))) {
  print(i)
  if(i=="Delhi") next
  state2 = (ind2[ind2$NAME_1 == i,])
  pafun_i = paf_un[(paf_un$State == i),]

# Merge data with map
state_data <- merge(state2, pafun_i, by.x="NAME_2", by.y="District")
# col = terrain.colors(length(levels(as.factor(state_data$PAF.UN.women.total.))))

png(paste0("results/",gsub(' ','',i),"-PAF-UN-women.png"), width = 880, height = 680)
print(spplot(state_data, c("PAF.UN.women.urban.","PAF.UN.women.rural.", "PAF.UN.women.total."), key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i, " districts: PAF for undernutrition - women"), names.attr=c("Urban","Rural","Total"), col.regions = col, colorkey= list(space = "bottom"), layout = c(3, 1), as.table=TRUE, par.settings=list(fontsize=list(text=13))))
dev.off()

png(paste0("results/",gsub(' ','',i),"-PAF-UN-men.png"), width = 880, height = 680)
print(spplot(state_data, c("PAF.UN.men.urban.","PAF.UN.men..rural.", "PAF.UN.men..total."), key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i, " districts: PAF for undernutrition - men"), names.attr=c("Urban","Rural","Total"), col.regions = col, colorkey= list(space = "bottom"), layout = c(3, 1), as.table=TRUE, par.settings=list(fontsize=list(text=13))))
dev.off()

png(paste0("results/",gsub(' ','',i),"-BMI-lt-18.5-women.png"), width = 880, height = 680)
print(spplot(state_data, c("BMI.18.5.in.women.urban.", "BMI.18.5.in.women.rural.","BMI.18.5...total."), key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i," districts: BMI < 18.5 for women"), names.attr=c("Urban","Rural","Total"), col.regions = col, colorkey= list(space = "bottom"), layout = c(3, 1), as.table=TRUE, par.settings=list(fontsize=list(text=13))))
dev.off()

png(paste0("results/",gsub(' ','',i),"-BMI-lt-18.5-men.png"), width = 880, height = 680)
print(spplot(state_data, c("BMI..18.5.in.men.urban.", "BMI..18.5.in.men.rural.", "BMI..18.5.in.men.total."), key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i," districts: BMI < 18.5 for men"), names.attr=c("Urban","Rural","Total"), col.regions = col, colorkey= list(space = "bottom"), layout = c(3, 1), as.table=TRUE, par.settings=list(fontsize=list(text=13))))
dev.off()
}

quit(save="ask")
stop("End of script")


## Scratch()

## Read map of India, state-level
ind1 = readRDS("Downloads/gadm36_IND_1_sp.rds")

# simple map of India with states drawn
spplot(ind1, "NAME_1", scales=list(draw=T), colorkey=F, main="India")

# map of India with states coloured with an arbitrary fake data
ind1$NAME_1 = as.factor(ind1$NAME_1)
ind1$fake.data = runif(length(ind1$NAME_1))
spplot(ind1,"NAME_1",  col.regions=rgb(0,ind1$fake.data,0), colorkey=TRUE, main="Indian States")


# map of Karnataka ( or any other state )
karnat1 = (ind1[ind1$NAME_1=="Karnataka",])
spplot(karnat1,"NAME_1", col.regions=rgb(0,0,1), main = "Karnataka, India",scales=list(draw=TRUE), colorkey=FALSE)

# spplot(state_data,"NAME_2", col.regions=rgb(0,as.numeric(as.character(state_data$PAF.UN.women.total.)),0), colorkey=TRUE, main="UN PAF for Women, by district", checkEmptyRC=FALSE)
# spplot(state_data,"NAME_2", col.regions=rgb(0,col,0), colorkey=TRUE, main="UN PAF for Women, by district", checkEmptyRC=TRUE)

# Old version of plot, does not work for Chhattisgarh
# sp.layout
l2 = list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(181300,429800), scale = 400)
l3 = list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(180500,429800), scale = 500, fill=c("transparent","black"))
l4 = list("sp.text", c(180500,329900), "0")
l5 = list("sp.text", c(181000,329900), "500 m")

png(paste0("results/",gsub(' ','',i),"-PAF-women.png"))
spplot(state_data, c("PAF.UN.women.urban.","PAF.UN.women.rural.", "PAF.UN.women.total."), sp.layout=list(l2,l3,l4,l5, which = 2),key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i, " districts: PAF for women"))
dev.off()

# version of plot that works for Chhattisgarh
png(paste0("results/",gsub(' ','',i),"-PAF-women---new.png"), width = 880, height = 680)
spplot(state_data, c("PAF.UN.women.urban.","PAF.UN.women.rural.", "PAF.UN.women.total."), layout = c(3, 1), aspect=1, main=paste0(i, " districts: PAF for women"), as.table=TRUE)
dev.off()

# Add custom colours
my.palette <- brewer.pal(n = 7, name = "Oranges")
png(paste0("results/",gsub(' ','',i),"-PAF-women.png"), width = 880, height = 680)
print(spplot(state_data, c("PAF.UN.women.urban.","PAF.UN.women.rural.", "PAF.UN.women.total."), key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i, " districts: PAF for women"), col.regions = my.palette, layout = c(3, 1), as.table=TRUE))
dev.off()


## Scratch
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


