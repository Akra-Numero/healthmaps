# Bijoy Joseph
# 2019.07.20
# Download India map data from http://gadm.org/, read in NFHS data and plot them
# Code examples taken from http://visual.yantrajaal.com/2015/05/using-r-for-maps-of-india-state.html
#+ and http://www.dpi.inpe.br/gilberto/tutorials/software/R-contrib/sp/html/spplot.html
# OUTPUTs:
#+  1) District-level map plots of data: Diabetes

library(sp)
library(RColorBrewer)

'%ni%' <- Negate('%in%')

## Read map of India, District-level
ind2 = readRDS("data/gadm36_IND_2_sp.rds")

## Read NFHS data, PAF for undernutrition
paf_un = read.csv2("data/NFHSDistrictlevel_withPAFcalculations.csv", stringsAsFactors=FALSE)

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
#  if(i %in% c("Delhi", "Daman and Diu","Goa")) next  # This is needed if anemia is plotted
  if(i %in% c("Delhi")) next
  state2 = (ind2[ind2$NAME_1 == i,])
  pafun_i = paf_un[(paf_un$State == i),]

# Merge data with map
state_data <- merge(state2, pafun_i, by.x="NAME_2", by.y="District")

# png(paste0("results/",gsub(' ','',i),"-anemia-children.png"), width = 880, height = 680)
# print(spplot(state_data, c("children.with.anemia.urban.","children.with.anemia.rural.","children.with.anemia..total."), key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i, " districts: Children with anemia"), names.attr=c("Urban","Rural","Total"), layout = c(3, 1), as.table=TRUE))
# dev.off()
# 
# png(paste0("results/",gsub(' ','',i),"-non-pregnant-anemia.png"), width = 880, height = 680)
# print(spplot(state_data, c("non.pregnant.women.with.anemia.urban.","non.pregnant.women.with.anemia.rural.","non.pregnant.women.with.anemia..total."), key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i, " districts: Non-pregnant women with anemia"), names.attr=c("Urban","Rural","Total"), layout = c(3, 1), as.table=TRUE))
# dev.off()
# 
# png(paste0("results/",gsub(' ','',i),"-pregnant-anemia.png"), width = 880, height = 680)
# print(spplot(state_data, c("pregnant.women.with.anemia.urban.","pregnant.women.with.anemia.rural.","pregnant.women.with.anemia..total."), key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i, " districts: Pregnant women with anemia"), names.attr=c("Urban","Rural","Total"), layout = c(3, 1), as.table=TRUE))
# dev.off()
# 
# png(paste0("results/",gsub(' ','',i),"-all-women-anemia.png"), width = 880, height = 680)
# print(spplot(state_data, c("all.women.with.anemia.urban.","all.women.with.anemia.rural.","all.women.with.anemia..total."), key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i," districts: All women with anemia"), names.attr=c("Urban","Rural","Total"), layout = c(3, 1), as.table=TRUE))
# dev.off()
# 
# png(paste0("results/",gsub(' ','',i),"-men-anemia.png"), width = 880, height = 680)
# print(spplot(state_data, c("Men.with.anemia.urban.","Men.with.anemia.rural.","Men.with.anemia.total."), key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i," districts: Men with anemia"), names.attr=c("Urban","Rural","Total"), layout = c(3, 1), as.table=TRUE))
# dev.off()

png(paste0("results/",gsub(' ','',i),"-BS-gt140-women.png"), width = 880, height = 680)
print(spplot(state_data, c("BS..140.in.women.urban.", "BS..140.in.women.rural.", "BS.140.in.women.total."), key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i," districts: BS > 140 in women"), names.attr=c("Urban","Rural","Total"), col.regions = col, colorkey= list(space = "bottom"), layout = c(3, 1), as.table=TRUE, par.settings=list(fontsize=list(text=13))))
dev.off()
         
png(paste0("results/",gsub(' ','',i),"-BS-gt140-men.png"), width = 880, height = 680)
print(spplot(state_data, c("BS..140.in.men.urban.", "BS..140.in.men.rural.", "BS.140.in.men.total."), key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i," districts: BS > 140 in men"), names.attr=c("Urban","Rural","Total"), col.regions = col, colorkey= list(space = "bottom"), layout = c(3, 1), as.table=TRUE, par.settings=list(fontsize=list(text=13))))
dev.off()

png(paste0("results/",gsub(' ','',i),"-BS-gt160-men.png"), width = 880, height = 680)
print(spplot(state_data, c("BS..160.in.men.urban.", "BS..160.in.men.rural.", "BS.160.in.men.total."), key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i," districts: BS > 160 in men"), names.attr=c("Urban","Rural","Total"), col.regions = col, colorkey= list(space = "bottom"), layout = c(3, 1), as.table=TRUE, par.settings=list(fontsize=list(text=13))))
dev.off()

png(paste0("results/",gsub(' ','',i),"-BS-gt160-women.png"), width = 880, height = 680)
print(spplot(state_data, c("BS..160.in.women.urban.", "BS..160.in.women.rural.", "BS.160.in.women.total."), key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i," districts: BS > 160 in women"), names.attr=c("Urban","Rural","Total"), col.regions = col, colorkey= list(space = "bottom"), layout = c(3, 1), as.table=TRUE, par.settings=list(fontsize=list(text=13))))
dev.off()

png(paste0("results/",gsub(' ','',i),"-PAF-DM-women.png"), width = 880, height = 680)
print(spplot(state_data, c("PAF.for.DM.in.women..urban.", "PAF.for.DM.in.women.rural.", "PAF.for.DM.in.women.total."), key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i," districts: PAF for Diabetes mellitus - women"), names.attr=c("Urban","Rural","Total"), col.regions = col, colorkey= list(space = "bottom"), layout = c(3, 1), as.table=TRUE, par.settings=list(fontsize=list(text=13))))
dev.off()

png(paste0("results/",gsub(' ','',i),"-PAF-DM-men.png"), width = 880, height = 680)
print(spplot(state_data, c("PAF.for.DM.in.Men.Urban.", "PAF.for.DM.in.men.rural.", "PAF.for.DM.in.men..Total."), key.space=list(x=0.1,y=.95,corner=c(0,1)), main=paste0(i," districts: PAF for Diabetes mellitus - men"), names.attr=c("Urban","Rural","Total"), col.regions = col, colorkey= list(space = "bottom"), layout = c(3, 1), as.table=TRUE, par.settings=list(fontsize=list(text=13))))
dev.off()

}

stop("End of script")


