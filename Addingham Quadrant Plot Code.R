#ADDINGHAM DATA#

load("~/.RData")
Addingham_Data <- read.csv(file.choose(), header = T)

#Define all variables from data#

Addingham_Level <- Addingham_Data$Level
Addingham_Flow <- Addingham_Data$Flow
Addingham_Day <- Addingham_Data$Day

#Create vectors for each variable#

Addingham_Level_vector <- c(Addingham_Level)
Addingham_Flow_vector <- c(Addingham_Flow)
Addingham_Day_vector <- c(Addingham_Day)

scalevector <- function(x) {return(((x-min(x)) / (max(x)-min(x))))}

#Vectors for scaled variables#

Addingham_Level_scaled <- scalevector(Addingham_Level_vector)
Addingham_Flow_scaled <- scalevector(Addingham_Flow_vector)
Addingham_Day_scaled <- scalevector(Addingham_Day_vector)

#Combine scaled vectors#

Addingham_Negative_Level <- -(Addingham_Level_scaled)
Addingham_HeightandDay <- c(Addingham_Negative_Level, Addingham_Day_scaled, Addingham_Negative_Level)
Addingham_Two_flowrate <- c(Addingham_Flow_scaled, Addingham_Flow_scaled)
Addingham_Negative_Day <- -(Addingham_Day_scaled)
FlowrateandDay <- c(Addingham_Negative_day, Addingham_Two_flowrate)

#Import Data For Rating Curve#

Addingham_Rating <- read.csv(file.choose(), header = T)

#Rename Vectors#

Addingham_A <- Addingham_Rating$a
Addingham_B <- Addingham_Rating$b
Addingham_C <- Addingham_Rating$C
Addingham_Lowlims <- Addingham_Rating$Lower.limit..m.
Addingham_Uplims <- Addingham_Rating$Upper.limit..m.
View(Addingham_Uplims)

#Create Values For RC#

Addingham_Rating_Curve <- seq(0.72, 2.463, 0.001817)

Addingham_h1 <- Addingham_Lowlims[1] <= Addingham_Rating_Curve & Addingham_Rating_Curve<Addingham_Uplims[1]
Addingham_h1vals = Addingham_Rating_Curve[Addingham_h1]

Addingham_h2 <- Addingham_Lowlims[2] <= Addingham_Rating_Curve & Addingham_Rating_Curve<Addingham_Uplims[2]
Addingham_h2vals = Addingham_Rating_Curve[Addingham_h2]

Addingham_h3 <- Addingham_Lowlims[3] <= Addingham_Rating_Curve & Addingham_Rating_Curve<Addingham_Uplims[3]
Addingham_h3vals = Addingham_Rating_Curve[Addingham_h3]

Addingham_h4 <- Addingham_Lowlims[4] <= Addingham_Rating_Curve & Addingham_Rating_Curve<Addingham_Uplims[4]
Addingham_h4vals = Addingham_Rating_Curve[Addingham_h4]

Addingham_h5 <- Addingham_Lowlims[5] <= Addingham_Rating_Curve & Addingham_Rating_Curve<Addingham_Uplims[5]
Addingham_h5vals = Addingham_Rating_Curve[Addingham_h5]

Addingham_Q1 = Addingham_C[1]*(h1vals - Aire_A[1])^(Aire_B[1])
Addingham_Q2 = Addingham_C[2]*(Addingham_h2vals - Addingham_A[2])^(Addingham_B[2])
Addingham_Q3 = Addingham_C[3]*(Addingham_h3vals - Addingham_A[3])^(Addingham_B[3])
Addingham_Q4 = Addingham_C[4]*(Addingham_h4vals - Addingham_A[4])^(Addingham_B[4])
Addingham_Q5 = Addingham_C[5]*(Addingham_h5vals - Addingham_A[5])^(Addingham_B[5])


Addingham_Q <- c(Addingham_Q1, Addingham_Q2, Addingham_Q3, Addingham_Q4, Addingham_Q5)
Addingham_H_For_Flow <- c(Addingham_h1vals, Addingham_h2vals, Addingham_h3vals, Addingham_h4vals, Addingham_h5vals)
View(Addingham_H_For_Flow)
(max(Addingham_Level))

#Check RC#

plot((-(Addingham_H_For_Flow)), ((Addingham_Q)), pch=".", lty=1)
plot((-scalevector(Addingham_H_For_Flow)), (scalevector(Addingham_Q)), pch=".", lty=1)
segments(x0=0, y0=0, x1=-1, y1=1, lty=2)

Addingham_Rating_Curve_Scaled <- c((-scalevector(Addingham_H_For_Flow)), (scalevector(Addingham_Q)))

#Plot Flow Data with the RC#

Addingham_g1 <- Addingham_Lowlims[1] <= Addingham_Level & Addingham_Level<Addingham_Uplims[1]
Addingham_g1vals = Addingham_Level[Addingham_g1]

Addingham_g2 <- Addingham_Lowlims[2] <= Addingham_Level & Addingham_Level<Addingham_Uplims[2]
Addingham_g2vals = Addingham_Level[Addingham_g2]

Addingham_g3 <- Addingham_Lowlims[3] <= Addingham_Level & Addingham_Level<Addingham_Uplims[3]
Addingham_g3vals = Addingham_Level[Addingham_g3]

Addingham_g4 <- Addingham_Lowlims[4] <= Addingham_Level & Addingham_Level<Addingham_Uplims[4]
Addingham_g4vals = Addingham_Level[Addingham_g4]

Addingham_g5 <- Addingham_Lowlims[5] <= Addingham_Level & Addingham_Level<Addingham_Uplims[5]
Addingham_g5vals = Addingham_Level[Addingham_g5]

View(max(Addingham_Level))

Addingham_Q_1 = Addingham_C[1]*(Addingham_g1vals - Addingham_A[1])^(Addingham_B[1])
Addingham_Q_2 = Addingham_C[2]*(Addingham_g2vals - Addingham_A[2])^(Addingham_B[2])
Addingham_Q_3 = Addingham_C[3]*(Addingham_g3vals - Addingham_A[3])^(Addingham_B[3])
Addingham_Q_4 = Addingham_C[4]*(Addingham_g4vals - Addingham_A[4])^(Addingham_B[4])
Addingham_Q_5 = Addingham_C[5]*(Addingham_g5vals - Addingham_A[5])^(Addingham_B[5])

Addingham_Q_For_RC <- c(Addingham_Q_1, Addingham_Q_2, Addingham_Q_3, Addingham_Q_4, Addingham_Q_5)
Addingham_G_For_Flow <- c(Addingham_g1vals, Addingham_g2vals, Addingham_g3vals, Addingham_g4vals, Addingham_g5vals)
Addingham_Flow_from_RC <- scalevector(Addingham_Q_For_RC)
View(Addingham_Q_For_RC)
View(Addingham_Day_scaled)

#Check Flow#

plot(Addingham_Day_scaled, Addingham_Flow_from_RC, pch=".", lty=1)
View(Addingham_Q_For_RC)
View(Addingham_G_For_Flow)

#Scale t and m values#

Addingham_Ht <- 1.87
Addingham_Hm <- 2.20
Addingham_Qm <- 300.13
Addingham_Qt <- 207.51
Addingham_scaledht <- (1.87 - min(Addingham_Level)) / (max(Addingham_Level) - min(Addingham_Level))
Addingham_scaledhm <- (2.2 - min(Addingham_Level)) / (max(Addingham_Level) - min(Addingham_Level))
Addingham_scaledqm <- (300.13 - min(Addingham_Flow)) / (max(Addingham_Flow) - min(Addingham_Flow))
Addingham_scaledqt <- (207.51 - min(Addingham_Flow)) / (max(Addingham_Flow) - min(Addingham_Flow))

#Hm is the mean of the heights above Ht# Ht given or estimated#

#Polygon#
Addingham_Data_For_Polygon <- read.csv(file.choose(), header = T)
Flow_Data_For_Polygon <- Addingham_Data_For_Polygon$Flow
Addingham_Data_For_Polygon_1 <- read.csv(file.choose(), header = T)
Day_Data_For_Polygon <- Addingham_Data_For_Polygon_1$Day
View(Day_Data_For_Polygon)

polygon(x=Scaled_Day_Data_For_Polygon[375:495], y=Scaled_Flow_Data_For_Polygon[375:495], col="gray")
#375, 493#
View(scalevector(Flow_Data_For_Polygon[375:495]))
Scaled_Day_Data_For_Polygon <- scalevector(Day_Data_For_Polygon)
Scaled_Flow_Data_For_Polygon <- scalevector(Flow_Data_For_Polygon)

#0.7 to 2.463#
#Find Scaled Axis Values#

(0.72 - min(Addingham_Level)) / (max(Addingham_Level) - min(Addingham_Level))
(1 - min(Addingham_Level)) / (max(Addingham_Level) - min(Addingham_Level))
(1.5 - min(Addingham_Level)) / (max(Addingham_Level) - min(Addingham_Level))
(2 - min(Addingham_Level)) / (max(Addingham_Level) - min(Addingham_Level))
(2.5 - min(Addingham_Level)) / (max(Addingham_Level) - min(Addingham_Level))
(350 - min(Addingham_Flow)) / (max(Addingham_Flow) - min(Addingham_Flow))
(400 - min(Addingham_Flow)) / (max(Addingham_Flow) - min(Addingham_Flow))
(300 - min(Addingham_Flow)) / (max(Addingham_Flow) - min(Addingham_Flow))
(250 - min(Addingham_Flow)) / (max(Addingham_Flow) - min(Addingham_Flow))
(200 - min(Addingham_Flow)) / (max(Addingham_Flow) - min(Addingham_Flow))
(100 - min(Addingham_Flow)) / (max(Addingham_Flow) - min(Addingham_Flow))

#FEV#
#27.12500-25.89583#
Addingham_FEV <- (((1.22917*24)*3600) * (300.13-207.51))
#FEV=9.84Mm^3
Addingham_Tf <- 1.22917*24

#Plot the Quadrant Plot#

plot(1, type="n", xlab="", ylab="", xlim=c(-1, 1), ylim=c(-1, 1), axes = F)
lines(Addingham_Negative_Level, Addingham_Negative_Day, lty=1)
lines(-scalevector(Addingham_H_For_Flow),scalevector(Addingham_Q), lty=1)
lines(Addingham_Day_scaled, Addingham_Flow_from_RC, lty=1)
segments(x0=0, y0=0, x1=-1, y1=1, lty=2)

#lines(Addingham_Day_scaled, Addingham_Flow_scaled) - Code for flow given not RC#

segments(x0=-1, y0=0, x1=1, y1=0, lty=1)
segments(x0=0, y0=-1,x1=0,y1=1, lty=1)

text(x=0, y=-0.9 ,label="t", cex=1, pos=4, font=3)
polygon(x=Scaled_Day_Data_For_Polygon[375:495], y=Scaled_Flow_Data_For_Polygon[375:495], col="gray")
abline(v=-(Addingham_scaledht), lty=3)
abline(v=-(Addingham_scaledhm), lty=3)
abline(h=Addingham_scaledqm, lty=3)
abline(h=Addingham_scaledqt, lty=3)
segments(x0=0, y0=0, x1 = -max(Addingham_Level_scaled), y1 = max(Addingham_Flow_scaled), lty=2)
segments(x0=Addingham_Day_scaled[375], y0=Addingham_scaledqm, x1=Addingham_Day_scaled[493], y1=Addingham_scaledqm)
segments(x0=Addingham_Day_scaled[375], y0=Addingham_scaledqt, x1=Addingham_Day_scaled[375], y1=Addingham_scaledqm)
segments(x0=Addingham_Day_scaled[493], y0=Addingham_scaledqt, x1=Addingham_Day_scaled[493], y1=Addingham_scaledqm)
segments(x0=Addingham_Day_scaled[375], y0=Addingham_scaledqt, x1=Addingham_Day_scaled[493], y1=Addingham_scaledqt)
segments(x0=Addingham_Day_scaled[375], y0=-0.35, x1=Addingham_Day_scaled[375], y1=Addingham_scaledqt, lty=3)
segments(x0=Addingham_Day_scaled[493], y0=-0.35, x1=Addingham_Day_scaled[493], y1=Addingham_scaledqt, lty=3)
arrows(x0=Addingham_Day_scaled[375], y0=-0.35, x1=Addingham_Day_scaled[493], y1=-0.35, length = 0.05, code=3)

#Graph labels#

text(x=Addingham_Day_scaled[425], y=-0.35, label="Tf", cex=1, pos=1, font=3)
text(x=Day_scaled[400], y=-0.53, label="FEV ≈ 9.84Mm^3", cex=1)
text(x=Day_scaled[400], y=-0.62, label="Tf = 29.50hrs", cex=1)
text(x=Day_scaled[400], y=-0.71, label="Ht = 1.87m", cex=1)
text(x=Day_scaled[400], y=-0.80, label="Hm = 2.20m", cex=1)
text(x=Day_scaled[400], y=-0.89, label="Qt = 207.51", cex=1)
text(x=Day_scaled[485], y=-0.88 ,label=expression("m"^3), cex=1)
text(x=Day_scaled[515], y=-0.88 ,label="/s", cex=1)
text(x=Day_scaled[400], y=-0.99, label="Qm = 300.13", cex=1)
text(x=Day_scaled[495], y=-0.98 ,label=expression("m"^3), cex=1)
text(x=Day_scaled[520], y=-0.98,label="/s", cex=1)
text(x=0.03, y=-0.9 ,label="[day]", cex=1, pos=4)
text(x=0, y=0.95 ,label="Q", cex=1, pos=4, font=3)
text(x=0.05, y=0.9 ,label=expression("[m"^3), cex=1)
text(x=0.049, y=0.89 ,label="/s]", cex=1, pos=4)
text(x=0.93, y=-0.01 ,label="t", cex=1, pos=1, font=3)
text(x=1, y=-0.01 ,label="[day]", cex=1, pos=1)
text(x=-0.98, y=-0.01 ,label=bquote(bar(h)), cex=1, pos=3, font=3)
text(x=-0.92, y=-0.01 ,label="[m]", cex=1, pos=3)
text(x=-(Addingham_scaledht), y=-0.9 ,label="Ht", cex=1, pos=4, font=3)
text(x=-(Addingham_scaledhm), y=-0.9 ,label="Hm", cex=1, pos=4, font=3)
text(x=0.9, y=Addingham_scaledqm ,label="Qm", cex=1, pos=1, font=3)
text(x=0.9, y=Addingham_scaledqt ,label="Qt", cex=1, pos=1, font=3)
axis(side=1, cex.axis=1, pos=0, at=c(-1.021228,-0.734366,-0.4475043,-0.1606426,0.0,0.2,0.4,0.6,0.8,1.0),labels=c("2.5","2","1.5","1","22","24","26","28","30","32"))
axis(side=2,cex.axis=1, pos=0, at=c(-1.0,-(4/5),-(3/5),-(2/5),-(1/5),0.0,0.2228261,0.4945652,0.7663043,1.038043),labels=c("32","30","28","26","24","22","100","200","300","400"), las=1)

