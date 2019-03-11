#AIRE DATA#

load("~/.RData")
Day <- Aire_Data$Day
View(Aire_Data)
Aire_Data <- read.csv(file.choose(), header = T)

#Define all variables from data#

Riverheight <- Aire_Data$Riverheight
Flowrate <- Aire_Data$Flowrate
Day <- Aire_Data$Day

#Create vectors for each variable#

Riverheight_vector <- c(Riverheight)
Flowrate_vector <- c(Flowrate)
Day_vector <- c(Day)

scalevector <- function(x) {return(((x-min(x)) / (max(x)-min(x))))}

#Vectors for scaled variables#

Riverheight_scaled <- scalevector(Riverheight_vector)
Flowrate_scaled <- scalevector(Flowrate_vector)
Day_scaled <- scalevector(Day_vector)

#Combine scaled vectors#

Negative_riverheight <- -(Riverheight_scaled)
HeightandDay <- c(Negative_riverheight, Day_scaled, Negative_riverheight)
Two_flowrate <- c(Flowrate_scaled, Flowrate_scaled)
Negative_day <- -(Day_scaled)
FlowrateandDay <- c(Negative_day, Two_flowrate)

#Import Data For Rating Curve#
Aire_Rating <- read.csv(file.choose(), header = T)

#Rename Vectors# incorrect values for armley, needs correcting
Aire_A <- c(0.156, 0.028, 0.153)
Aire_B <- c(1.115, 1.462, 1.502)
Aire_C <- c(30.69, 27.884, 30.127)
Lowlims <- c(0, 0.685, 1.917)
Uplims <- c(0.685, 1.917, 6)

#Create Values#
Rating_Curve <- seq(1, 5.217, 0.00733)

h1 <- Lowlims[1] <= Rating_Curve & Rating_Curve<Uplims[1]
h1vals = Rating_Curve[h1]

h2 <- Lowlims[2] <= Rating_Curve & Rating_Curve<Uplims[2]
h2vals = Rating_Curve[h2]

h3 <- Lowlims[3] <= Rating_Curve & Rating_Curve<Uplims[3]
h3vals = Rating_Curve[h3]

Q1 = Aire_C[1]*(h1vals - Aire_A[1])^(Aire_B[1])
Q2 = Aire_C[2]*(h2vals - Aire_A[2])^(Aire_B[2])
Q3 = Aire_C[3]*(h3vals - Aire_A[3])^(Aire_B[3])

Q <- c(Q1, Q2, Q3)
H_For_Flow <- c(h1vals, h2vals, h3vals)

plot((-scalevector(H_For_Flow)), (scalevector(Q)), pch=".", lty=1)
segments(x0=0, y0=0, x1=-1, y1=1, lty=2)

Rating_Curve_Scaled <- c((-scalevector(H_For_Flow)), (scalevector(Q)))

scaledht <- (3.9 - min(Riverheight)) / (max(Riverheight) - min(Riverheight))
scaledhm <- (4.77 - min(Riverheight)) / (max(Riverheight) - min(Riverheight))
scaledqm <- (219.1 - min(Flowrate)) / (max(Flowrate) - min(Flowrate))
scaledqt <- (300.2 - min(Flowrate)) / (max(Flowrate) - min(Flowrate))
#Hm is the mean of the heights above Ht# Ht given or estimated#

no2 <- (2 - min(Riverheight)) / (max(Riverheight) - min(Riverheight))
no1 <- (1 - min(Riverheight)) / (max(Riverheight) - min(Riverheight))
no3 <- (3 - min(Riverheight)) / (max(Riverheight) - min(Riverheight))
no4 <- (4 - min(Riverheight)) / (max(Riverheight) - min(Riverheight))
no5 <- (5 - min(Riverheight)) / (max(Riverheight) - min(Riverheight))


plot(1, type="n", xlab="", ylab="", xlim=c(-1, 1), ylim=c(-1, 1), axes = F)
lines(Negative_riverheight, Negative_day, lty=1)
lines(Day_scaled, Flowrate_from_RC, pch=".", lty=1)
lines(-scalevector(H_For_Flow),scalevector(Q), lty=1)
segments(x0=0, y0=0, x1=-1, y1=1, lty=2)
lines(Day_scaled, Flowrate_scaled)
segments(x0=-1, y0=0, x1=1, y1=0, lty=1)
segments(x0=0, y0=-1,x1=0,y1=1, lty=1)
polygon(x=Day_scaled[138:266], y=Flowrate_scaled[138:266], col="gray")
text(x=0, y=-0.9 ,label="t", cex=1, pos=4, font=3)
abline(v=-(scaledht), lty=3)
abline(v=-(scaledhm), lty=3)
abline(h=scaledqm, lty=3)
abline(h=scaledqt, lty=3)
segments(x0=0, y0=0, x1 = -max(Riverheight_scaled), y1 = max(Flowrate_scaled), lty=2)
segments(x0=Day_scaled[137], y0=scaledqm, x1=Day_scaled[265], y1=scaledqm)
segments(x0=Day_scaled[137], y0=scaledqt, x1=Day_scaled[137], y1=scaledqm)
segments(x0=Day_scaled[265], y0=scaledqt, x1=Day_scaled[265], y1=scaledqm)
segments(x0=Day_scaled[137], y0=scaledqt, x1=Day_scaled[265], y1=scaledqt)
segments(x0=Day_scaled[137], y0=-0.35, x1=Day_scaled[137], y1=scaledqt, lty=3)
segments(x0=Day_scaled[265], y0=-0.35, x1=Day_scaled[265], y1=scaledqt, lty=3)
arrows(x0=Day_scaled[137], y0=-0.35, x1=Day_scaled[265], y1=-0.35, length = 0.05, code=3)

#Graph labels#

text(x=Day_scaled[201], y=-0.35, label="Tf", cex=1, pos=1, font=3)
text(x=Day_scaled[400], y=-0.55, label="FEV ≈ 9.34Mm^3", cex=1)
text(x=Day_scaled[418], y=-0.62, label="Tf = 32.00hrs", cex=1)
text(x=Day_scaled[424], y=-0.69, label="Ht = 3.90m", cex=1)
text(x=Day_scaled[424], y=-0.76, label="Hm = 4.77m", cex=1)
text(x=Day_scaled[390], y=-0.83, label="Qt = 219.1", cex=1)
text(x=Day_scaled[464], y=-0.82 ,label=expression("m"^3), cex=1)
text(x=Day_scaled[484], y=-0.82 ,label="/s", cex=1)
text(x=Day_scaled[380], y=-0.9, label="Qm = 300.2", cex=1)
text(x=Day_scaled[464], y=-0.89 ,label=expression("m"^3), cex=1)
text(x=Day_scaled[484], y=-0.89,label="/s", cex=1)
text(x=0.03, y=-0.9 ,label="[day]", cex=1, pos=4)
text(x=0, y=0.95 ,label="Q", cex=1, pos=4, font=3)
text(x=0.05, y=0.9 ,label=expression("[m"^3), cex=1)
text(x=0.047, y=0.89 ,label="/s]", cex=1, pos=4)
text(x=0.93, y=-0.02 ,label="t", cex=1, pos=1, font=3)
text(x=1, y=-0.02 ,label="[day]", cex=1, pos=1)
text(x=-0.98, y=-0.01 ,label=bquote(bar(h)), cex=1, pos=3, font=3)
text(x=-0.92, y=-0.01 ,label="[m]", cex=1, pos=3)
text(x=-(scaledht), y=-0.9 ,label="Ht", cex=1, pos=4, font=3)
text(x=-(scaledhm), y=-0.9 ,label="Hm", cex=1, pos=4, font=3)
text(x=0.9, y=scaledqt ,label="Qm", cex=1, pos=1, font=3)
text(x=0.9, y=scaledqm ,label="Qt", cex=1, pos=1, font=3)
axis(side=1, cex.axis=1, pos=0, at=c(-(4874/4091),-(3874/4091),-(2874/4091),-(1874/4091),-(874/4091),0.0,0.2,0.4,0.6,0.8,1.0),labels=c("6","5","4","3","2","25","26","27","28","29","30"))
axis(side=2,cex.axis=1, pos=0, at=c(-1.0,-(4/5),-(3/5),-(2/5),-(1/5),1/7,2/7,3/7,4/7,5/7,6/7,1),labels=c("30","29","28","27","26","50","100","150","200","250","300","350"), las=1)















Rating_Curve <- seq(1, 5.217, 0.00733)

g1 <- Lowlims[1] <= Riverheight & Riverheight<Uplims[1]
g1vals = Riverheight[g1]

g2 <- Lowlims[2] <= Riverheight & Riverheight<Uplims[2]
g2vals = Riverheight[g2]

g3 <- Lowlims[3] <= Riverheight & Riverheight<Uplims[3]
g3vals = Riverheight[g3]

Q_1 = Aire_C[1]*(g1vals - Aire_A[1])^(Aire_B[1])
Q_2 = Aire_C[2]*(g2vals - Aire_A[2])^(Aire_B[2])
Q_3 = Aire_C[3]*(g3vals - Aire_A[3])^(Aire_B[3])

Q_For_RC <- c(Q_1, Q_2, Q_3)
G_For_Flow <- c(g1vals, g2vals, g3vals)
Flowrate_from_RC <- scalevector(Q_For_RC)
plot(-Day, Q_For_RC, pch=".", lty=1)
plot(-Day_scaled, Flowrate_from_RC, pch=".", lty=1)
View(Q_For_RC)
View(G_For_Flow)
