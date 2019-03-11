#CALDER DATA#

#Import Calder data#

Calder_data <- read.csv(file.choose(), header = T)

Calder_Flowrate <- Calder_data$Flowrate
Calder_Height <- Calder_data$Riverheight
Calder_Day <- Calder_data$Day

#Create vectors for each variable#

Calder_Riverheight_vector <- c(Calder_Height)
Calder_Flowrate_vector <- c(Calder_Flowrate)
Calder_Day_vector <- c(Calder_Day)

scalevector <- function(x) {return(((x-min(x)) / (max(x)-min(x))))}

#Vectors for scaled variables#

Calder_Riverheight_scaled <- scalevector(Calder_Riverheight_vector)
Calder_Flowrate_scaled <- scalevector(Calder_Flowrate_vector)
Calder_Day_scaled <- scalevector(Calder_Day_vector)

#Combine scaled vectors#

Calder_Negative_riverheight <- -(Calder_Riverheight_scaled)
Calder_HeightandDay <- c(Calder_Negative_riverheight, Calder_Day_scaled, Calder_Negative_riverheight)
Calder_Two_flowrate <- c(Calder_Flowrate_scaled, Calder_Flowrate_scaled)
Calder_Negative_day <- -(Calder_Day_scaled)
Calder_FlowrateandDay <- c(Calder_Negative_day, Calder_Two_flowrate)

#Scale the lines#

Calder_scaledht <- (4.5 - min(Calder_Height)) / (max(Calder_Height) - min(Calder_Height))
Calder_scaledhm <- (5.25 - min(Calder_Height)) / (max(Calder_Height) - min(Calder_Height))
Calder_scaledqm <- (197.5 - min(Calder_Flowrate)) / (max(Calder_Flowrate) - min(Calder_Flowrate))
Calder_scaledqt <- ((142 - min(Calder_Flowrate)) / (max(Calder_Flowrate) - min(Calder_Flowrate)))

#Plot all data together#

plot(Calder_HeightandDay, Calder_FlowrateandDay, pch=".", axes=FALSE, xlab=NA, ylab=NA, xaxt='n', yaxt='n', main="Calder Graph")
polygon((cbind(c(scalevector(26.35417), (Calder_Day_scaled[130:164]), scalevector(26.69792)), c((scalevector(164.1)), (Calder_Flowrate_scaled[130:163]), scalevector(164.1)))), col ="110000")
axis(side=1, cex.axis=0.5, pos=0, at=c(-(4731/4466),-(533/638),-(2731/4466),-(1731/4466),-(731/4466),0.25,0.5,0.75,1.0),labels=c("6","5","4","3","2","26","27","28","29"))
axis(side=2,cex.axis=0.5, pos=0, at=c(-1.0,-(3/4),-(2/4),-(1/4),0.0,2413/11593,4643/11593,7143/11593,9643/11593,1.047442422),labels=c("29","28","27","26","25","50","100","150","200","250"), las=1)
text(x=0, y=-1 ,label="t", cex=0.5, pos=4, font=3)
abline(v=-(Calder_scaledht), lty=3)
abline(v=-(Calder_scaledhm), lty=3)
abline(h=Calder_scaledqm, lty=3)
abline(h=Calder_scaledqt, lty=3)
segments(x0=0, y0=0, x1 = -max(Calder_Riverheight_scaled), y1 = max(Calder_Flowrate_scaled), lty=2)
segments(x0=Calder_Day_scaled[130], y0=Calder_scaledqt, x1=Calder_Day_scaled[163], y1=Calder_scaledqt)
segments(x0=Calder_Day_scaled[130], y0=Calder_scaledqm, x1=Calder_Day_scaled[163], y1=Calder_scaledqm)
segments(x0=Calder_Day_scaled[130], y0=Calder_scaledqt, x1=Calder_Day_scaled[130], y1=Calder_scaledqm)
segments(x0=Calder_Day_scaled[163], y0=Calder_scaledqt, x1=Calder_Day_scaled[163], y1=Calder_scaledqm)
segments(x0=Calder_Day_scaled[130], y0=-0.35, x1=Calder_Day_scaled[130], y1=Calder_scaledqt, lty=3)
segments(x0=Calder_Day_scaled[165], y0=-0.35, x1=Calder_Day_scaled[165], y1=Calder_scaledqt, lty=3)
arrows(x0=Calder_Day_scaled[131], y0=-0.35, x1=Calder_Day_scaled[165], y1=-0.35, length = 0.05, code=3)

#Graph labels#

text(x=Calder_Day_scaled[148], y=-0.35, label="Tf", cex=0.5, pos=1, font=3)
text(x=Calder_Day_scaled[330], y=-0.55, label="FEV ≈ 1.65Mm^3", cex=0.5)
text(x=Calder_Day_scaled[350], y=-0.62, label="Tf = 8.25hrs", cex=0.5)
text(x=Calder_Day_scaled[350], y=-0.69, label="hT = 4.50m", cex=0.5)
text(x=Calder_Day_scaled[350], y=-0.76, label="hM = 5.25m", cex=0.5)
text(x=Calder_Day_scaled[320], y=-0.83, label="Qt = 142.0", cex=0.5)
text(x=Calder_Day_scaled[370], y=-0.82 ,label=expression("m"^3), cex=0.5)
text(x=Calder_Day_scaled[385], y=-0.82 ,label="/s", cex=0.5)
text(x=Calder_Day_scaled[320], y=-0.9, label="Qm = 197.5", cex=0.5)
text(x=Calder_Day_scaled[370], y=-0.89 ,label=expression("m"^3), cex=0.5)
text(x=Calder_Day_scaled[385], y=-0.89,label="/s", cex=0.5)
text(x=0.03, y=-1 ,label="[day]", cex=0.5, pos=4)
text(x=0, y=1 ,label="Q", cex=0.5, pos=4, font=3)
text(x=0.135, y=1 ,label=expression("[m"^3), cex=0.5)
text(x=0.125, y=0.99 ,label="/s]", cex=0.5, pos=4)
text(x=0.93, y=-0.05 ,label="t", cex=0.5, pos=1, font=3)
text(x=1, y=-0.05 ,label="[day]", cex=0.5, pos=1)
text(x=-0.98, y=-0.01 ,label=bquote(bar(h)), cex=0.5, pos=3, font=3)
text(x=-0.92, y=-0.01 ,label="[m]", cex=0.5, pos=3)
text(x=-(Calder_scaledht), y=-1 ,label="hT", cex=0.5, pos=4, font=3)
text(x=-(Calder_scaledhm), y=-1 ,label="hM", cex=0.5, pos=4, font=3)
text(x=1, y=Calder_scaledqm ,label="Qm", cex=0.5, pos=1, font=3)
text(x=1, y=Calder_scaledqt ,label="Qt", cex=0.5, pos=1, font=3)
######

Calder_Rating <- read.csv(file.choose(), header = T)

#Rename Vectors#

Calder_A <- c(0.342, 0.826, -0.856)
Calder_B <- c(2.239, 1.37, 2.515)
Calder_C <- c(8.459, 21.5, 2.086)
Calder_Lowlims <- c(0, 2.107, 3.088)
Calder_Uplims <- c(2.107, 3.088, 5.8)
View(Calder_Uplims)
View(min(Calder_Height))
#Create Values For RC#

Calder_Rating_Curve <- seq(1.269, 5.735, 0.00776)

Calder_h1 <- Calder_Lowlims[1] <= Calder_Rating_Curve & Calder_Rating_Curve<Calder_Uplims[1]
Calder_h1vals = Calder_Rating_Curve[Calder_h1]

Calder_h2 <- Calder_Lowlims[2] <= Calder_Rating_Curve & Calder_Rating_Curve<Calder_Uplims[2]
Calder_h2vals = Calder_Rating_Curve[Calder_h2]

Calder_h3 <- Calder_Lowlims[3] <= Calder_Rating_Curve & Calder_Rating_Curve<Calder_Uplims[3]
Calder_h3vals = Calder_Rating_Curve[Calder_h3]


Calder_Q1 = Calder_C[1]*(Calder_h1vals - Calder_A[1])^(Calder_B[1])
Calder_Q2 = Calder_C[2]*(Calder_h2vals - Calder_A[2])^(Calder_B[2])
Calder_Q3 = Calder_C[3]*(Calder_h3vals - Calder_A[3])^(Calder_B[3])


Calder_Q <- c(Calder_Q1, Calder_Q2, Calder_Q3)
Calder_H_For_Flow <- c(Calder_h1vals, Calder_h2vals, Calder_h3vals)
View(Calder_H_For_Flow)
(max(Calder_Level))

#Check RC#

plot((-(Calder_H_For_Flow)), ((Calder_Q)), pch=".", lty=1)
plot((-scalevector(Calder_H_For_Flow)), (scalevector(Calder_Q)), pch=".", lty=1)
segments(x0=0, y0=0, x1=-1, y1=1, lty=2)

Calder_Rating_Curve_Scaled <- c((-scalevector(Calder_H_For_Flow)), (scalevector(Calder_Q)))


#####
Calder_g1 <- Calder_Lowlims[1] <= Calder_Height & Calder_Height<Calder_Uplims[1]
Calder_g1vals = Calder_Height[Calder_g1]

Calder_g2 <- Calder_Lowlims[2] <= Calder_Height & Calder_Height<Calder_Uplims[2]
Calder_g2vals = Calder_Height[Calder_g2]

Calder_g3 <- Calder_Lowlims[3] <= Calder_Height & Calder_Height<Calder_Uplims[3]
Calder_g3vals = Calder_Height[Calder_g3]

View(max(Calder_Height))

Calder_Q_1 = Calder_C[1]*(Calder_g1vals - Calder_A[1])^(Calder_B[1])
Calder_Q_2 = Calder_C[2]*(Calder_g2vals - Calder_A[2])^(Calder_B[2])
Calder_Q_3 = Calder_C[3]*(Calder_g3vals - Calder_A[3])^(Calder_B[3])

Calder_Q_For_RC <- c(Calder_Q_1, Calder_Q_2, Calder_Q_3)
Calder_G_For_Flow <- c(Calder_g1vals, Calder_g2vals, Calder_g3vals)
Calder_Flow_from_RC <- scalevector(Calder_Q_For_RC)
View(Calder_Q_For_RC)
View(Calder_Day_scaled)

#Check Flow#

plot(-Calder_Day_scaled, Calder_Flow_from_RC, pch=".", lty=1)
View(Calder_Q_For_RC)
View(Calder_G_For_Flow)

#####
plot(1, type="n", xlab="", ylab="", xlim=c(-1, 1), ylim=c(-1, 1), axes = F)
lines(Calder_Negative_riverheight, Calder_Negative_day, lty=1)
lines(-scalevector(Calder_H_For_Flow),scalevector(Calder_Q), lty=1)
#lines(Calder_Day_scaled, Calder_Flow_from_RC, lty=1)
segments(x0=0, y0=0, x1=-1, y1=1, lty=2)
lines(Calder_Day_scaled, Calder_Flowrate_scaled)

text(x=0, y=-0.9 ,label="t", cex=1, pos=4, font=3)
polygon(x=Calder_Polygon_Day_Scaled[130:164], y=Calder_Polygon_Flow_Scaled[130:164], col="gray")
abline(v=-(Calder_scaledht), lty=3)
abline(v=-(Calder_scaledhm), lty=3)
abline(h=Calder_scaledqm, lty=3)
abline(h=Calder_scaledqt, lty=3)
segments(x0=0, y0=0, x1 = -max(Calder_Riverheight_scaled), y1 = max(Calder_Flowrate_scaled), lty=2)
segments(x0=Calder_Day_scaled[130], y0=Calder_scaledqm, x1=Calder_Day_scaled[163], y1=Calder_scaledqm)
segments(x0=Calder_Day_scaled[130], y0=Calder_scaledqt, x1=Calder_Day_scaled[130], y1=Calder_scaledqm)
segments(x0=Calder_Day_scaled[163], y0=Calder_scaledqt, x1=Calder_Day_scaled[163], y1=Calder_scaledqm)
segments(x0=Calder_Day_scaled[130], y0=Calder_scaledqt, x1=Calder_Day_scaled[163], y1=Calder_scaledqt)
segments(x0=Calder_Day_scaled[130], y0=-0.35, x1=Calder_Day_scaled[130], y1=Calder_scaledqt, lty=3)
segments(x0=Calder_Day_scaled[163], y0=-0.35, x1=Calder_Day_scaled[163], y1=Calder_scaledqt, lty=3)
arrows(x0=Calder_Day_scaled[130], y0=-0.35, x1=Calder_Day_scaled[163], y1=-0.35, length = 0.05, code=3)

#Graph labels#

text(x=Calder_Day_scaled[142], y=-0.35, label="Tf", cex=1, pos=1, font=3)
text(x=Day_scaled[400], y=-0.33, label="FEV ≈ 1.65Mm^3", cex=1)
text(x=Day_scaled[400], y=-0.42, label="Tf = 8.25hrs", cex=1)
text(x=Day_scaled[400], y=-0.51, label="Ht = 4.5m", cex=1)
text(x=Day_scaled[400], y=-0.60, label="Hm = 5.25m", cex=1)
text(x=Day_scaled[350], y=-0.69, label="Qt = 142", cex=1)
text(x=Day_scaled[460], y=-0.68 ,label=expression("m"^3), cex=1)
text(x=Day_scaled[500], y=-0.68 ,label="/s", cex=1)
text(x=Day_scaled[350], y=-0.79, label="Qm = 197.5", cex=1)
text(x=Day_scaled[495], y=-0.78 ,label=expression("m"^3), cex=1)
text(x=Day_scaled[532], y=-0.78,label="/s", cex=1)
text(x=0.03, y=-0.9 ,label="[day]", cex=1, pos=4)
text(x=0, y=0.95 ,label="Q", cex=1, pos=4, font=3)
text(x=0.05, y=0.87 ,label=expression("[m"^3), cex=1)
text(x=0.049, y=0.86 ,label="/s]", cex=1, pos=4)
text(x=0.8, y=-0.01 ,label="t", cex=1, pos=1, font=3)
text(x=0.9, y=-0.01 ,label="[day]", cex=1, pos=1)
text(x=-0.98, y=-0.01 ,label=bquote(bar(h)), cex=1, pos=3, font=3)
text(x=-0.9, y=-0.01 ,label="[m]", cex=1, pos=3)
text(x=-(Calder_scaledht), y=-0.9 ,label="Ht", cex=1, pos=4, font=3)
text(x=-(Calder_scaledhm), y=-0.9 ,label="Hm", cex=1, pos=4, font=3)
text(x=0.9, y=Calder_scaledqm ,label="Qm", cex=1, pos=1, font=3)
text(x=0.9, y=Calder_scaledqt ,label="Qt", cex=1, pos=1, font=3)
axis(side=1, cex.axis=1, pos=0, at=c(-1.059337,-0.8354232,-0.6115092,-0.3875952,-0.1636811, 0.0,1/6,2/6,3/6,4/6,5/6,6),labels=c("6","5","4","3","2","25","26","27","28","29","30","31"))
axis(side=2,cex.axis=1, pos=0, at=c(-1.0,-(5/6),-(4/6),-(3/6),-(2/6),-(1/6),0.1848529,0.4005003,0.6161477,0.831795,1.047442),labels=c("31","30","29","28","27","26","50","100","150","200","250"), las=1)

####
(250 - min(Calder_Flowrate)) / (max(Calder_Flowrate) - min(Calder_Flowrate))
#130,164
Calder_Polygon <- read.csv(file.choose(), header = T)
Calder_Polygon_Day <- Calder_Polygon$Day
Calder_Polygon_Flow <- Calder_Polygon$Flowrate
Calder_Polygon_Day_Scaled <- scalevector(Calder_Polygon_Day)
Calder_Polygon_Flow_Scaled <- scalevector(Calder_Polygon_Flow)
