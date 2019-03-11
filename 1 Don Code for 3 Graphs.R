#DON DATA#

Don_Data <- read.csv(file.choose(), header = T)
View(Don_Data)

#Define all variables from data#

Don_Height <- Don_Data$Stage
Don_Flowrate <- Don_Data$Flow
Don_Day <- Don_Data$Day

#Create vectors for each variable#
Don_Day_Vector <- c(Don_Day)
Don_Flow_Vector <- c(Don_Flowrate)
Don_Height_Vector <- c(Don_Height)

#Vectors for scaled variables#
scalevector <- function(q) {return ((q - min(q)) / (max(q) - min(q)))}

#Combine scaled vectors#
Don_Day_Scaled <- scalevector(Don_Day_Vector)
Don_Flow_Scaled <- scalevector(Don_Flow_Vector)
Don_Height_Scaled <- scalevector(Don_Height_Vector)


Don_negativeheight <- -(Don_Height_Scaled)
Don_heightandday <- c(Don_negativeheight, Don_Day_Scaled, Don_negativeheight)
Don_twoflow <- c(Don_Flow_Scaled, Don_Flow_Scaled)
Don_negativeday <- -(Don_Day_Scaled)
Don_flowandday <- c(Don_negativeday, Don_twoflow)

#Scale the lines#
Don_scaledht <- (2.9 - min(Don_Height)) / (max(Don_Height) - min(Don_Height))
Don_scaledhm <- (4.06 - min(Don_Height)) / (max(Don_Height) - min(Don_Height))
Don_scaledqm <- (225.9 - min(Don_Flowrate)) / (max(Don_Flowrate) - min(Don_Flowrate))
Don_scaledqt <- (164.1 - min(Don_Flowrate)) / (max(Don_Flowrate) - min(Don_Flowrate))

#Plot all data together#
par(mfcol = c(1,1))
plot(Don_heightandday, Don_flowandday, pch=".", axes=FALSE, xlab=NA, ylab=NA, xaxt='n', yaxt='n', main="Don graph")
axis(side=1, cex.axis=0.5, pos=0, at=c(-(1),-(4/5),-(3/5),-(2/5),-(1/5),1/4,2/4,3/4,1),labels=c("6","5","4","3","2","26","27","28","29"))
axis(side=2,cex.axis=0.5, pos=0, at=c(-1,-3/4,-2/4,-1/4,1024/6249,758/2083,3524/6249,4774/6249,2008/2083,1.164026244),labels=c("29","28","27","26","50","100","150","200","250","300"), las=1)
polygon(x=Don_Day_Scaled[132:161], y=Don_Flow_Scaled[132:161], col="grey")
abline(v=-(Don_scaledht), lty=3)
abline(v=-(Don_scaledhm), lty=3)
abline(h=Don_scaledqm, lty=3)
abline(h=Don_scaledqt, lty=3)
segments(x0=0, y0=0, x1 = -max(Don_Height_Scaled), y1 = max(Don_Flow_Scaled), lty=2)
segments(x0=Don_Day_Scaled[132], y0=Don_scaledqt, x1=Don_Day_Scaled[162], y1=Don_scaledqt)
segments(x0=Don_Day_Scaled[132], y0=Don_scaledqm, x1=Don_Day_Scaled[162], y1=Don_scaledqm)
segments(x0=Don_Day_Scaled[132], y0=Don_scaledqt, x1=Don_Day_Scaled[132], y1=Don_scaledqm)
segments(x0=Don_Day_Scaled[162], y0=Don_scaledqm, x1=Don_Day_Scaled[162], y1=Don_scaledqt)
segments(x0=Don_Day_Scaled[132], y0=-0.35, x1=Don_Day_Scaled[132], y1=Don_scaledqt, lty=3)
segments(x0=Don_Day_Scaled[162], y0=-0.35, x1=Don_Day_Scaled[162], y1=Don_scaledqt, lty=3)
arrows(x0=Don_Day_Scaled[132], y0=-0.35, x1=Don_Day_Scaled[162], y1=-0.35, length = 0.05, code=3)

#Graph labels#
text(x=0, y=-1 ,label="t", cex=0.5, pos=4, font=3)
text(x=0.03, y=-1 ,label="[day]", cex=0.5, pos=4)
text(x=0, y=1 ,label="Q", cex=0.5, pos=4, font=3)
text(x=0.135, y=1 ,label=expression("[m"^3), cex=0.5)
text(x=0.125, y=0.99 ,label="/s]", cex=0.5, pos=4)
text(x=0.93, y=-0.05 ,label="t", cex=0.5, pos=1, font=3)
text(x=1, y=-0.05 ,label="[day]", cex=0.5, pos=1)
text(x=-0.98, y=-0.01 ,label=bquote(bar(h)), cex=0.5, pos=3, font=3)
text(x=-0.92, y=-0.01 ,label="[m]", cex=0.5, pos=3)
text(x=-(Don_scaledht), y=-1 ,label="hT", cex=0.5, pos=4, font=3)
text(x=-(Don_scaledhm), y=-1 ,label="hM", cex=0.5, pos=4, font=3)
text(x=1, y=Don_scaledqm ,label="Qm", cex=0.5, pos=1, font=3)
text(x=1, y=Don_scaledqt ,label="Qt", cex=0.5, pos=1, font=3)
text(x=Don_Day_Scaled[148], y=-0.35, label="Tf", cex=0.5, pos=1, font=3)
text(x=Don_Day_Scaled[330], y=-0.55, label="FEV ≈ 3.00Mm^3", cex=0.5)
text(x=Don_Day_Scaled[345], y=-0.62, label="Tf = 13.50hrs", cex=0.5)
text(x=Don_Day_Scaled[350], y=-0.69, label="hT = 2.90m", cex=0.5)
text(x=Don_Day_Scaled[350], y=-0.76, label="hM = 4.06m", cex=0.5)
text(x=Don_Day_Scaled[320], y=-0.83, label="Qt = 164.1", cex=0.5)
text(x=Don_Day_Scaled[370], y=-0.82 ,label=expression("m"^3), cex=0.5)
text(x=Don_Day_Scaled[385], y=-0.82 ,label="/s", cex=0.5)
text(x=Don_Day_Scaled[320], y=-0.9, label="Qm = 225.9", cex=0.5)
text(x=Don_Day_Scaled[370], y=-0.89 ,label=expression("m"^3), cex=0.5)
text(x=Don_Day_Scaled[385], y=-0.89,label="/s", cex=0.5)

#######
Don_A <- c(0.223, 0.3077, 0.34, -0.5767)
Don_B <- c(1.7742, 1.3803, 1.2967, 1.1066)
Don_C <- c(78.4407, 77.2829, 79.5956, 41.3367)
Don_Lowlims <- c(0, 0.52, 0.931, 1.436)
Don_Uplims <- c(0.52, 0.931, 1.436, 4.675)
View(Don_Uplims)
View(min(Don_Height))
View(max(Don_Height))
#Create Values For RC#

Don_Rating_Curve <- seq(0.519, 4.675, 0.0108)

Don_h1 <- Don_Lowlims[1] <= Don_Rating_Curve & Don_Rating_Curve<Don_Uplims[1]
Don_h1vals = Don_Rating_Curve[Don_h1]

Don_h2 <- Don_Lowlims[2] <= Don_Rating_Curve & Don_Rating_Curve<Don_Uplims[2]
Don_h2vals = Don_Rating_Curve[Don_h2]

Don_h3 <- Don_Lowlims[3] <= Don_Rating_Curve & Don_Rating_Curve<Don_Uplims[3]
Don_h3vals = Don_Rating_Curve[Don_h3]

Don_h4 <- Don_Lowlims[4] <= Don_Rating_Curve & Don_Rating_Curve<Don_Uplims[4]
Don_h4vals = Don_Rating_Curve[Don_h4]

Don_Q1 = Don_C[1]*(Don_h1vals - Don_A[1])^(Don_B[1])
Don_Q2 = Don_C[2]*(Don_h2vals - Don_A[2])^(Don_B[2])
Don_Q3 = Don_C[3]*(Don_h3vals - Don_A[3])^(Don_B[3])
Don_Q4 = Don_C[4]*(Don_h4vals - Don_A[4])^(Don_B[4])

Don_Q <- c(Don_Q1, Don_Q2, Don_Q3, Don_Q4)
Don_H_For_Flow <- c(Don_h1vals, Don_h2vals, Don_h3vals, Don_h4vals)
View(Don_H_For_Flow)
(max(Don_Level))

#Check RC#

plot((-(Don_H_For_Flow)), ((Don_Q)), pch=".", lty=1)
plot((-scalevector(Don_H_For_Flow)), (scalevector(Don_Q)), pch=".", lty=1)
segments(x0=0, y0=0, x1=-1, y1=1, lty=2)

Don_Rating_Curve_Scaled <- c((-scalevector(Don_H_For_Flow)), (scalevector(Don_Q)))


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
lines(Don_negativeheight, Don_negativeday, lty=1)
lines(-scalevector(Don_H_For_Flow),scalevector(Don_Q), lty=1)
#lines(Calder_Day_scaled, Calder_Flow_from_RC, lty=1)
segments(x0=0, y0=0, x1=-1, y1=1, lty=2)
lines(Don_Day_Scaled, Don_Flow_Scaled)

text(x=0, y=-0.9 ,label="t", cex=1, pos=4, font=3)
polygon(x=Don_Polygon_Day_Scaled[50:105], y=Don_Polygon_Flow_Scaled[50:105], col="gray")
abline(v=-(Don_scaledht), lty=3)
abline(v=-(Don_scaledhm), lty=3)
abline(h=Don_scaledqm, lty=3)
abline(h=Don_scaledqt, lty=3)
segments(x0=0, y0=0, x1 = -max(Don_Height_Scaled), y1 = max(Don_Flow_Scaled), lty=2)
segments(x0=Don_Day_Scaled[49], y0=Don_scaledqm, x1=Don_Day_Scaled[104], y1=Don_scaledqm)
segments(x0=Don_Day_Scaled[49], y0=Don_scaledqt, x1=Don_Day_Scaled[49], y1=Don_scaledqm)
segments(x0=Don_Day_Scaled[104], y0=Don_scaledqt, x1=Don_Day_Scaled[104], y1=Don_scaledqm)
segments(x0=Don_Day_Scaled[49], y0=Don_scaledqt, x1=Don_Day_Scaled[104], y1=Don_scaledqt)
segments(x0=Don_Day_Scaled[49], y0=-0.35, x1=Don_Day_Scaled[49], y1=Don_scaledqt, lty=3)
segments(x0=Don_Day_Scaled[104], y0=-0.35, x1=Don_Day_Scaled[104], y1=Don_scaledqt, lty=3)
arrows(x0=Don_Day_Scaled[49], y0=-0.35, x1=Don_Day_Scaled[104], y1=-0.35, length = 0.05, code=3)

#Graph labels#
text(x=0, y=-0.9 ,label="t", cex=1, pos=4, font=3)
text(x=0.04, y=-0.9 ,label="[day]", cex=1, pos=4)
text(x=0, y=0.9 ,label="Q", cex=1, pos=4, font=3)
text(x=0, y=0.8 ,label=expression("[m"^3), cex=1)
text(x=0, y=0.79 ,label="/s]", cex=1, pos=4)
text(x=0.80, y=-0.01 ,label="t", cex=1, pos=1, font=3)
text(x=0.9, y=-0.01 ,label="[day]", cex=1, pos=1)
text(x=-0.95, y=-0.01 ,label=bquote(bar(h)), cex=1, pos=3, font=3)
text(x=-0.88, y=-0.01 ,label="[m]", cex=1, pos=3)
text(x=-(Don_scaledht), y=-0.9 ,label="Ht", cex=1, pos=4, font=3)
text(x=-(Don_scaledhm), y=-0.9 ,label="Hm", cex=1, pos=4, font=3)
text(x=0.9, y=Don_scaledqm ,label="Qm", cex=1, pos=1, font=3)
text(x=0.9, y=Don_scaledqt ,label="Qt", cex=1, pos=1, font=3)
text(x=Don_Day_Scaled[76], y=-0.35, label="Tf", cex=1, pos=1, font=3)
text(x=Don_Day_Scaled[260], y=-0.55, label="FEV ≈ 3.00Mm^3", cex=1)
text(x=Don_Day_Scaled[285], y=-0.62, label="Tf = 13.50hrs", cex=1)
text(x=Don_Day_Scaled[280], y=-0.69, label="Ht = 2.90m", cex=1)
text(x=Don_Day_Scaled[270], y=-0.76, label="Hm = 4.06m", cex=1)
text(x=Don_Day_Scaled[260], y=-0.83, label="Qt = 164.1", cex=1)
text(x=Don_Day_Scaled[347], y=-0.82 ,label=expression("m"^3), cex=1)
text(x=Don_Day_Scaled[372], y=-0.82 ,label="/s", cex=1)
text(x=Don_Day_Scaled[250], y=-0.9, label="Qm = 225.9", cex=1)
text(x=Don_Day_Scaled[350], y=-0.89 ,label=expression("m"^3), cex=1)
text(x=Don_Day_Scaled[375], y=-0.89,label="/s", cex=1)
axis(side=1, cex.axis=1, pos=0, at=c(-1.0782,-0.8375842,-0.5969682,-0.3563523,-0.1157363, 0.0,1/4,2/4,3/4,4/4),labels=c("6","5","4","3","2","25","26","27","28","29"))
axis(side=2,cex.axis=1, pos=0, at=c(-(4/4),-(3/4),-(2/4),-(1/4),0.1638662,0.3638982,0.5639302,0.7639622,0.9639942),labels=c("29","28","27","26","50","100","150","200","250"), las=1)

####
(1 - min(Don_Height)) / (max(Don_Height) - min(Don_Height))
#130,164
Don_Polygon <- read.csv(file.choose(), header = T)
View(Don_Polygon)
Don_Polygon_Day <- Don_Polygon$Day
Don_Polygon_Flow <- Don_Polygon$Flow
Don_Polygon_Day_Scaled <- scalevector(Don_Polygon_Day)
Don_Polygon_Flow_Scaled <- scalevector(Don_Polygon_Flow)
#49,104
#50,105

