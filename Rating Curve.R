#Import Data#
Aire_Rating <- read.csv(file.choose(), header = T)

#Rename Vectors#
Aire_A <- Aire_Rating$Aire.A
Aire_B <- Aire_Rating$Aire.B
Aire_C <- Aire_Rating$Aire.C
Lowlims <- c(0, 0.685, 1.917)
Uplims <- c(0.685, 1.917, 6)

#Create Values#
Rating_Curve <- seq(0.16, 6, 0.001)

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






