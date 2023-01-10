library(astsa)
### AMD Data ###
amddata <- read.csv("AMD.csv", header = T)
amd <- amddata$Close
amd <- ts(amd)
plot(amd, ylab = "AMD Close Price", main = "AMD Close Price from April 2020 to April 2021")
amd.diff1 <- diff(amd, diff = 1)
plot(amd.diff1, ylab = "AMD Close Price", main = "AMD Close Price from April 2020 to April 2021, d=1")
acf2(amd.diff1, main = "AMD Close Price")
fit = sarima(amd, 1, 1, 1, 0, 0, 0);fit
sarima.for(amd,n.ahead = 5, p= 1, d = 1, q = 1, P= 0, D = 0, Q = 0, plot = T)

amdper = spec.pgram(amd, taper = 0, log = "no")

### Intel Data ###
inteldata <- read.csv("INTC.csv", header =T)
intc <- inteldata$Close
intc <- ts(intc)
plot(intc, ylab = "INTC Close Price", main = "INTC Close Price from April 2020 to April 2021")

fin <- ts.union(amd, intc, dframe = F)
plot(fin)
finper <- spec.pgram(fin, taper = 0, log = "no", kernel("daniell", 3))
plot(finper, plot.type = "coh", main = "Squared Coherency")
