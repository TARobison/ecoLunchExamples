##############################################################
### How to making pretty, readable plots in base R
### Illustrated with examples
### Michael Stemkovski
### 3/1/2019 Ecolunch
##############################################################

### auxilary stuff

plot.pch<-function(){
  oldPar<-par()
  par(font=2, mar=c(0.5,0,0,0))
  y=rev(c(rep(1,6),rep(2,5), rep(3,5), rep(4,5), rep(5,5)))
  x=c(rep(1:5,5),6)
  plot(x, y, pch = 0:25, cex=1.5, ylim=c(1,5.5), xlim=c(1,6.5), 
       axes=FALSE, xlab="", ylab="", bg="blue")
  text(x, y, labels=0:25, pos=3)
  par(mar=oldPar$mar,font=oldPar$font )
}

layout(matrix(1,nrow=1,ncol=1))

#######################
### simulating data ###
#######################

time <- 1:1000
scatter_data <- time + rnorm(1000,1,1)*time*0.4 + rnorm(1000,1,50)
make.groups <- function(x){
  add_noise <- x + rnorm(1,10,50)
  if(add_noise < 200) return(1)
  if(add_noise > 200 & add_noise < 600) return(2)
  else return(3)
}
scatter_data_groups <- sapply(time, make.groups)

years <- rep(2010:2018, each=300)
year_data <- years + rnorm(length(years),1,200) + 1:length(years)*0.6
years <- years[-c(400:600)]
year_data <- year_data[-c(400:600)]

env <- runif(1000,1,100)
alive_or_not <- sapply(env, function(x) if((x + rnorm(1,10,25)) > 50) return(1) else return(0))

data(iris) # loading in iris trait data

# calculating means and standard deviations
setosa_means <- colMeans(iris[which(iris$Species == "setosa"),-5])
setosa_sd <- apply(iris[which(iris$Species == "setosa"),-5],2,sd)

versicolor_means <- colMeans(iris[which(iris$Species == "versicolor"),-5])
versicolor_sd <- apply(iris[which(iris$Species == "versicolor"),-5],2,sd)

virginica_means <- colMeans(iris[which(iris$Species == "virginica"),-5])
virginica_sd <- apply(iris[which(iris$Species == "virginica"),-5],2,sd)

#########################
### basic beautifying ###
#########################

plot(scatter_data ~ time) # it's so bad

# pch
plot.pch()
plot(scatter_data ~ time, pch=20)

# colors, paletts, transparency
plot(scatter_data ~ time, pch=20, col=rgb(0.3,0.6,0)) # setting colors
plot(scatter_data ~ time, pch=20, col=rgb(0.3,0.6,0,0.4)) # transparency

library(RColorBrewer)
old_mar <- par("mar")
par(mar=c(3,4,2,2))
display.brewer.all()
par(mar = old_mar)

color_pallet <- brewer.pal(n=3, name="Set2")
group_colors <- sapply(scatter_data_groups, function(x) color_pallet[x])
plot(scatter_data ~ time, pch=20, col=group_colors) # group colors

group_colors_transparent <- adjustcolor(group_colors, alpha.f = 0.5)
plot(scatter_data ~ time, pch=20, col=group_colors_transparent) # transparent group colors

plot(scatter_data ~ time, pch=20, col=group_colors_transparent,
     xlab = "Environmental gradient",
     ylab = "Trait value") # finishing up

legend("topleft", inset = 0.05,
       legend = c("Species A", "Species B", "Species C"),
       col=color_pallet,
       pch = 20, cex=0.9, pt.cex = 1,
       bty = "n")

##############
### jitter ###
##############

plot(year_data ~ years) # eww

plot(year_data ~ years, pch=20, col=rgb(0.2,0,0.3,0.2),
     xlab = "Years", ylab = "Trait value") 

plot(year_data ~ jitter(years,0.9),
     pch=20, col=rgb(0.2,0,0.3,0.2),
     xlab = "Years", ylab = "Trait value") # adding jitter


plot(alive_or_not ~ env) # go away

plot(jitter(alive_or_not, 0.5) ~ env, pch=20, col= rgb(0.1,0.5,0.1,0.4),
     xlab = "Environmental gradient", ylab = "Survival") # mmmm... nice

surv_model <- glm(alive_or_not ~ env, family = binomial(link = "logit"))
lines(predict(surv_model,data.frame(env = 1:100), type="response")) # this line is meh

lines(predict(surv_model,data.frame(env = 1:100), type="response"),
      lwd=3, col = rgb(0.8,0,0)) # this line is better




###################################
### setting up plotting windows ###
###################################

color_pallet <- brewer.pal(n=3, name="Dark2") #colors

# setting plot ranges
y_range <- c(0,8)
x_range <- c(0.5,4.5)

#empty window
par(mfrow=c(1,1),
    mar=c(4.1, 4.1, 2.1, 2.1))
plot(1,type = "n", xaxt = "n",
     xlim = x_range, ylim = y_range,
     xlab = "", ylab = "Trait values")

# x axis
axis(1, at = c(1:4), labels = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width"),
     tick=F)

poly_width <- 0.4
poly_color <- "gray"

#drawing polygons and mean lines
for(i in 1:4){
  polygon(x = c(i-poly_width,i+poly_width,i+poly_width,i-poly_width),
          y = c(y_range[1]-3,y_range[1]-3,y_range[2]+3,y_range[2]+3),
          col = poly_color,
          border = F)
  
  lines(x = c(i-poly_width, i+poly_width),
        y = rep(mean(c(setosa_means[i],versicolor_means[i],virginica_means[i])), 2),
        lty=2)
}

# plotting means
points(x = c(1:4)-0.2, y = setosa_means, pch = 20, col = color_pallet[1], cex=2)
points(x = c(1:4), y = versicolor_means, pch = 20, col = color_pallet[2], cex=2)
points(x = c(1:4)+0.2, y = virginica_means, pch = 20, col = color_pallet[3], cex=2)

# plotting standard deviations
arrows(c(1:4)-0.2,
       setosa_means-(setosa_sd/2),
       c(1:4)-0.2,
       setosa_means+(setosa_sd/2),
       lwd=1.5, angle=90, code=3, length=0.08, col = color_pallet[1])

arrows(c(1:4),
       versicolor_means-(versicolor_sd/2),
       c(1:4),
       versicolor_means+(versicolor_sd/2),
       lwd=1.5, angle=90, code=3, length=0.08, col = color_pallet[2])

arrows(c(1:4)+0.2,
       virginica_means-(virginica_sd/2),
       c(1:4)+0.2,
       virginica_means+(virginica_sd/2),
       lwd=1.5, angle=90, code=3, length=0.08, col = color_pallet[3])

#legend
legend("topright", inset = 0.05, legend = c("Iris setosa","Iris versicolor","Iris virginica"),
       col=color_pallet, pch = c(20,20,20), cex=1, pt.cex = 2)

box()


#################################
### multiple plotting windows ###
#################################

layout(matrix(1:4,nrow=2,ncol=2))

plot(year_data ~ years) # eww

plot(year_data ~ years, pch=20, col=rgb(0.2,0,0.3,0.2),
     xlab = "Years", ylab = "Trait value") 

plot(year_data ~ jitter(years,0.9),
     pch=20, col=rgb(0.2,0,0.3,0.2),
     xlab = "Years", ylab = "Trait value") # adding jitter

plot(year_data ~ jitter(years,0.9),
     pch=20, col=rgb(0.2,0,0.3,0.2),
     xlab = "Years", ylab = "Trait value",
     xaxt = "n", yaxt="n",
     bty="n") #removing problematic axis text

axis(1, at=2010:2018, labels=2010:2018, las=2)
axis(2, at=seq(1500,4000, by=500), labels=seq(1500,4000, by=500), las=2)
box()


###################
### historgrams ###
###################

pop1 <- rnorm(1000,70,15)
pop2 <- rnorm(1000,35,10)

hist(pop1) # wow... lets not

break_n = 20

hist(pop1, breaks = break_n,
     xlim=c(0,120), ylim=c(0,200),
     col=rgb(1, 0, 0, 0.5),
     border = F,
     xaxt='n',xlab="Trait value",main="")

hist(pop2, breaks = break_n, add=TRUE,
     col=rgb(0, 0, 1, 0.5),
     border = F)

axis(1, at=seq(0,120,30),labels=seq(0,120,30))

legend("topright", inset=.05,c("Species A","Species B"),
       fill=c(rgb(1, 0, 0, 0.5),rgb(0, 0, 1, 0.5)), cex=1,
       border=F)


#########################
### other neat tricks ###
#########################

### save as SVG

# svg("filename.svg",width=11,height=7)
# your figure
# dev.off()

svg("/home/michael/Desktop/ecolunch_figure.svg",width=11,height=7)

layout(matrix(1,nrow=1,ncol=1))

#empty window
par(mfrow=c(1,1),
    mar=c(4.1, 4.1, 2.1, 2.1))
plot(1,type = "n", xaxt = "n",
     xlim = x_range, ylim = y_range,
     xlab = "", ylab = "Trait values")

# x axis
axis(1, at = c(1:4), labels = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width"),
     tick=F)

poly_width <- 0.4
poly_color <- "gray"

#drawing polygons and mean lines
for(i in 1:4){
  polygon(x = c(i-poly_width,i+poly_width,i+poly_width,i-poly_width),
          y = c(y_range[1]-3,y_range[1]-3,y_range[2]+3,y_range[2]+3),
          col = poly_color,
          border = F)
  
  lines(x = c(i-poly_width, i+poly_width),
        y = rep(mean(c(setosa_means[i],versicolor_means[i],virginica_means[i])), 2),
        lty=2)
}

# plotting means
points(x = c(1:4)-0.2, y = setosa_means, pch = 20, col = color_pallet[1], cex=2)
points(x = c(1:4), y = versicolor_means, pch = 20, col = color_pallet[2], cex=2)
points(x = c(1:4)+0.2, y = virginica_means, pch = 20, col = color_pallet[3], cex=2)

# plotting standard deviations
arrows(c(1:4)-0.2,
       setosa_means-(setosa_sd/2),
       c(1:4)-0.2,
       setosa_means+(setosa_sd/2),
       lwd=1.5, angle=90, code=3, length=0.08, col = color_pallet[1])

arrows(c(1:4),
       versicolor_means-(versicolor_sd/2),
       c(1:4),
       versicolor_means+(versicolor_sd/2),
       lwd=1.5, angle=90, code=3, length=0.08, col = color_pallet[2])

arrows(c(1:4)+0.2,
       virginica_means-(virginica_sd/2),
       c(1:4)+0.2,
       virginica_means+(virginica_sd/2),
       lwd=1.5, angle=90, code=3, length=0.08, col = color_pallet[3])

#legend
legend("topright", inset = 0.05, legend = c("Iris setosa","Iris versicolor","Iris virginica"),
       col=color_pallet, pch = c(20,20,20), cex=1, pt.cex = 2)

box()

dev.off()


### make a gif

# saveGIF({
#   ani.options(interval = 0.2, nmax = 100)
#      your loop
# }, movie.name = "filename.gif",ani.width = 600, ani.height = 400)


data(beavers)
head(beaver1)

beaver_day <- beaver1[which(beaver1$day == 346),]
x_range <- range(beaver_day$time)
y_range <- range(beaver_day$temp)

temp_ramp <- colorRampPalette(c(rgb(0,0.5,0.9), rgb(0.9,0.1,0)))
temp_ramp <- temp_ramp(dim(beaver_day)[1])

library(animation)
saveGIF({
  ani.options(interval = 0.2, nmax = 100)
  count <- 1
  for(i in beaver_day$time){
    #i <- beaver_day$time[3]
    beaver_sub <- beaver_day[which(beaver_day$time <= i),]
    plot(1,type = "n",
         xlim = x_range, ylim = y_range,
         xlab = "Time", ylab = "Beaver Temperature")
    points(temp~time ,data = beaver_sub, pch=20, cex=1.5, col="gray")
    if(dim(beaver_sub)[1] > 5){
      beaver_model <- lm(temp~time ,data = beaver_sub)
      abline(beaver_model, lwd=3, col="red")
    }
    this_time <- beaver_sub[which(beaver_sub$time == i),]
    where_in_ramp <- as.numeric((this_time["temp"] - y_range[1])/(y_range[2]-y_range[1]))
    points(temp~time ,data = this_time, pch=20, cex=3,
           col=temp_ramp[count])
    count <- count + 1
  }
}, movie.name = "/home/michael/Desktop/ecolunch_animation.gif",ani.width = 600, ani.height = 400)

