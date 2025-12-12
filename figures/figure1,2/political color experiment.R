#Power balance: combination of president, House, and Senate
#2-year interval
#left-wing
balance2year=read.csv("balance2year.csv",stringsAsFactors=F)
start_year=1989
end_year=2025
c=-0.5
year=seq(start_year, end_year, by=2)
years=length(year)
beta_1=ifelse(
  balance2year$president==1, -0.07, -0.07
)
beta_2=-0.014
pc=numeric(years)
#Trump effect during tenure
IT_t=ifelse((year>=2017 & year<=2020)|(year==2025), 1,0)

for(t in 1:years){
  pc[t]=c+beta_1[t]*balance2year$balance[t]+beta_2*IT_t[t]
}

pc_df=data.frame(
  year=year,
  political_color_left=pc
)

setEPS()
postscript("power-D.eps", width=10, height=7)
plot(
  pc_df$year,
  pc_df$political_color_left,
  type="o", col="black", lwd=2, pch=16,
  xlab="", ylab="",
  ylim=c(-0.7,-0.3),
  main=""
)
mtext("Year", side = 1, line = 3, cex = 2)
dev.off()

#right-wing
start_year=1989
end_year=2025
c=0.5
year=seq(start_year, end_year, by=2)
years=length(year)
beta_1=ifelse(
  balance2year$president==1, 0.04, 0.04
)
beta_2=0.008
pc=numeric(years)
IT_t=ifelse((year>=2017 & year<= 2020)|(year==2025), 1,0)

for(t in 1:years){
  pc[t]=c+beta_1[t]*balance2year$balance[t]+beta_2*IT_t[t]
}

pc_df$political_color_right=pc
write.csv(pc_df, "political color_national.csv", row.names = F)

setEPS()
postscript("power-R.eps", width=10, height=7)
plot(
  pc_df$year,
  pc_df$political_color_right,
  type="o", col="black", lwd=2, pch=16,
  xlab="", ylab="",
  ylim=c(0.3,0.7),
  main=""
)
mtext("Year", side = 1, line = 3, cex = 2)
dev.off()

#difference between left and right
pc_df$diff=pc_df$political_color_right-pc_df$political_color_left
setEPS()
postscript("distance.eps", width=10, height=7)
plot(
  pc_df$year,
  pc_df$diff,
  type="o", col="black", lwd=2, pch=16,
  xlab="", ylab="",
  ylim=c(0.8,1.3),
  main=""
)
mtext("Year", side = 1, line = 3, cex = 1)
dev.off()