library(tidyverse)
require(lcmm)

lefnw <- read_rds("../HILDA/data/quarterly events long.rds") |> 
  filter(event == "lefnw") |> 
  select(year, xwaveid, qtr, val)

swb <- read_rds("../HILDA/data/SWB.rds") |> 
  select(year = wave, xwaveid, losat, age, sex) |> 
  filter(xwaveid %in% lefnw$xwaveid)

df <- left_join(swb, lefnw, by = join_by(xwaveid, year)) |> 
  mutate(
    xwaveid = as.integer(xwaveid),
    year = parse_number(year),
    age = c(scale(age)),
    val = replace_na(val, 0),
    qtr = recode(qtr, "1" = 0, "2" = 4/12, "3" = 7/12, "4" = 10/12,
                 .missing = 0)) |> 
  arrange(xwaveid, year) |> 
  group_by(xwaveid) |> 
  mutate(cval = cumsum(val)) |> 
  filter(cval < 2) |> 
  mutate(
    event_year = year[which(val == 1)],
    year_pst = year - event_year,
    time = year_pst + qtr
  ) |> 
  filter(!is.na(losat)) |> 
  filter(between(time, -3, 3)) |> 
  filter(min(time) == -3) |> 
  # mutate(losat = losat - mean(losat, na.rm=T)) |>
  select(xwaveid, time, losat, age, sex) |> 
  ungroup()

fdf <- df |> 
  # filter(xwaveid %in% sample(unique(df$xwaveid), 100)) |>
  mutate(losat = jitter(losat)) |> 
  as.data.frame()

write_rds(fdf, "../data/lefnw_losat_mf.rds")

m1 <- hlme(losat ~ time + I(time^2) + I(time^3) + sex,
           random =~ time + I(time^2) + I(time^3),
           subject = 'xwaveid', 
           ng = 1,
           data = fdf)

write_rds(m1, "../results/losat_lefnw_1group.rds")

# Estimation considering 2 classes : 
m2 <- gridsearch(
  hlme(losat ~ time + I(time^2) + I(time^3) + sex,
       random =~ time + I(time^2) + I(time^3),
       subject = 'xwaveid', 
       data = fdf, 
       ng = 2, 
       nproc = 2,
       mixture =~time + I(time^2) + I(time^3)),
  rep=10, maxiter=30, minit=m1
) 

write_rds(m2, "../results/losat_lefnw_2group.rds")
postprob(m2)           
plot(m2, which="fit", var.time="time", marg=FALSE, shades = TRUE)  


# Estimation considering 3 classes : 
m3 <- gridsearch(
  hlme(losat ~ time + I(time^2) + I(time^3) + sex,
       random =~ time + I(time^2) + I(time^3),
       subject = 'xwaveid', 
       data = fdf, 
       ng = 3, 
       nproc = 3,
       mixture =~time + I(time^2) + I(time^3)),
  rep=10, maxiter=30, minit=m1
)

write_rds(m3, "../results/losat_lefnw_3group.rds")
postprob(m3)           
plot(m3, which="fit", var.time="time", marg=FALSE, shades = TRUE)  

# Estimation considering 4 classes : 
m4 <- gridsearch(
  hlme(losat ~ time + I(time^2) + I(time^3) + sex,
       random =~ time + I(time^2) + I(time^3),
       subject = 'xwaveid', 
       data = fdf, 
       ng = 4, 
       nproc = 4, 
       mixture =~time + I(time^2) + I(time^3)),
  rep=100, maxiter=30, minit=m1
)

write_rds(m4, "../results/losat_lefnw_4group.rds")
postprob(m4)           
plot(m4, which="fit", var.time="time", marg=FALSE, shades = TRUE)  

# Estimation considering 5 classes : 
m5 <- gridsearch(
  hlme(losat ~ time + I(time^2) + I(time^3) + sex,
       random =~ time + I(time^2) + I(time^3),
       subject = 'xwaveid', 
       data = fdf, 
       ng = 5, 
       nproc = 5, 
       mixture =~time + I(time^2) + I(time^3)),
  rep=100, maxiter=30, minit=m1
)

postprob(m5)

summaryplot(m1, m2, m3, m4, m5, which = c("AIC", "BIC", "entropy"))
