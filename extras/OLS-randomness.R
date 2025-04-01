
ggplot(incdata,
       aes(x = educ,
           y = realrinc/1000)) +
  geom_jitter(alpha = .2) +
  geom_smooth(method = "lm",
              se = FALSE)

m <- lm(realrinc/1000 ~ educ,
        data = incdata)
mytidy(m)

corrmod <- lm(scale(realrinc) ~ scale(educ),
              data = incdata)
mytidy(corrmod)

incdata <- incdata |> 
  mutate(zinc = (realrinc - mean(realrinc)) / sd(realrinc),
         zedu = (educ - mean(educ)) / sd(educ))

corrmod2 <- lm(zinc ~ zedu,
               data = incdata)
summary(corrmod2)