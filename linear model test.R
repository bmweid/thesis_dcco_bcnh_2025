mod <- lm(bcnh_growthindex ~ year, data=combined_dataset)

summary(mod)

nrow(na.omit(combined_dataset))
