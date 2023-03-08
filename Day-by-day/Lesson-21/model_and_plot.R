model_and_plot <- function(tilde, data, label="") {
  newdat <- data[ , all.vars(tilde)]
  names(newdat) <- c("y", "one", "two")[1:length(newdat)]
  if (length(all.vars(tilde)) == 1) newtilde <- y ~ 1
  if (length(all.vars(tilde)) == 2) newtilde <- y ~ one
  if (length(all.vars(tilde)) == 3) {
    newtilde <- tilde
    newtilde[[2]] <- as.name("y")
    newtilde[[3]][[2]] <- as.name("one")
    newtilde[[3]][[3]] <- as.name("two")
  }
  mod <- lm(newtilde, data=newdat)
  model_plot(mod, data_alpha=.05, data=newdat, nlevels=21) + labs(title=label)
}
