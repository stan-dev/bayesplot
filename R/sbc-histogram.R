sbc_histogram <- function(ranks, n_draws, n_reps, offset=5) {
  
  if (!is.null(dim(ranks)) | is.list(ranks) ) {
    stop("ranks must be a numeric vector!")
  }
  
  CI <- qbinom(c(0.005,0.5,0.995), size=n_reps,prob  =  1/(n_draws+1))
  mean <- n_reps/(n_draws+1)
  
  ggplot(data.frame(rank=(ranks)),aes(x=rank)) + 
    geom_segment(aes(x=0,y=mean,xend=n_draws,yend=mean),colour="grey25") +
    geom_polygon(data=data.frame(x=c(-offset,0,-offset,n_draws + offset ,100,n_draws + offset,-offset),
                                 y=c(CI[1],CI[2],CI[3],CI[3],CI[2],CI[1],CI[1])),
                 aes(x=x,y=y),fill="grey45",color="grey25",alpha=0.5) +
    geom_bar(fill="#A25050",colour="black")
  
}
