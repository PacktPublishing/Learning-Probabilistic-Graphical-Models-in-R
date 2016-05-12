bayes <- function(prior, likelihood, data)
{
	posterior <- matrix(0, nrow=length(data), ncol=length(prior))
	dimnames(posterior) <- list(data, names(prior))

	initial_prior = prior
	for(i in 1:length(data))
	{
		posterior[i, ] <- 
			prior*likelihood[ , data[i]]/
			sum(prior * likelihood[ , data[i]])

		prior <- posterior[i , ]
	}

	return(rbind(initial_prior,posterior))
}
