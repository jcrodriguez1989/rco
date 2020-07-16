# Ignoring case where their is an expression instead of a symbol in place of index
v <- NULL
for(i in seq_len(5)) {
	v[i] <- i
	v[i+5] <- i*i 
}

#We also have to take care of the case when the same vector name is being repeated at differnet places. For instance,
z <- NULL
v <- NULL
for(i in seq_len(5)) {
	v[i] <- i
}
z <- v
v <- c()
for(i in seq_len(10)) {
	v[i] <- i*i
}
