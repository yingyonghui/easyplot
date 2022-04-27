#' To present a flip barplot
#' @param data dataframe
#' @param x x
#' @param y y
#' @importFrom ggplot2 ggplot geom_point scale_color_manual labs theme element_text element_rect element_line aes scale_color_gradientn geom_text geom_bar scale_fill_manual coord_flip scale_x_discrete scale_y_continuous
#' @return a flip barplot
#' @export
barFlip <- function(data,x,y){
	data[,x] <- as.character(data[,x])
	data[,x] <- factor(data[,x],levels=rev(data[,x]))

	num_p_sig <- length(which(data$adj.P.Val < 0.05 & data$t > 0))
	num_n_sig <- length(which(data$adj.P.Val < 0.05 & data$t < 0))
	num_non_sig <- length(which(data$adj.P.Val >= 0.05))

	data$color.bar <- c(rep("firebrick1",num_p_sig),rep(gray(0.5),num_non_sig),rep("steelblue",num_n_sig))
	data$color.txt <- c(rep("black",num_p_sig),rep(gray(0.5),num_non_sig),rep("black",num_n_sig))

	data$hjust <- ifelse(data$t>0,1,0)
	ax.limits <- ceiling(max(abs(data$y)))

	plot <- ggplot(data,aes(x=x,y=y)) + 
		geom_text(aes(y=0,label=x,hjust=hjust),color=data$color.txt) + 
		geom_bar(aes(fill=x),stat="identity") + 
		scale_fill_manual(values=rev(data$color.bar)) +
		coord_flip() + 
		theme(legend.position='none')+
		theme(panel.background=element_rect(fill=NA))+
		theme(panel.border=element_rect(fill=NA,colour="black",size=1.2)) +
		labs(x="",y="") + 
		scale_x_discrete(breaks=NULL) + 
		scale_y_continuous(limits=c(-ax.limits,ax.limits))
	return(plot)
}
