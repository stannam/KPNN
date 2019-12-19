library(shinydashboard)
library(shiny)
library(fresh)
library(networkD3)
library(tidyverse)
library(igraph)
library(DT)
if(Sys.info()[['sysname']] == 'Linux'){
  Sys.getlocale(category = "LC_ALL")
  Sys.setlocale("LC_ALL","korean")
  Sys.getlocale(category = "LC_ALL")
  Sys.getenv()
	dir.create('~/.fonts') 
	file.copy("data/NanumGothic.ttf", '~/.fonts', overwrite = T)
	system('fc-cache -f ~/.fonts')
}

total<-read_csv("data/words.csv")
net <- readRDS("data/net.RDS")
subnet <- decompose(net, mode = c("weak", "strong"), max.comps = NA, min.vertices = 8)

shinyServer(function(input, output) {
	
	#tab2
	output$outputtable <- DT::renderDataTable({
	  outputTable <- total
	  
		#strata separation
		if (input$strata != "total") {
		  outputTable <- outputTable %>%
		    filter(stratum == input$strata)
		    }

	  #neighbourhood density option
	  outputTable  <- outputTable %>%
	    filter(density >= input$density[1], density <= input$density[2])

		#word count separating
		if (input$slider <= nrow(outputTable)) {
		  outputTable <- outputTable[1:input$slider,]
		}
		
		DT::datatable(outputTable, options = list(pageLength = 25))
	})
	
	output$totalsave <- downloadHandler(
		filename = function() {"Korean lexicon.csv"},
		content = function(file) {
		  file.copy("data/words.csv", file)
		}
	)
	
	output$save <- downloadHandler(
		filename = function() {"Korean lexicon.csv"},
		content = function(file) {
			write.csv(outputTable, file, row.names = FALSE)
		}
	)

	#tab3
	output$samplePlot <- renderForceNetwork({
		i <- as.numeric(input$subnet)
		g <- subnet[[i]]
		g$layout <- layout_with_kk
		nd3_g <- igraph_to_networkD3(g)
		nd3_g$nodes$name <- V(g)$entry
		
		network <- forceNetwork(Links = nd3_g$links,
		             Nodes = nd3_g$nodes,
		             NodeID = "name",
		             Group = "name",
		             opacityNoHover = 2,
		             zoom = T,
		             charge = -50,
		             bounded = T,
		             fontFamily = "Nanum Gothic",
		             fontSize = 13)
		saveNetwork(network, "net.html")
		network
	})
	output$savesubgraph <- downloadHandler(
	  filename = function() {
	    paste("Subgraph","html",sep=".")
	    },
	  content = function(file) {
	    file.copy("net.html", file)
	    },
	  contentType = "application/html"
	)
	
	#tab4
	output$graphSave <- downloadHandler(
		filename = function() {"Graph.html"},
		content = function(file) {
		  file.copy("net.html", file)
		}
	)
	
	output$tableSave <- downloadHandler(
		filename = function() {"neighbour_list.csv"},
		content = function(file) {
			write.csv(neighborList, file, row.names = T)
		}
	)
	
	output$neighbourtable <- DT::renderDataTable({
		if(length(which(total$orthography==input$text))==0){
			neighborList<-"try other word!"
		}
		else {
			nlist <- as_ids(neighbors(net, which(total$orthography==input$text)))
			nlist <- as.numeric(nlist) 								# list of neighbors as vertex id
			nlist <- c(which(total$orthography==input$text),nlist)	# the target word must come at the top of the table so add the id for target word at the beginning
			neighborList<-data.frame()
			for (i in nlist) {neighborList <- rbind(neighborList, total[i,1:5])}
			neighborList %>%
			  select(-serial) %>%
			  DT::datatable(options = list(pageLength = 25))
		}
		
	}, width = 600)
	
	output$networkPlot <- renderForceNetwork({
		if(length(which(total$orthography==input$text))==0){
			vertexList<-16714
		}
		else {
			vertexList <- as_ids(neighbors(net, which(total$orthography==input$text)))
			vertexList <- as.numeric(vertexList)				# list of neighbors as vertex id
			vertexList <- c(which(total$orthography==input$text),vertexList)	# the target word must come at the top of the table so add the id for target word at the beginning
			vertexList <- sort(vertexList, decreasing = F)		# labels should be match with the vertex id.
		}
		label_list <<- total$orthography[vertexList]
		g <<- induced_subgraph(net,vertexList)
		nd3_g <- igraph_to_networkD3(g)
		nd3_g$nodes$name <- V(g)$entry
		network <- forceNetwork(Links = nd3_g$links,
		                        Nodes = nd3_g$nodes,
		                        NodeID = "name",
		                        Group = "name",
		                        opacityNoHover = 2,
		                        zoom = T,
		                        charge = -50,
		                        bounded = T,
		                        fontFamily = "Nanum Gothic",
		                        fontSize = 13)
		saveNetwork(network, "net.html")
		network
		
	})

})