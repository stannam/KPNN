library(shinydashboard)
library(shiny)
library(fresh)
library(DT)
frontText <- scan(file="data/documentation.txt", sep="`", what="char", quote=NULL, encoding="UTF-8",quiet=T)
frontText <- paste(frontText, collapse="")
dashboardPage(
	dashboardHeader(title = "Korean Phonological Neighbourhood Network",		# Header title
			titleWidth = 450 ),   

	dashboardSidebar(
		sidebarMenu(
			menuItem("Front page (첫 페이지)", tabName = "front", icon = icon("th")), 						# 1st tab: front
			menuItem("Stratified Korean Lexicon", tabName = "wordlist", icon = icon("list-alt")), 		# 2nd tab: wordlist
			menuItem("Sample subcomponents", tabName = "sample", icon = icon("screenshot", lib = "glyphicon")), 	# 3rd tab: sample
			menuItem("Phonological neighbours", tabName = "pnlist", icon = icon("screenshot", lib = "glyphicon")) 			# 4th tab: pnlist
		)
	),
  
  
	dashboardBody(
		tabItems(
		
		# 1st tab: front
			tabItem(tabName = "front",
				fluidPage(
					helpText(HTML(frontText))
				)
			),
			
		# 2nd tab: wordlist
			tabItem(tabName = "wordlist",
				fluidPage(
					titlePanel("Stratified Korean Lexicon"),
					sidebarLayout(
						sidebarPanel(
							radioButtons("strata", label = "Select Strata (층위선택)",
								choices = list("Total (전체)" = "total", "Native (순우리말)" = "native", "Sino-Korean (한자어)" = "sino", "Foreign (외래어)" = "foreign"),selected = "total"),
							sliderInput("slider", label = "# of words (어휘 개수)", min = 0, max = 26759, value = 50),
							sliderInput("density",label = "Density / # of phonological neighbours (음운이웃 개수)", min=0, max=93, value = c(0,93)),
							br(),
							helpText(HTML("Select your options above, and the list of words that satisfy them will appear on the right-hand side. <br>
								The default sorting of the words are in frequency (descending order). Specifying \" # of words\" will show # words that are frequent.<br>
								You can change how the words are sorted, by choosing an option under \" sort by\".<br>
								You may also want to specify the number of phonological neighbours.<br>
								상단의 옵션을 선택하시면 조건을 만족하는 단어의 목록이 우측에 표시됩니다. 단어는 기본적으로 사용빈도 (고빈도 우선) 순으로 표시됩니다. 따라서 \" # of words\"값 설정하시면 고빈도 어휘가 나옵니다. <br>
								음운이웃개수를 지정할 수도 있습니다. <br><br>
								Two buttons below export <b>as a csv file</b> the total lexicon or the list of words shown on the right-hand side.<br>
								하단의 버튼 2개는 각각, 어휘목록 전체, 혹은 우측에 나열된 어휘목록을 <b>csv 파일로</b> 저장합니다.<br><br>
								<b>Please cite this data as:</b><br> Nam, Sunghyun. (2018). <i>Korean phonological neighbourhood network</i>. [Computer program]. Available from https://namsling.shinyapps.io/kpnn/.<br><br>
								<b>다음과 같이 인용해주세요</b><br> 남성현. (2018). <i>Korean phonological neighbourhood network</i>. [컴퓨터 프로그램]. URL: https://namsling.shinyapps.io/kpnn/.<br><br>
							")),
							br(),
							downloadButton("totalsave", "Whole lexicon (전체)"),
							downloadButton("save", "RHS table (우측 표)")
						),
						mainPanel(
							DT::dataTableOutput("outputtable")
						)
					)
				)
			),

		# 3rd tab: sample subcomponents
			tabItem(tabName = "sample",
				fluidPage(
					titlePanel("Sample subcomponents"),
					sidebarLayout(
						sidebarPanel(
							selectInput("subnet", label = "Select a component # (하위망 # 선택)",
								choices = list(
									"Subgraph #1 (N=17063)" = 1,
									"Subgraph #2 (N=18)" = 6,
									"Subgraph #3 (N=15)" = 5,
									"Subgraph #4 (N=9)" = 4,
									"Subgraph #5 (N=9)" = 7,
									"Subgraph #6 (N=8)" = 2,
									"Subgraph #7 (N=8)" = 3,
									"Subgraph #8 (N=8)" =8
								), selected = 6
							),
							helpText(HTML("There are 570 components in the Korean phonological neighbourhood network, most of which have two words only. The largest one (giant component / giant cluster in the literature) consists of 17,063 lexemes and the second largest one has 18. You can plot a graph for one of the eight largest components here. Select one above but note that the largest component takes eternity to plot.
								<br><br>
								한국어 음운망에는 총 570개의 하위망이 있으나 대부분은 단어가 2개밖에 없습니다. 가장 큰 하위망은 17,063개의 어휘로 구성되며, 그 다음으로 큰 것은 단어가 18개입니다. 여기서는 한국어의 하위망 중 크기가 큰 8개를 네트워크 그래프로 시각화할 수 있습니다. 상단의 선택메뉴에서 하나를 선택해주세요. 단, 가장 큰 하위망을 그리는 데에는 시간이 엄청 오래 걸립니다.
								<br><br><br>
								See<a href=\"http://dcollection.cau.ac.kr//jsp/common/DcLoOrgPer.jsp?sItemId=000000198203\" target=\"_blank\"> Nam (2017) </a>(in English) or<a href=\"https://www.kci.go.kr/kciportal/co/download/popup/poDownload.kci?storFileBean.orteFileId=KCI_FI002342049\" target=\"_blank\"> Nam & Kim (2018a) </a>(in Korean) for general properties of the Korean PNN, including clusteredness.
								<br><br>
								한국어 음운망의 전반적인 특성, 특히 하위망의 분포 등에 대해서는,<a href=\"http://dcollection.cau.ac.kr//jsp/common/DcLoOrgPer.jsp?sItemId=000000198203\" target=\"_blank\"> Nam (2017) </a>(영어) 혹은<a href=\"https://www.kci.go.kr/kciportal/co/download/popup/poDownload.kci?storFileBean.orteFileId=KCI_FI002342049\" target=\"_blank\"> Nam & Kim (2018a) </a>(한국어) 를 참고해주세요.<br><br>
								<b>Please cite this data as:</b><br> Nam, Sunghyun. (2018). <i>Korean phonological neighbourhood network</i>. [Computer program]. Available from https://namsling.shinyapps.io/kpnn/.<br><br>
								<b>다음과 같이 인용해주세요</b><br> 남성현. (2018). <i>Korean phonological neighbourhood network</i>. [컴퓨터 프로그램]. URL: https://namsling.shinyapps.io/kpnn/.<br><br>")),
							downloadButton("savesubgraph", "Get the graph on your right as a .html file (with network interaction!)")
						),
						mainPanel(
						  networkD3::forceNetworkOutput("samplePlot")			#샘플로 받아와야 하는것은 samplePlot
						)
					)
				)
			),
			
      # 4th tab: pnlist
			tabItem(tabName = "pnlist",
				fluidPage(
					titlePanel("Phonological Neighbourhood"),
					sidebarLayout(
						sidebarPanel(
							textInput("text", label = HTML("Enter a word <i>in Hangul </i>to get a list of phonological neighbours.<br> 음운이웃을 찾을 단어를 입력하세요."), value = "병"),
							helpText(HTML("<br><br>Here, you can see a part of the Korean lexicon visually and as a table. <br>Try your target word in the textbox above.<br>이곳에서는 한국어 렉시콘의 일부를 시각적으로 그리고 어휘목록표로 볼 수 있습니다. <br> 한국어 단어를 상단 글상자에 입력해주세요.")),
							helpText(HTML("<br><br>The network graph (nodes and edges that connect them) consist of your target word and its phonological neighbours. <br>
								우측 상단에는 '네트워크 그래프'가 있습니다. 네트워크 그래프는 절점(node)과 절점을 잇는 연결선(edge)으로 구성됩니다. 지금은, 입력된 단어가 가운데 있고 그것의 음운이웃들이 연결된 그래프가 시각적으로 표현되어 있습니다. <br><br>
								The graph is followed by a list of words that are 'phonological neighbours' to the target word.<br>
								우측 하단에는 입력된 단어와 그것의 음운이웃들이 표 형식으로 표현되어 있습니다. <br><br>
								You can also download the graph and table on the right-hand side, with the two buttons below.<br>
								우측에 표현된 정보는 각각 다운로드받을 수 있습니다. <br><br>
								<b>Please cite this data as:</b><br> Nam, Sunghyun. (2018). <i>Korean phonological neighbourhood network</i>. [Computer program]. Available from https://namsling.shinyapps.io/kpnn/.<br><br>
								<b>다음과 같이 인용해주세요</b><br> 남성현. (2018). <i>Korean phonological neighbourhood network</i>. [컴퓨터 프로그램]. URL: https://namsling.shinyapps.io/kpnn/.<br><br>")),
							br(),
							downloadButton("graphSave", "Network graph as a .html file　(망 그래프)"),
							br(),
							downloadButton("tableSave", "Neighbour list as a .csv file　 (음운이웃 목록)")
						),
						mainPanel(
							HTML("<b><h3>A network graph with the target word (at the centre) and its neighbours.</b></h3>"),
							networkD3::forceNetworkOutput("networkPlot"),
							HTML("<br><h3><b>A list of phonological neighbours.</b></h3><br>"),
							DT::dataTableOutput("neighbourtable")
						)
					)
				)
			)
		)
	)
)
  


